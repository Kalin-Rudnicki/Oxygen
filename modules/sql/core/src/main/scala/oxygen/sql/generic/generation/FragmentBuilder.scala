package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.{*, given}
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.generic.parsing.part.*
import oxygen.sql.schema.*
import scala.quoted.*

final case class FragmentBuilder(inputs: List[Input])(using Quotes) {

  private val nonConstInputParams: List[QueryReference.InputParam] =
    inputs.map(_.queryRef).flatMap {
      case p: QueryReference.InputParam => p.some
      case _: QueryReference.ConstInput => None
    }

  private val symIdxMap: Map[Symbol, Int] =
    nonConstInputParams.map(_.param.sym).zipWithIndex.toMap

  val tmp: (TypeRepr, Option[K0.ProductGeneric[?]]) =
    nonConstInputParams match {
      case Nil      => (TypeRepr.of[Any], None)
      case i :: Nil => (i.param.tpe, None)
      case is       =>
        val gen = K0.ProductGeneric.ofTuple(is.map(_.param.tpe))
        (gen.typeRepr, gen.some)
    }
  private val (_, inputTpeTupGen) = tmp

  private val symMap: Map[Symbol, Either[Term, TermTransformer.Root]] =
    inputs.map { i =>
      val either: Either[Term, TermTransformer.Root] =
        (inputTpeTupGen, i.queryRef) match {
          case (None, QueryReference.InputParam(_)) =>
            TermTransformer.Id.asRight
          case (Some(inputTpeTupGen), QueryReference.InputParam(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)).asRight
          case (_, QueryReference.ConstInput(_, term, _)) =>
            term.asLeft
        }

      i.queryRef.param.sym -> either
    }.toMap

  def convert(queryExpr: QueryExpr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    queryExpr match {
      case input: QueryExpr.InputLike       => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case query: QueryExpr.QueryLike       => convertQuery(query)
      case QueryExpr.AndOr(_, lhs, op, rhs) =>
        for {
          lhsFrag <- convert(lhs)
          rhsFrag <- convert(rhs)
        } yield GeneratedFragment.of(lhsFrag.wrapInParensIf(lhs.isAndOr), op.sqlPadded, rhsFrag.wrapInParensIf(rhs.isAndOr))
      case QueryExpr.Comp.QueryQuery(_, lhsQuery, op, rhsQuery) =>
        for {
          lhsFrag <- convertQuery(lhsQuery)
          rhsFrag <- convertQuery(rhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
      case QueryExpr.Comp.QueryInput(_, lhsQuery, op, rhsInput) =>
        for {
          lhsFrag <- convertQuery(lhsQuery)
          rhsFrag <- convertInput(rhsInput, lhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
      case QueryExpr.Comp.InputQuery(_, lhsInput, op, rhsQuery) =>
        for {
          lhsFrag <- convertInput(lhsInput, rhsQuery)
          rhsFrag <- convertQuery(rhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
    }

  private def convertQuery(query: QueryExpr.QueryLike)(using Quotes, GenerationContext): ParseResult[GeneratedFragment] =
    ParseResult.success {
      GeneratedFragment.both(
        GenerationContext.get.query.`ref.a, ref.b, ref.c`('{ ${ query.rowRepr }.columns }, Expr(query.rootIdent.name.camelToSnake)),
        GeneratedInputEncoder.empty,
      )
    }

  private def convertInput(input: QueryExpr.InputLike, query: QueryExpr.QueryLike)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    convertInput(input, query.rowRepr)

  private def inputToEncoder(input: QueryExpr.InputLike, rowRepr: Expr[RowRepr[?]])(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    for {
      either <- symMap.get(input.queryRef.param.sym) match {
        case Some(value) => ParseResult.success(value)
        case None        => ParseResult.error(input.rootIdent, "Not able to find in symMap?")
      }
      enc: GeneratedInputEncoder <- either match {
        case Right(inputTransformer) =>
          (inputTransformer >>> input) match {
            case TermTransformer.Die => ParseResult.error(input.fullTerm, "Expected to not call input transform?")
            case TermTransformer.Id  =>
              ParseResult.success(GeneratedInputEncoder.nonConst('{ $rowRepr.encoder }, input.inTpe))
            case transform: TermTransformer.Transform =>
              type A
              type B
              given Type[A] = transform.inTpe.asTypeOf
              given Type[B] = transform.outTpe.asTypeOf
              val typedQueryRowRepr: Expr[RowRepr[B]] = rowRepr.asExprOf[RowRepr[B]]
              ParseResult.success(GeneratedInputEncoder.nonConst('{ $typedQueryRowRepr.encoder.contramap[A](${ transform.convertExprF[A, B] }) }, transform.inTpe))
          }
        case Left(const) =>
          type A
          given Type[A] = const.tpe.widen.asTypeOf
          val typedQueryRowRepr: Expr[RowRepr[A]] = rowRepr.asExprOf[RowRepr[A]]
          val expr: Expr[A] = const.asExprOf[A]

          ParseResult.success(GeneratedInputEncoder.const('{ InputEncoder.Const($typedQueryRowRepr.encoder, $expr) }))
      }
    } yield enc

  private def convertInput(input: QueryExpr.InputLike, rowRepr: Expr[RowRepr[?]])(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      enc: GeneratedInputEncoder <- inputToEncoder(input, rowRepr)
      qMarkStrings = GenerationContext.get.input.`?, ?, ?`('{ $rowRepr.columns })
      frag = GeneratedFragment.both(qMarkStrings, enc)
    } yield frag

  // =====|  |=====

  def insert(i: InsertQ)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "INSERT INTO ",
        '{ ${ i.tableRepr }.ref },
        s" AS ",
        i.queryRef match {
          case Some(queryRef) => Expr(queryRef.param.name.camelToSnake)
          case None           => '{ ${ i.tableRepr }.tableName.head.toString }
        },
      ),
    )

  def select(s: SelectQ)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "\n    FROM ",
        '{ ${ s.tableRepr }.ref },
        s" ${s.queryRef.param.name.camelToSnake}",
      ),
    )

  def update(u: UpdateQ)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "UPDATE ",
        '{ ${ u.tableRepr }.ref },
        " ",
        u.queryRef match {
          case Some(queryRef) => Expr(queryRef.param.name.camelToSnake)
          case None           => '{ ${ u.tableRepr }.tableName.head.toString }
        },
      ),
    )

  def delete(d: DeleteQ)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "DELETE FROM ",
        '{ ${ d.tableRepr }.ref },
        s" ${d.queryRef.param.name.camelToSnake}",
      ),
    )

  def ret(r: Returning, idt: String)(using ParseContext, GenerationContext, Quotes): ParseResult[Option[GeneratedFragment]] =
    Option.when(r.exprs.nonEmpty)(r.exprs).traverse {
      _.traverse {
        case query: QueryExpr.QueryLike => GenerationContext.updated(query = GenerationContext.Parens.Never) { convertQuery(query) }
        case queryExpr                  => convert(queryExpr)
      }
        .map { res => GeneratedFragment.flatten(res.intersperse(GeneratedFragment.sql(s",\n$idt"))) }
    }

  def requiredRet(r: Returning, idt: String)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    ret(r, idt).flatMap {
      case Some(value) => ParseResult.success(value)
      case None        => ParseResult.error(r.tree, "expected non-empty return")
    }

  def join(j: Join)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      onFrag <- convert(j.on)
    } yield GeneratedFragment.of(
      // join
      "\n    JOIN ",
      '{ ${ j.tableRepr }.ref },
      s" ${j.escapingParam.param.name.camelToSnake}",
      // on
      " ON ",
      onFrag,
    )

  def where(w: Where)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      whereFrag <- convert(w.where)
    } yield GeneratedFragment.of(
      "\n    WHERE ",
      whereFrag,
    )

  def values(ins: InsertQ, i: Into)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      valuesFrag <- GenerationContext.updated(input = GenerationContext.Parens.Always) { convertInput(i.queryExpr, ins.rowRepr) }
    } yield GeneratedFragment.of(
      "\n    VALUES ",
      valuesFrag,
    )

  def setPart(s: Set.SetPart)(using ParseContext, Quotes): ParseResult[GeneratedFragment] = {
    val rowRepr: Expr[RowRepr[?]] = s.lhsExpr.rowRepr
    val lhsParts: Expr[ArraySeq[String]] = '{ $rowRepr.columns.columns.map(_.name) }

    val rhsParts: ParseResult[(Expr[ArraySeq[String]], GeneratedInputEncoder)] =
      s.rhsExpr match {
        case input: QueryExpr.InputLike =>
          inputToEncoder(input, rowRepr).map(('{ $rowRepr.columns.columns.map(_ => "?") }, _))
        case query: QueryExpr.QueryLike =>
          ParseResult.success(
            (
              '{ ${ query.rowRepr }.columns.columns.map(c => s"${${ Expr(query.queryRef.param.name.camelToSnake) }}.${c.name}") },
              GeneratedInputEncoder.empty,
            ),
          )
      }

    rhsParts.map { case (rhsParts, enc) =>
      val zipped: Expr[ArraySeq[String]] =
        '{
          val lhs: ArraySeq[String] = $lhsParts
          val rhs: ArraySeq[String] = $rhsParts
          if (lhs.length != rhs.length)
            throw new RuntimeException("defect: non-equal set lengths")
          lhs.zip(rhs).map { case (a, b) => s"$a = $b" }
        }
      val joined: Expr[String] = '{ $zipped.mkString(",\n        ") }

      GeneratedFragment.both(GeneratedSql.single(joined), enc)
    }
  }

  def set(s: Set)(using ParseContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      partsFrag <- s.parts.toList.traverse(setPart).map { res => GeneratedFragment.flatten(res.intersperse(GeneratedFragment.sql(",\n        "))) }
    } yield GeneratedFragment.of(
      "\n    SET ",
      partsFrag,
    )

}
