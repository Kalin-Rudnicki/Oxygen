package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.{*, given}
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.schema.*
import scala.quoted.*

final case class FragmentBuilder(inputs: List[InputPart])(using Quotes) {

  private val nonConstInputParams: List[QueryParam.InputParam] =
    inputs.map(_.mapQueryRef).flatMap {
      case p: QueryParam.InputParam => p.some
      case _: QueryParam.ConstInput => None
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
        (inputTpeTupGen, i.mapQueryRef) match {
          case (None, QueryParam.InputParam(_)) =>
            TermTransformer.Id.asRight
          case (Some(inputTpeTupGen), QueryParam.InputParam(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)).asRight
          case (_, QueryParam.ConstInput(_, term, _)) =>
            term.asLeft
        }

      i.mapQueryRef.param.sym -> either
    }.toMap

  def convert(queryExpr: QueryExpr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    queryExpr match {
      case input: QueryExpr.UnaryInput            => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case input: QueryExpr.ConstValue            => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case query: QueryExpr.UnaryQuery            => convertQuery(query)
      case QueryExpr.BinaryAndOr(_, lhs, op, rhs) =>
        for {
          lhsFrag <- convert(lhs)
          rhsFrag <- convert(rhs)
        } yield GeneratedFragment.of(lhsFrag.wrapInParensIf(lhs.isAndOr), op.sqlPadded, rhsFrag.wrapInParensIf(rhs.isAndOr))
      case QueryExpr.BinaryComp.QueryQuery(_, lhsQuery, op, rhsQuery) =>
        for {
          lhsFrag <- convertQuery(lhsQuery)
          rhsFrag <- convertQuery(rhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
      case QueryExpr.BinaryComp.QueryInput(_, lhsQuery, op, rhsInput) =>
        for {
          lhsFrag <- convertQuery(lhsQuery)
          rhsFrag <- convertConstOrInput(rhsInput, lhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
      case QueryExpr.BinaryComp.InputQuery(_, lhsInput, op, rhsQuery) =>
        for {
          lhsFrag <- convertConstOrInput(lhsInput, rhsQuery)
          rhsFrag <- convertQuery(rhsQuery)
        } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
    }

  private def convertQuery(query: QueryExpr.UnaryQuery)(using Quotes, GenerationContext): ParseResult[GeneratedFragment] =
    ParseResult.success {
      GeneratedFragment.both(
        GenerationContext.get.query.`ref.a, ref.b, ref.c`(query.rowRepr.columns, Expr(query.rootIdent.name.camelToSnake)),
        GeneratedInputEncoder.empty,
      )
    }

  private def convertConstOrInput(input: QueryExpr.ConstOrUnaryInput, query: QueryExpr.UnaryQuery)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    convertConstOrInput(input, query.rowRepr)

  private def constOrInputToEncoder(input: QueryExpr.ConstOrUnaryInput, rowRepr: TypeclassExpr.RowRepr)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
    input match {
      case QueryExpr.ConstValue(_, term) => ParseResult.Success(GeneratedInputEncoder.const(rowRepr.constEncoder(term)))
      case input: QueryExpr.UnaryInput   => inputToEncoder(input, rowRepr)
    }

  private def inputToEncoder(input: QueryExpr.UnaryInput, rowRepr: TypeclassExpr.RowRepr)(using ParseContext, Quotes): ParseResult[GeneratedInputEncoder] =
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
              ParseResult.success(GeneratedInputEncoder.nonConst('{ ${ rowRepr.expr }.encoder }, input.inTpe))
            case transform: TermTransformer.Transform =>
              type A
              type B
              given Type[A] = transform.inTpe.asTypeOf
              given Type[B] = transform.outTpe.asTypeOf
              val typedQueryRowRepr: Expr[RowRepr[B]] = rowRepr.expr.asExprOf[RowRepr[B]]
              ParseResult.success(GeneratedInputEncoder.nonConst('{ $typedQueryRowRepr.encoder.contramap[A](${ transform.convertExprF[A, B] }) }, transform.inTpe))
          }
        case Left(const) =>
          type A
          given Type[A] = const.tpe.widen.asTypeOf
          val typedQueryRowRepr: Expr[RowRepr[A]] = rowRepr.expr.asExprOf[RowRepr[A]]
          val expr: Expr[A] = const.asExprOf[A]

          ParseResult.success(GeneratedInputEncoder.const('{ InputEncoder.Const($typedQueryRowRepr.encoder, $expr) }))
      }
    } yield enc

  private def convertConstOrInput(input: QueryExpr.ConstOrUnaryInput, rowRepr: TypeclassExpr.RowRepr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      enc: GeneratedInputEncoder <- constOrInputToEncoder(input, rowRepr)
      qMarkStrings = GenerationContext.get.input.`?, ?, ?`(rowRepr.columns)
      frag = GeneratedFragment.both(qMarkStrings, enc)
    } yield frag

  // =====|  |=====

  def insert(i: InsertPart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "INSERT INTO ",
        i.tableRepr.tableRef,
        s" AS ",
        i.mapQueryRef match {
          case Some(queryRef) => Expr(queryRef.param.name.camelToSnake)
          case None           => i.tableRepr.tableNameFirstChar
        },
      ),
    )

  def select(s: SelectPart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "\n    FROM ",
        s.tableRepr.tableRef,
        s" ${s.mapQueryRef.param.name.camelToSnake}",
      ),
    )

  def update(u: UpdatePart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "UPDATE ",
        u.tableRepr.tableRef,
        " ",
        u.queryRef match {
          case Some(queryRef) => Expr(queryRef.param.name.camelToSnake)
          case None           => u.tableRepr.tableNameFirstChar
        },
      ),
    )

  def delete(d: DeletePart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "DELETE FROM ",
        d.tableRepr.tableRef,
        s" ${d.mapQueryRef.param.name.camelToSnake}",
      ),
    )

  def ret(r: ReturningPart, idt: String)(using ParseContext, GenerationContext, Quotes): ParseResult[Option[GeneratedFragment]] =
    Option.when(r.returningExprs.nonEmpty)(r.returningExprs).traverse {
      _.traverse {
        case query: QueryExpr.UnaryQuery => GenerationContext.updated(query = GenerationContext.Parens.Never) { convertQuery(query) }
        case queryExpr                   => convert(queryExpr)
      }
        .map { res => GeneratedFragment.flatten(res.intersperse(GeneratedFragment.sql(s",\n$idt"))) }
    }

  def requiredRet(r: ReturningPart, idt: String)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    ret(r, idt).flatMap {
      case Some(value) => ParseResult.success(value)
      case None        => ParseResult.error(r.fullTree, "expected non-empty return")
    }

  def join(j: JoinPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      onFrag <- convert(j.filterExpr)
    } yield GeneratedFragment.of(
      // join
      j.joinType match
        case JoinPart.JoinType.Inner     => "\n    JOIN "
        case JoinPart.JoinType.LeftOuter => "\n    LEFT JOIN ",
      j.tableRepr.tableRef,
      s" ${j.mapQueryRef.param.name.camelToSnake}",
      // on
      " ON ",
      onFrag,
    )

  def where(w: WherePart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      whereFrag <- convert(w.filterExpr)
    } yield GeneratedFragment.of(
      "\n    WHERE ",
      whereFrag,
    )

  private def orderByPart(l: OrderByPart.OrderByExpr)(using GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      queryExprFrag <- convertQuery(l.queryExpr)
    } yield GeneratedFragment.of(
      queryExprFrag,
      " ",
      l.ord.toString.toUpperCase,
    )

  def orderBy(l: OrderByPart)(using GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      partFrags <- l.orderByExprs.traverse(orderByPart)
      withCommas = partFrags.toList.intersperse(GeneratedFragment.sql(", "))
    } yield GeneratedFragment.of(
      "\n    ORDER BY ",
      GeneratedFragment.flatten(withCommas),
    )

  def limit(l: LimitPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      limitFrag <- convertConstOrInput(l.limitQueryExpr, TypeclassExpr.RowRepr { '{ RowRepr.int } })
    } yield GeneratedFragment.of(
      "\n    LIMIT ",
      limitFrag,
    )

  def offset(o: OffsetPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      offsetFrag <- convertConstOrInput(o.offsetQueryExpr, TypeclassExpr.RowRepr { '{ RowRepr.int } })
    } yield GeneratedFragment.of(
      "\n    OFFSET ",
      offsetFrag,
    )

  def values(ins: InsertPart, i: IntoPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      valuesFrag <- GenerationContext.updated(input = GenerationContext.Parens.Always) { convertConstOrInput(i.queryExpr, ins.rowRepr) }
    } yield GeneratedFragment.of(
      "\n    VALUES ",
      valuesFrag,
    )

  def setPart(s: SetPart.SetExpr)(using ParseContext, Quotes): ParseResult[GeneratedFragment] = {
    val rowRepr: TypeclassExpr.RowRepr = s.fieldToSetExpr.rowRepr
    val lhsParts: Expr[ArraySeq[String]] = rowRepr.columns.exprSeqNames

    val rhsParts: ParseResult[(Expr[ArraySeq[String]], GeneratedInputEncoder)] =
      s.setValueExpr match {
        case input: QueryExpr.ConstOrUnaryInput =>
          constOrInputToEncoder(input, rowRepr).map((rowRepr.columns.exprSeqQMark, _))
        case query: QueryExpr.UnaryQuery =>
          ParseResult.success(
            (
              query.rowRepr.columns.exprSeqRefNames(query.queryRef.param.name.camelToSnake),
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

  def set(s: SetPart)(using ParseContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      partsFrag <- s.setExprs.toList.traverse(setPart).map { res => GeneratedFragment.flatten(res.intersperse(GeneratedFragment.sql(",\n        "))) }
    } yield GeneratedFragment.of(
      "\n    SET ",
      partsFrag,
    )

}
