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

  private val nonConstInputParams: List[QueryParam.NonConstInput] =
    inputs.map(_.mapQueryRef).flatMap {
      case p: QueryParam.InputParam         => p.some
      case p: QueryParam.OptionalInputParam => p.some
      case _: QueryParam.ConstInput         => None
    }

  private val symIdxMap: Map[Symbol, Int] =
    nonConstInputParams.map(_.param.sym).zipWithIndex.toMap

  private val tmp: (TypeRepr, Option[K0.ProductGeneric[?]]) =
    nonConstInputParams match {
      case Nil      => (TypeRepr.of[Any], None)
      case i :: Nil => (i.param.tpe, None)
      case is       =>
        val gen = K0.ProductGeneric.ofTuple(is.map(_.nonConstInputType))
        (gen.typeRepr, gen.some)
    }

  /**
    * when you have inputs like:
    * a <- input[A]
    * b <- input[B]
    * C <- input[C]
    *
    * you will end up with a query input type of (A, B, C)
    *
    * this tuple generic assists in selecting the individual A/B/C fields
    */
  private val (_, multiInputTupleGeneric) = tmp

  private enum InputRepr {
    case Const(term: Term)
    case NonConst(transformer: TermTransformer, isOptional: Boolean)
  }

  private val inputSymToInputRepr: Map[Symbol, InputRepr] =
    inputs.map { i =>
      val inputRepr: InputRepr =
        (multiInputTupleGeneric, i.mapQueryRef) match {
          case (None, QueryParam.InputParam(_)) =>
            InputRepr.NonConst(TermTransformer.Id, false)
          case (Some(inputTpeTupGen), QueryParam.InputParam(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            InputRepr.NonConst(TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)), false)
          case (None, QueryParam.OptionalInputParam(_)) =>
            InputRepr.NonConst(TermTransformer.Id, true)
          case (Some(inputTpeTupGen), QueryParam.OptionalInputParam(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            InputRepr.NonConst(TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)), true)
          case (_, QueryParam.ConstInput(_, term, _)) =>
            InputRepr.Const(term)
        }

      i.mapQueryRef.param.sym -> inputRepr
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
      case QueryExpr.BinaryComp.QueryInput(_, lhsQuery, op, rhsInput: QueryExpr.UnaryInput) if rhsInput.isOptional =>
        for {
          lhsFrag <- convertQuery(lhsQuery)
          rhsFrag <- GenerationContext.updated(allowOptionalInput = true) { convertConstOrInput(rhsInput, lhsQuery) }
        } yield GeneratedFragment.of("( ? OR ( ", lhsFrag, op.sqlPadded, rhsFrag, " ) )")
      case QueryExpr.BinaryComp.InputQuery(_, lhsInput: QueryExpr.UnaryInput, op, rhsQuery) if lhsInput.isOptional =>
        for {
          lhsFrag <- GenerationContext.updated(allowOptionalInput = true) { convertConstOrInput(lhsInput, rhsQuery) }
          rhsFrag <- convertQuery(rhsQuery)
        } yield GeneratedFragment.of("( ? OR ( ", lhsFrag, op.sqlPadded, rhsFrag, " ) )")
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

  private def constOrInputToEncoder(input: QueryExpr.ConstOrUnaryInput, rowRepr: TypeclassExpr.RowRepr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedInputEncoder] =
    input match {
      case input: QueryExpr.UnaryInput   => inputToEncoder(input, rowRepr)
      case QueryExpr.ConstValue(_, term) => ParseResult.Success(GeneratedInputEncoder.const(rowRepr.constEncoder(term)))
    }

  private def inputToEncoder(input: QueryExpr.UnaryInput, rowRepr: TypeclassExpr.RowRepr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedInputEncoder] =
    for {
      _ <-
        if (input.isOptional) GenerationContext.enforceOptionalInputAllowed(input.fullTerm)
        else ParseResult.Success(())

      inputRepr <- inputSymToInputRepr.get(input.queryRef.param.sym) match {
        case Some(value) => ParseResult.success(value)
        case None        => ParseResult.error(input.rootIdent, "Not able to find in symMap?")
      }
      enc: GeneratedInputEncoder <- inputRepr match {
        case InputRepr.NonConst(inputTransformer, false) =>
          (inputTransformer >>> input) match {
            case TermTransformer.Die => ParseResult.error(input.fullTerm, "Expected to not call input transform?")
            case TermTransformer.Id  =>
              ParseResult.success(GeneratedInputEncoder.nonConst('{ ${ rowRepr.expr }.encoder }, input.inTpe))
            case transform: TermTransformer.Transform =>
              type FullInputT
              type ThisParamT
              given Type[FullInputT] = transform.inTpe.asTypeOf
              given Type[ThisParamT] = transform.outTpe.asTypeOf

              val typedQueryRowRepr: Expr[RowRepr[ThisParamT]] = rowRepr.expr.asExprOf[RowRepr[ThisParamT]]
              ParseResult.success(GeneratedInputEncoder.nonConst('{ $typedQueryRowRepr.encoder.contramap[FullInputT](${ transform.convertExprF[FullInputT, ThisParamT] }) }, transform.inTpe))
          }
        case InputRepr.NonConst(inputTransformer, true) =>
          (inputTransformer >>> input) match {
            case TermTransformer.Die => ParseResult.error(input.fullTerm, "Expected to not call input transform?")
            case TermTransformer.Id  =>
              ParseResult.success(GeneratedInputEncoder.nonConst('{ ${ rowRepr.expr }.encoder.optional }, input.inTpe))
            case transform: TermTransformer.Transform =>
              type FullInputT
              type ThisParamT
              given Type[FullInputT] = transform.inTpe.asTypeOf
              given Type[ThisParamT] = transform.outTpe.asTypeOf

              val enc1: Expr[InputEncoder[ThisParamT]] = '{ InputEncoder.isNullEncoder }.asExprOf[InputEncoder[ThisParamT]]
              val enc2: Expr[InputEncoder[ThisParamT]] = '{ ${ rowRepr.expr }.encoder.optional }.asExprOf[InputEncoder[ThisParamT]]
              val combinedInc: Expr[InputEncoder[ThisParamT]] = '{ $enc1 ~ $enc2 }

              ParseResult.success(
                GeneratedInputEncoder.nonConst('{ $combinedInc.contramap[FullInputT](${ transform.convertExprF[FullInputT, ThisParamT] }) }, transform.inTpe),
              )
          }
        case InputRepr.Const(const) =>
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

  def setPart(s: SetPart.SetExpr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
    val rowRepr: TypeclassExpr.RowRepr = s.fieldToSetExpr.rowRepr
    val lhsParts: Expr[ArraySeq[String]] = rowRepr.columns.exprSeqNames

    val rhsParts: ParseResult[(Expr[ArraySeq[String]], GeneratedInputEncoder)] =
      s.setValueExpr match {
        case input: QueryExpr.UnaryInput if input.isOptional =>
          report.errorAndAbort("You can not use an optional input in a set expression")
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

  def set(s: SetPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      partsFrag <- s.setExprs.toList.traverse(setPart).map { res => GeneratedFragment.flatten(res.intersperse(GeneratedFragment.sql(",\n        "))) }
    } yield GeneratedFragment.of(
      "\n    SET ",
      partsFrag,
    )

}
