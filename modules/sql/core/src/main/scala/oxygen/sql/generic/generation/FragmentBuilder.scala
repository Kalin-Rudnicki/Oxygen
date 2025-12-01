package oxygen.sql.generic.generation

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import scala.annotation.tailrec
import scala.quoted.*

final case class FragmentBuilder(inputs: List[InputPart])(using Quotes) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      misc calculated
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val nonConstInputParams: List[VariableReference.NonConstInput] =
    inputs.map(_.mapQueryRef).flatMap {
      case p: VariableReference.FromInput         => p.some
      case p: VariableReference.OptionalFromInput => p.some
      case _: VariableReference.FromConstInput    => None
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
          case (None, VariableReference.FromInput(_)) =>
            InputRepr.NonConst(TermTransformer.Id, false)
          case (Some(inputTpeTupGen), VariableReference.FromInput(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            InputRepr.NonConst(TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)), false)
          case (None, VariableReference.OptionalFromInput(_)) =>
            InputRepr.NonConst(TermTransformer.Id, true)
          case (Some(inputTpeTupGen), VariableReference.OptionalFromInput(param)) =>
            val idx: Int = symIdxMap.getOrElse(param.sym, report.errorAndAbort("sym not found?", param.tree.pos))
            InputRepr.NonConst(TermTransformer.FromProductGenericField(inputTpeTupGen, inputTpeTupGen.fields(idx)), true)
          case (_, VariableReference.FromConstInput(_, term, _)) =>
            InputRepr.Const(term)
        }

      i.mapQueryRef.param.sym -> inputRepr
    }.toMap

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      queryExprToFragment
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object queryExprToFragment {

    def apply(queryExpr: QueryExpr, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      (queryExpr, parentContext) match {
        case (queryExpr: QueryExpr.ConstValue, Some(parentContext))                 => queryExprToFragment.constOrInput(queryExpr, parentContext)
        case (queryExpr: QueryExpr.InputVariableReferenceLike, Some(parentContext)) => queryExprToFragment.constOrInput(queryExpr, parentContext)
        case (queryExpr: QueryExpr.QueryVariableReferenceLike, _)                   => queryExprToFragment.query(queryExpr)
        case (queryExpr: QueryExpr.Binary, _)                                       => queryExprToFragment.binary(queryExpr)
        case (queryExpr: QueryExpr.BuiltIn, _)                                      => queryExprToFragment.builtIn(queryExpr)
        case (queryExpr: QueryExpr.Composite, _)                                    => queryExprToFragment.composite(queryExpr, parentContext)
        case (queryExpr: QueryExpr.ConstValue, None)                                => ParseResult.error(queryExpr.fullTerm, "No known RowRepr to compare with")
        case (queryExpr: QueryExpr.InputVariableReferenceLike, None)                => ParseResult.error(queryExpr.fullTerm, "No known RowRepr to compare with")
      }

    def apply(queryExpr: QueryExpr, parentContext: TypeclassExpr.RowRepr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      apply(queryExpr, parentContext.some)

    def constOrInput(
        queryExpr: QueryExpr.ConstValue | QueryExpr.InputVariableReferenceLike,
        parentContext: TypeclassExpr.RowRepr,
    )(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      queryExpr match
        case queryExpr: QueryExpr.ConstValue                 => queryExprToFragment.const(queryExpr, parentContext)
        case queryExpr: QueryExpr.InputVariableReferenceLike => queryExprToFragment.input(queryExpr, parentContext)

    def constOrInput(
        queryExpr: QueryExpr.ConstValue | QueryExpr.InputVariableReferenceLike,
        parentContext: QueryExpr.QueryVariableReferenceLike,
    )(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      queryExprToFragment.constOrInput(queryExpr, parentContext.rowRepr)

    def const(
        queryExpr: QueryExpr.ConstValue,
        parentContext: TypeclassExpr.RowRepr,
    )(using GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      ParseResult.success(GeneratedInputEncoder.const(parentContext.inputEncoder, queryExpr.constTerm).usingRowRepr(parentContext))

    private def nonConstInput(
        input: QueryExpr.InputVariableReferenceLike,
        parentContext: TypeclassExpr.RowRepr,
        transform: TermTransformer.SimpleValid,
        handleOpt: Option[GenerationContext.HandleOptionalInputs.Allowed],
    )(using GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
      val frag: GeneratedFragment =
        handleOpt match {
          case Some(handleOpt) =>
            type T
            given Type[T] = input.outTpe.asTypeOf
            val optTpe: TypeRepr = TypeRepr.of[Option[T]]
            handleOpt.make(optTpe, GeneratedInputEncoder.nonConst(parentContext.inputEncoder.optional, optTpe).usingRowRepr(parentContext))
          case None =>
            GeneratedInputEncoder.nonConst(parentContext.inputEncoder, input.outTpe).usingRowRepr(parentContext)
        }

      ParseResult.success {
        transform match {
          case TermTransformer.Id                   => frag
          case transform: TermTransformer.Transform => frag.contramap(transform)
        }
      }
    }

    def input(
        input: QueryExpr.InputVariableReferenceLike,
        parentContext: TypeclassExpr.RowRepr,
    )(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      for {
        inputRepr <- inputSymToInputRepr.get(input.queryRef.param.sym) match {
          case Some(value) => ParseResult.success(value)
          case None        => ParseResult.error(input.rootIdent, "Not able to find in symMap?")
        }

        frag: GeneratedFragment <- inputRepr match {
          case InputRepr.NonConst(inputTransformer, isOptional) =>
            for {
              transform <-
                (inputTransformer >>> input) match {
                  case TermTransformer.Die                    => ParseResult.error(input.fullTerm, "Expected to not call input transform?")
                  case transform: TermTransformer.SimpleValid => ParseResult.success(transform)
                }
              handleOpt <- Option.when(isOptional)(GenerationContext.optionalInputHandling(input.fullTerm)).sequence
              frag <- nonConstInput(input, parentContext, transform, handleOpt)
            } yield frag
          case InputRepr.Const(const) =>
            ParseResult.success(GeneratedInputEncoder.const(parentContext.inputEncoder, const).usingRowRepr(parentContext))
        }
      } yield frag

    def query(queryExpr: QueryExpr.QueryVariableReferenceLike)(using GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      ParseResult.success {
        GeneratedFragment.both(
          GenerationContext.get.query.`ref.a, ref.b, ref.c`(queryExpr.rowRepr.columns, Expr(queryExpr.queryRef.sqlString)),
          GeneratedInputEncoder.empty,
        )
      }

    def binary(queryExpr: QueryExpr.Binary)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      queryExpr match {
        // standard and/or
        case QueryExpr.BinaryAndOr(_, lhs, op, rhs) =>
          for {
            lhsFrag <- queryExprToFragment(lhs, None)
            rhsFrag <- queryExprToFragment(rhs, None)
          } yield GeneratedFragment.of(lhsFrag.wrapInParensIf(lhs.isAndOr), op.sqlPadded, rhsFrag.wrapInParensIf(rhs.isAndOr))
        // binary comparison between 2 query-vars
        case QueryExpr.BinaryComp(_, lhs: QueryExpr.QueryVariableReferenceLike, op, rhs: QueryExpr.QueryVariableReferenceLike) =>
          for {
            lhsFrag <- queryExprToFragment.query(lhs)
            rhsFrag <- queryExprToFragment.query(rhs)
          } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
        // binary comparison between query-var and (OPTIONAL) input var
        case QueryExpr.BinaryComp(_, lhs: QueryExpr.QueryVariableReferenceLike, op, rhs: QueryExpr.InputVariableReferenceLike) if rhs.isOptional =>
          for {
            lhsFrag <- queryExprToFragment.query(lhs)
            handleOpt = GenerationContext.HandleOptionalInputs.or { frag => GeneratedFragment.of(lhsFrag, op.sqlPadded, frag) }
            fullFrag <- GenerationContext.updated(handleOptionalInputs = handleOpt) { queryExprToFragment(rhs, lhs.rowRepr) }
          } yield fullFrag
        // binary comparison between query-var and (OPTIONAL) input var
        case QueryExpr.BinaryComp(_, lhs: QueryExpr.InputVariableReferenceLike, op, rhs: QueryExpr.QueryVariableReferenceLike) if lhs.isOptional =>
          for {
            rhsFrag <- queryExprToFragment.query(rhs)
            handleOpt = GenerationContext.HandleOptionalInputs.or { frag => GeneratedFragment.of(frag, op.sqlPadded, rhsFrag) }
            fullFrag <- GenerationContext.updated(handleOptionalInputs = handleOpt) { queryExprToFragment(lhs, rhs.rowRepr) }
          } yield fullFrag
        // binary comparison between query-var and non-query-var
        case QueryExpr.BinaryComp(_, lhs: QueryExpr.QueryVariableReferenceLike, op, rhs) =>
          for {
            lhsFrag <- queryExprToFragment.query(lhs)
            rhsFrag <- queryExprToFragment(rhs, lhs.rowRepr)
          } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
        // binary comparison between query-var and non-query-var
        case QueryExpr.BinaryComp(_, lhs, op, rhs: QueryExpr.QueryVariableReferenceLike) =>
          for {
            lhsFrag <- queryExprToFragment(lhs, rhs.rowRepr)
            rhsFrag <- queryExprToFragment.query(rhs)
          } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
        // general comparison of 2 non-query-vars
        case QueryExpr.BinaryComp(_, lhs, op, rhs) =>
          for {
            lhsFrag <- queryExprToFragment(lhs, None)
            rhsFrag <- queryExprToFragment(rhs, None)
          } yield GeneratedFragment.of(lhsFrag, op.sqlPadded, rhsFrag)
      }

    def builtIn(queryExpr: QueryExpr.BuiltIn)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      queryExpr match
        case QueryExpr.Static(_, out, _)      => ParseResult.Success(GeneratedFragment.sql(out))
        case QueryExpr.CountWithArg(_, inner) => queryExprToFragment(inner, None).map { frag => GeneratedFragment.of("COUNT(", frag, ")") }

    def composite(queryExpr: QueryExpr.Composite, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
      queryExpr match {
        case it: QueryExpr.InstantiateTable =>
          it.cleanedArgs.traverse(queryExprToFragment(_, None)).map { argFrags => GeneratedFragment.flatten(argFrags.intersperse(GeneratedFragment.sql(", "))) }
        case QueryExpr.StringConcat(_, args) =>
          args.traverse(queryExprToFragment(_, TypeclassExpr.RowRepr.string)).map { argFrags =>
            GeneratedFragment.flatten(argFrags.surround(GeneratedFragment.sql("CONCAT("), GeneratedFragment.sql(", "), GeneratedFragment.sql(")")))
          }
        case QueryExpr.OptionApply(_, inner) =>
          queryExprToFragment(inner, parentContext)
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      queryExprToInputEncoder
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object queryExprToInputEncoder {

    def constOrInput(
        queryExpr: QueryExpr.ConstValue | QueryExpr.InputVariableReferenceLike,
        parentContext: TypeclassExpr.RowRepr,
    )(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedInputEncoder] =
      queryExpr match
        case queryExpr: QueryExpr.ConstValue                 => queryExprToInputEncoder.const(queryExpr, parentContext)
        case queryExpr: QueryExpr.InputVariableReferenceLike => queryExprToInputEncoder.input(queryExpr, parentContext)

    def const(queryExpr: QueryExpr.ConstValue, parentContext: TypeclassExpr.RowRepr)(using GenerationContext, Quotes): ParseResult[GeneratedInputEncoder] =
      queryExprToFragment.const(queryExpr, parentContext).map(_.generatedInputEncoder)

    def input(input: QueryExpr.InputVariableReferenceLike, parentContext: TypeclassExpr.RowRepr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedInputEncoder] =
      queryExprToFragment.input(input, parentContext).map(_.generatedInputEncoder)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  // partToFragment
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : wrap in object

  def insert(i: InsertPart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "INSERT INTO ",
        i.tableRepr.tableRef,
        s" AS ",
        i.mapQueryRef match {
          case Some(queryRef) => Expr(queryRef.sqlString)
          case None           => i.tableRepr.tableNameFirstChar
        },
      ),
    )

  def select(s: SelectPart.FromTable)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "\n    FROM ",
        s.tableRepr.tableRef,
        s" ${s.mapQueryRef.sqlString}",
      ),
    )

  def select(s: SelectPart.FromSubQuery)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for sq <- s.subQuery.makeFragment
    yield GeneratedFragment.of(
      "\n    FROM (\n",
      GeneratedFragment.indented(sq, "        "),
      s"\n    ) AS ${s.subQueryTableName}",
    )

  def update(u: UpdatePart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "UPDATE ",
        u.tableRepr.tableRef,
        " ",
        u.queryRef match {
          case Some(queryRef) => Expr(queryRef.sqlString)
          case None           => u.tableRepr.tableNameFirstChar
        },
      ),
    )

  def delete(d: DeletePart)(using Quotes): ParseResult[GeneratedFragment] =
    ParseResult.success(
      GeneratedFragment.of(
        "DELETE FROM ",
        d.tableRepr.tableRef,
        s" ${d.mapQueryRef.sqlString}",
      ),
    )

  def ret(r: ReturningPart, idt: String)(using ParseContext, GenerationContext, Quotes): ParseResult[Option[GeneratedFragment]] =
    Option.when(r.returningExprs.nonEmpty)(r.returningExprs).traverse {
      _.traverse { elem =>
        elem.as match {
          case Some(as) =>
            elem.expr match {
              case QueryExpr.QueryVariableReferenceLike.ReferencedVariable(_, queryRef) =>
                ParseResult.success(GeneratedFragment.sql(s"${queryRef.sqlString} AS $as"))
              case queryExpr =>
                ParseResult.error(queryExpr.fullTerm, "unable to resolve reference for sub-query return")
            }
          case None =>
            elem.expr match {
              case query: QueryExpr.QueryVariableReferenceLike => GenerationContext.updated(query = GenerationContext.Parens.Never) { queryExprToFragment.query(query) }
              case queryExpr                                   => queryExprToFragment(queryExpr, None)
            }
        }
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
      onFrag <- queryExprToFragment(j.filterExpr, None)
    } yield GeneratedFragment.of(
      // join
      j.joinType match
        case JoinPart.JoinType.Inner     => "\n    JOIN "
        case JoinPart.JoinType.LeftOuter => "\n    LEFT JOIN ",
      j.tableRepr.tableRef,
      s" ${j.mapQueryRef.sqlString}",
      // on
      " ON ",
      onFrag,
    )

  def where(w: WherePart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      whereFrag <- queryExprToFragment(w.filterExpr, None)
    } yield GeneratedFragment.of(
      "\n    WHERE ",
      whereFrag,
    )

  private def orderByPart(l: OrderByPart.OrderByExpr)(using GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      queryExprFrag <- queryExprToFragment.query(l.queryExpr)
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
      limitFrag <- GenerationContext.updated(handleOptionalInputs = GenerationContext.HandleOptionalInputs.id) {
        queryExprToFragment(l.limitQueryExpr, TypeclassExpr.RowRepr.int)
      }
    } yield GeneratedFragment.of(
      "\n    LIMIT ",
      limitFrag,
    )

  def offset(o: OffsetPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      offsetFrag <- GenerationContext.updated(handleOptionalInputs = GenerationContext.HandleOptionalInputs.id) {
        queryExprToFragment(o.offsetQueryExpr, TypeclassExpr.RowRepr.int)
      }
    } yield GeneratedFragment.of(
      "\n    OFFSET ",
      offsetFrag,
    )

  def values(ins: InsertPart, i: IntoPart)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] =
    for {
      valuesFrag <- GenerationContext.updated(input = GenerationContext.Parens.Always) { queryExprToFragment(i.queryExpr, ins.rowRepr) }
    } yield GeneratedFragment.of(
      "\n    VALUES ",
      valuesFrag,
    )

  private def setPart(s: SetPart.SetExpr)(using ParseContext, GenerationContext, Quotes): ParseResult[GeneratedFragment] = {
    val setTargetRowRepr: TypeclassExpr.RowRepr = s.fieldToSetExpr.rowRepr
    val setTargetColumnNames: Expr[ArraySeq[String]] = setTargetRowRepr.columns.exprSeqNames

    @tailrec
    def qMarksAndInputEncoderResult(
        queryExpr: QueryExpr,
        rowRepr: TypeclassExpr.RowRepr,
    ): ParseResult[(Expr[ArraySeq[String]], GeneratedInputEncoder)] =
      queryExpr match { // TODO (KR) : this ignores optionality in inputs
        case input: QueryExpr.ConstValue                 => queryExprToInputEncoder.constOrInput(input, rowRepr).map((rowRepr.columns.exprSeqQMark, _))
        case input: QueryExpr.InputVariableReferenceLike => queryExprToInputEncoder.constOrInput(input, rowRepr).map((rowRepr.columns.exprSeqQMark, _))
        case queryExpr: QueryExpr.OptionApply            => qMarksAndInputEncoderResult(queryExpr.inner, rowRepr)
        case query: QueryExpr.QueryVariableReferenceLike =>
          ParseResult.success(
            (
              query.rowRepr.columns.exprSeqRefNames(query.queryRef.sqlString),
              GeneratedInputEncoder.empty,
            ),
          )
        case _ =>
          queryExprToFragment(queryExpr, rowRepr).map { frag =>
            ('{ ArraySeq(${ frag.generatedSql.buildExpr.sql }) }, frag.generatedInputEncoder)
          }
      }

    qMarksAndInputEncoderResult(s.setValueExpr, setTargetRowRepr).map { case (rhsParts, enc) =>
      val zipped: Expr[ArraySeq[String]] =
        '{
          val lhs: ArraySeq[String] = $setTargetColumnNames
          val rhs: ArraySeq[String] = $rhsParts
          if lhs.length != rhs.length then throw new RuntimeException("defect: non-equal set lengths")
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
