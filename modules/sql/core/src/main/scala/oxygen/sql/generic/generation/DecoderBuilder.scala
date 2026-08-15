package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

final class DecoderBuilder {

  object convert {

    def apply(queryExpr: QueryExpr, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
      (queryExpr, parentContext) match
        case (queryExpr: QueryExpr.ConstValue, Some(parentContext))                 => convert.const(queryExpr, parentContext)
        case (queryExpr: QueryExpr.InputVariableReferenceLike, Some(parentContext)) => convert.input(queryExpr, parentContext)
        case (queryExpr: QueryExpr.QueryVariableReferenceLike, _)                   => convert.query(queryExpr)
        case (_: QueryExpr.ArrayContains, _)                         => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
        case (queryExpr: QueryExpr.InList, _)                        => ParseResult.error(queryExpr.fullTerm, "`in`/`notIn` is a predicate and can not be used as a returned/output value")
        case (queryExpr: QueryExpr.Binary, _)                        => convert.binary(queryExpr)
        case (queryExpr: QueryExpr.BuiltIn, _)                       => convert.builtIn(queryExpr)
        case (queryExpr: QueryExpr.Composite, _)                     => convert.composite(queryExpr, parentContext)
        case (queryExpr: QueryExpr.ConstValue, None)                 => ParseResult.error(queryExpr.fullTerm, "No RowRepr to compare with")
        case (queryExpr: QueryExpr.InputVariableReferenceLike, None) => ParseResult.error(queryExpr.fullTerm, "No RowRepr to compare with")

    def const(queryExpr: QueryExpr.ConstValue, parentContext: TypeclassExpr.RowRepr): ParseResult[GeneratedResultDecoder] =
      ParseResult.success(GeneratedResultDecoder.single(parentContext.resultDecoder, queryExpr.fullTerm.tpe.widen))

    def query(queryExpr: QueryExpr.QueryVariableReferenceLike)(using Quotes): ParseResult[GeneratedResultDecoder] =
      ParseResult.success(GeneratedResultDecoder.single(queryExpr.rowRepr.resultDecoder, queryExpr.fullTerm.tpe.widen))

    def input(queryExpr: QueryExpr.InputVariableReferenceLike, rowRepr: TypeclassExpr.RowRepr): ParseResult[GeneratedResultDecoder] =
      ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, queryExpr.fullTerm.tpe.widen))

    def binary(queryExpr: QueryExpr.Binary)(using Quotes): ParseResult[GeneratedResultDecoder] =
      queryExpr match
        case _: QueryExpr.BinaryComp  => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
        case _: QueryExpr.BinaryAndOr => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))

    def builtIn(queryExpr: QueryExpr.BuiltIn)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
      queryExpr match
        case QueryExpr.Static(fullTerm, _, rowRepr)                        => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, fullTerm.tpe.widen))
        case _: QueryExpr.CountWithArg                                     => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.long.resultDecoder, TypeRepr.of[Long]))
        case QueryExpr.AggregateWithArg(fullTerm, fn, coalesceZero, inner) =>
          // SUM/AVG/MIN/MAX over an empty result set return SQL NULL -> decode as `Option[_]`.
          // The `sum(_)`/`sum.orZero(_)` variants wrap the result in `COALESCE(_, 0)`, so they are
          // never null and decode to a non-optional `Out`. The DSL declares the widened result type
          // (see `SumType`/`AvgType`), so the full term's type is already `Out` / `Option[Out]`.
          val resultTpe: TypeRepr = fullTerm.tpe.widen
          fn match
            case AggregateFunction.Min | AggregateFunction.Max =>
              // MIN/MAX keep the column's own type: reuse its `RowRepr`, wrapped in `optional`.
              ParseResult.success(GeneratedResultDecoder.single(inner.rowRepr.optional.resultDecoder, resultTpe))
            case AggregateFunction.Sum | AggregateFunction.Avg =>
              // non-COALESCE result type is `Option[Out]`; COALESCE result type is `Out` directly.
              val outTpe: Option[TypeRepr] = if coalesceZero then Some(resultTpe) else resultTpe.typeArgs.headOption
              outTpe match
                case Some(outTpe) =>
                  convert.aggregateDecoder(outTpe, optional = !coalesceZero) match
                    case Some(dec) => ParseResult.success(GeneratedResultDecoder.single(dec, resultTpe))
                    case None      => ParseResult.error(fullTerm, s"unsupported ${fn.sql} result type: ${outTpe.showAnsiCode}")
                case None =>
                  ParseResult.error(fullTerm, s"expected an Option[_] result type for ${fn.sql}, got: ${resultTpe.showAnsiCode}")

    /** Result decoder for a widened SUM/AVG output type; `optional` wraps it for the nullable variants. */
    private def aggregateDecoder(outTpe: TypeRepr, optional: Boolean)(using Quotes): Option[TypeclassExpr.ResultDecoder] = {
      val base: Option[Expr[oxygen.sql.schema.ResultDecoder[?]]] =
        if outTpe =:= TypeRepr.of[Long] then Some('{ oxygen.sql.schema.RowRepr.long.decoder })
        else if outTpe =:= TypeRepr.of[Double] then Some('{ oxygen.sql.schema.RowRepr.double.decoder })
        else if outTpe =:= TypeRepr.of[Float] then Some('{ oxygen.sql.schema.RowRepr.float.decoder })
        else if outTpe =:= TypeRepr.of[BigInt] then Some('{ oxygen.sql.schema.RowRepr.bigInt.decoder })
        else if outTpe =:= TypeRepr.of[BigDecimal] then Some('{ oxygen.sql.schema.RowRepr.bigDecimal.decoder })
        else None
      base.map { dec =>
        val full: Expr[oxygen.sql.schema.ResultDecoder[?]] = if optional then '{ $dec.optional } else dec
        TypeclassExpr.ResultDecoder(full)
      }
    }

    def composite(queryExpr: QueryExpr.Composite, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
      queryExpr match
        case QueryExpr.InstantiateTable(_, gen, tr, _) => ParseResult.success(GeneratedResultDecoder.single(tr.tableRowRepr.resultDecoder, gen.typeRepr))
        case _: QueryExpr.StringConcat                 => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.string.resultDecoder, TypeRepr.of[String]))
        case QueryExpr.OptionApply(_, inner)           => convert(inner, parentContext.map(_.optional))
        case _: QueryExpr.OptionNullability            => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))

  }

  def ret(r: ReturningPart, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    r.returningExprs.traverse(e => convert(e.expr, parentContext)).map(GeneratedResultDecoder.flatten(_))

}
