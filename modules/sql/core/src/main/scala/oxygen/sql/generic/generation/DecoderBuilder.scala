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
        case (queryExpr: QueryExpr.Binary, _)                                       => convert.binary(queryExpr)
        case (queryExpr: QueryExpr.BuiltIn, _)                                      => convert.builtIn(queryExpr)
        case (queryExpr: QueryExpr.Composite, _)                                    => convert.composite(queryExpr, parentContext)
        case (queryExpr: QueryExpr.ConstValue, None)                                => ParseResult.error(queryExpr.fullTerm, "No RowRepr to compare with")
        case (queryExpr: QueryExpr.InputVariableReferenceLike, None)                => ParseResult.error(queryExpr.fullTerm, "No RowRepr to compare with")

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

    def builtIn(queryExpr: QueryExpr.BuiltIn)(using Quotes): ParseResult[GeneratedResultDecoder] =
      queryExpr match
        case QueryExpr.Static(fullTerm, _, rowRepr) => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, fullTerm.tpe.widen))
        case _: QueryExpr.CountWithArg              => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.long.resultDecoder, TypeRepr.of[Long]))

    def composite(queryExpr: QueryExpr.Composite, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
      queryExpr match
        case QueryExpr.InstantiateTable(_, gen, tr, _) => ParseResult.success(GeneratedResultDecoder.single(tr.tableRowRepr.resultDecoder, gen.typeRepr))
        case _: QueryExpr.StringConcat                 => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.string.resultDecoder, TypeRepr.of[String]))
        case QueryExpr.OptionApply(_, inner)           => convert(inner, parentContext.map(_.optional))
        case _: QueryExpr.OptionNullability            => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))

  }

  def ret(r: ReturningPart.Basic, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    r.returningExprs.traverse(e => convert(e.expr, parentContext)).map(GeneratedResultDecoder.flatten(_))

}
