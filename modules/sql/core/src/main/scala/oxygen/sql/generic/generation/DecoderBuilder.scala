package oxygen.sql.generic.generation

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.schema.*
import scala.quoted.*

final class DecoderBuilder {

  def convert(queryExpr: QueryExpr)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    queryExpr match {
      case input: QueryExpr.UnaryInput            => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case input: QueryExpr.ConstValue            => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case query: QueryExpr.UnaryQuery            => ParseResult.success(GeneratedResultDecoder.single(query.rowRepr.resultDecoder, query.fullTerm.tpe.widen))
      case QueryExpr.Static(fullTerm, _, rowRepr) => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, fullTerm.tpe.widen))
      case _: QueryExpr.BinaryAndOr               => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
      case _: QueryExpr.BinaryComp                => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
    }

  def ret(r: ReturningPart)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    r.returningExprs.traverse(convert).map(GeneratedResultDecoder.flatten(_))

}
