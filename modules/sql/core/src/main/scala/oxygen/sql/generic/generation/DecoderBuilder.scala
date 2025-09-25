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
      case input: QueryExpr.InputLike => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case query: QueryExpr.QueryLike => ParseResult.success(GeneratedResultDecoder.single('{ ${ query.rowRepr }.decoder }, query.fullTerm.tpe.widen))
      case _: QueryExpr.AndOr         => ParseResult.success(GeneratedResultDecoder.single('{ RowRepr.boolean.decoder }, TypeRepr.of[Boolean]))
      case _: QueryExpr.Comp          => ParseResult.success(GeneratedResultDecoder.single('{ RowRepr.boolean.decoder }, TypeRepr.of[Boolean]))
    }

  def ret(r: ReturningPart)(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    r.returningExprs.traverse(convert).map(GeneratedResultDecoder.flatten(_))

}
