package oxygen.sql.generic.generation

import java.time.Instant
import java.util.UUID
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.model.part.*
import oxygen.sql.generic.parsing.*
import scala.quoted.*

final class DecoderBuilder {

  def convert(queryExpr: QueryExpr, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    (queryExpr, parentContext) match {
      case (QueryExpr.ConstValue(fullTerm, _), Some(rowRepr)) => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, fullTerm.tpe.widen))
      case (input: QueryExpr.UnaryInput, Some(rowRepr))       => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, input.fullTerm.tpe.widen))
      case (input: QueryExpr.UnaryInput, _)                   => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case (input: QueryExpr.ConstValue, _)                   => ParseResult.error(input.fullTerm, "no query reference to compare input to")
      case (query: QueryExpr.UnaryQuery, _)                   => ParseResult.success(GeneratedResultDecoder.single(query.rowRepr.resultDecoder, query.fullTerm.tpe.widen))
      case (QueryExpr.Static(fullTerm, _, rowRepr), _)        => ParseResult.success(GeneratedResultDecoder.single(rowRepr.resultDecoder, fullTerm.tpe.widen))
      case (_: QueryExpr.BinaryAndOr, _)                      => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
      case (_: QueryExpr.BinaryComp, _)                       => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.boolean.resultDecoder, TypeRepr.of[Boolean]))
      case (QueryExpr.InstantiateTable(_, gen, tr, _), _)     => ParseResult.success(GeneratedResultDecoder.single(tr.tableRowRepr.resultDecoder, gen.typeRepr))
      case (_: QueryExpr.RandomUUID, _)                       => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.uuid.resultDecoder, TypeRepr.of[UUID]))
      case (_: QueryExpr.InstantNow, _)                       => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.instant.resultDecoder, TypeRepr.of[Instant]))
      case (_: QueryExpr.StringConcat, _)                     => ParseResult.success(GeneratedResultDecoder.single(TypeclassExpr.RowRepr.string.resultDecoder, TypeRepr.of[String]))
    }

  def ret(r: ReturningPart, parentContext: Option[TypeclassExpr.RowRepr])(using ParseContext, Quotes): ParseResult[GeneratedResultDecoder] =
    r.returningExprs.traverse(convert(_, parentContext)).map(GeneratedResultDecoder.flatten(_))

}
