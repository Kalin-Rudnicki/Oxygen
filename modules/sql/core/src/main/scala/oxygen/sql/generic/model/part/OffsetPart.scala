package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class OffsetPart(
    offsetQueryExpr: QueryExpr,
) {

  def show(using Quotes): String =
    s"""
       |    OFFSET ${offsetQueryExpr.show}""".stripMargin

}
object OffsetPart extends MapChainParser[OffsetPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[OffsetPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.Offset](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      intExpr: Expr[Int] <- mapAAFC.lhs.asExpr match {
        case '{ Q.offset($intExpr) } => ParseResult.Success(intExpr)
        case _                       => ParseResult.error(mapAAFC.lhs, "does not look like `offset(...)`")
      }
      offsetQueryExpr <- RawQueryExpr.parse((intExpr.toTerm, refs))
      offsetQueryExpr <- QueryExpr.parse(offsetQueryExpr)

    } yield MapChainResult(OffsetPart(offsetQueryExpr), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
