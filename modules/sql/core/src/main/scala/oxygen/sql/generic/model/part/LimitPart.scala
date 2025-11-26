package oxygen.sql.generic.model.part

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class LimitPart(
    limitQueryExpr: QueryExpr,
) {

  def show(using Quotes): String =
    s"""
       |    LIMIT ${limitQueryExpr.show}""".stripMargin

}
object LimitPart extends MapChainParser[LimitPart] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[LimitPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.Limit](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      intExpr: Expr[Int] <- mapAAFC.lhs.asExpr match {
        case '{ Q.limit($intExpr) } => ParseResult.Success(intExpr)
        case _                      => ParseResult.error(mapAAFC.lhs, "does not look like `limit(...)`")
      }
      limitQueryExpr <- RawQueryExpr.parse((intExpr.toTerm, refs))
      limitQueryExpr <- QueryExpr.parse(limitQueryExpr)

    } yield MapChainResult(LimitPart(limitQueryExpr), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
