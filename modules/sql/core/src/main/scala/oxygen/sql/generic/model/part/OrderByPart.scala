package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import scala.quoted.*

final case class OrderByPart(
    orderByExprs: NonEmptyList[OrderByPart.OrderByExpr],
) {

  def show(using Quotes): String =
    s"\n    ORDER BY ${orderByExprs.head.show}${orderByExprs.tail.map { p => s"\n             ${p.show}" }.mkString}"

}
object OrderByPart extends MapChainParser[OrderByPart] {

  enum Ord { case Asc, Desc }

  final case class OrderByExpr(
      queryExpr: QueryExpr.QueryVariableReferenceLike,
      ord: Ord,
  ) {

    def show(using Quotes): String = s"${queryExpr.show} $ord"

  }

  private def parseSingleOrderByExpr(expr: Expr[T.Partial.OrderByPart], refs: RefMap)(using ParseContext, Quotes): ParseResult[OrderByExpr] =
    for {
      (rawOrdExpr, ordType) <- expr match {
        case '{ Q.asc($rawOrdExpr) }  => ParseResult.Success((rawOrdExpr, Ord.Asc))
        case '{ Q.desc($rawOrdExpr) } => ParseResult.Success((rawOrdExpr, Ord.Desc))
        case _                        => ParseResult.error(expr.toTerm, "invalid orderBy part")
      }
      queryExpr <- RawQueryExpr.VariableReferenceLike.parse((rawOrdExpr.toTerm, refs))
      queryExpr <- QueryExpr.VariableReferenceLike.parse(queryExpr)
      queryExpr <- queryExpr match
        case queryExpr: QueryExpr.QueryVariableReferenceLike => ParseResult.Success(queryExpr)
        case _                                               => ParseResult.error(queryExpr.fullTerm, "expected orderBy part to reference a query param")
    } yield OrderByExpr(queryExpr, ordType)

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[OrderByPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.OrderBy](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      orderByParts <- mapAAFC.lhs.asExpr match {
        case '{ Q.orderBy(${ Varargs(orderByParts) }*) } => ParseResult.Success(orderByParts)
        case _                                           => ParseResult.error(mapAAFC.lhs, "does not look like `orderBy(...)`")
      }
      orderByParts <- NonEmptyList.fromList(orderByParts.toList) match
        case Some(orderByParts) => ParseResult.Success(orderByParts)
        case None               => ParseResult.error(mapAAFC.lhs, "no parts provided to orderBy")

      parts <- orderByParts.traverse(parseSingleOrderByExpr(_, refs))
    } yield MapChainResult(OrderByPart(parts), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
