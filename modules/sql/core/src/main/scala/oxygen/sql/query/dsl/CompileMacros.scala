package oxygen.sql.query.dsl

import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.query.*
import scala.quoted.*

private[sql] object CompileMacros {

  def apply(queryNameExpr: Expr[String], expr: Expr[QueryLike], debugExpr: Expr[Boolean])(using Quotes): Expr[QueryLike] = {
    val debug: Boolean = debugExpr.evalRequired

    val rhs: Term = expr.toTerm.removeInline
    val tmpTup: (ParsedQuery, Term) = ParsedQuery.compile(queryNameExpr, rhs, debug)
    val parsedQuery: ParsedQuery = tmpTup._1
    val newRHS: Term = tmpTup._2

    if (debug)
      report.info(
        s"""
             |=====| Compiled Query |=====
             |
             |name : ${queryNameExpr.evalEither.fold(_.showAnsiCode, identity)}
             |type: ${newRHS.tpe.widen.showAnsiCode}
             |
             |--- parsed query ---
             |
             |${parsedQuery.show}
             |
             |--- expr ---
             |
             |${newRHS.showAnsiCode}
             | """.stripMargin,
      )

    newRHS.asExprOf[QueryLike]
  }

  def queryIO[I: Type, O: Type](queryName: Expr[String], expr: Expr[QueryIO[I, O]], debug: Expr[Boolean])(using Quotes): Expr[QueryIO[I, O]] =
    apply(queryName, expr, debug).asExprOf[QueryIO[I, O]]

  def queryO[O: Type](queryName: Expr[String], expr: Expr[QueryO[O]], debug: Expr[Boolean])(using Quotes): Expr[QueryO[O]] =
    apply(queryName, expr, debug).asExprOf[QueryO[O]]

  def queryI[I: Type](queryName: Expr[String], expr: Expr[QueryI[I]], debug: Expr[Boolean])(using Quotes): Expr[QueryI[I]] =
    apply(queryName, expr, debug).asExprOf[QueryI[I]]

  def query(queryName: Expr[String], expr: Expr[Query], debug: Expr[Boolean])(using Quotes): Expr[Query] =
    apply(queryName, expr, debug).asExprOf[Query]

}
