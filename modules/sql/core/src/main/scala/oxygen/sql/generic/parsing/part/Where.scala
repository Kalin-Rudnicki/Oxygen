package oxygen.sql.generic.parsing.part

import oxygen.core.typeclass.Zip3
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.{Q, T}
import oxygen.sql.schema.*
import scala.annotation.tailrec
import scala.quoted.*

final case class Where(
    where: QueryExpr,
) {

  def show(using Quotes): String =
    s"""
         |    WHERE ${where.show}""".stripMargin

}
object Where extends QueryParser[Where] {

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Where, String, RefMap, Term)] = {
    for {
      fc1 <- FunctionCall.parseTyped[T.Where](term, "function 1").ignore
      _ <- fc1.funct.parseEmptyParams
      f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
      fc2 <- FunctionCall.parseTyped[T.Partial.Where](fc1.lhs, "function 2").filterLhs { case '{ Q.where } => }.unknownAsError
      _ <- fc2.funct.parseEmptyParams
      _ <- functionNames.withFilter.parse(fc2.nameRef).unknownAsError

      whereExpr <- RawQueryExpr.parse((fc2.funct.body.simplify, refs)).unknownAsError
      whereExpr <- QueryExpr.parse(whereExpr).unknownAsError

    } yield (Where(whereExpr), f1Name, refs, fc1.funct.body)
  }

}
