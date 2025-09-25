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

final case class Join(
    escapingParam: QueryReference.Query,
    onParam: QueryReference.Query,
    on: QueryExpr,
    tableRepr: Expr[TableRepr[?]],
) {

  def show(using Quotes): String =
    s"""
         |    Join ${onParam.param.tpe.showAnsiCode} ${onParam.show}
         |      ON ${on.show}""".stripMargin

  def queryRefs: Growable[QueryReference] =
    escapingParam +: onParam +: on.queryRefs

}
object Join extends QueryParser[Join] {

  // FIX-PRE-MERGE (KR) : remove
  /*

       input[UUID].flatMap { i =>
         select[Person].flatMap { p1 =>
           join[Person]
             .withFilter { p2 => p2.last == p1.last && p2.id != p1.id }
             .flatMap { p2 =>
               where
                 .withFilter { _ => p1.id == i }
                 .map { _ => p2 }
             }
         }
       }

   */

  // FIX-PRE-MERGE (KR) : move comment to [[QueryParser]], and also add example for non-with-filter
  /**
    * params coming in from parent: [i, p1]
    * ------------------------- Full Query -------------------------
    * join[Person]
    *   .withFilter { p2_1 => p2_1.last == p1.last && p2_1.id != p1.id }
    *   .flatMap { p2_2 =>
    *     where
    *       .withFilter { _ => p1.id == i }
    *       .map { _ => p2_2 }
    *  }
    * ------------------------- fc1.lhs -------------------------
    * join[Person]
    *   .withFilter { p2_1 => p2_1.last == p1.last && p2_1.id != p1.id }
    * ------------------------- fc1.funct.body -------------------------
    * where
    *   .withFilter { _ => p
    *   .map { _ => p2_2 }
    * ------------------------- fc2.lhs -------------------------
    * join[Person]
    * ------------------------- fc2.funct.body -------------------------
    * p2_1.last == p1.last && p2_1.id != p1.id
    * ------------------------- ... -------------------------
    * core parts:
    * - FunctionCall.lhs
    * - FunctionCall.nameRef
    * - FunctionCall.funct
    *   - FunctionCall.funct.params
    *   - FunctionCall.funct.body
    * ------------------------- ... -------------------------
    * ((fc2.lhs))
    *     .((fc2.nameRef)) { ((fc2.funct.params)) => ((fc2.funct.body)) }
    *     .((fc1.nameRef)) { ((fc1.funct.params)) => ((fc1.funct.body)) }
    * ------------------------- ... -------------------------
    * ((fc1.lhs))
    *     .((fc1.nameRef)) { ((fc1.funct.params)) => ((fc1.funct.body)) }
    */
  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(Join, String, RefMap, Term)] =
    for {
      fc1 <- FunctionCall.parseTyped[T.Join[?]](term, "function 1").ignore
      p1 <- fc1.funct.parseParam1
      f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
      (fc2, tableRepr) <-
        FunctionCall
          .parseTyped[T.Partial.JoinLike](fc1.lhs, "function 2")
          .parseLhs {
            // TODO (KR) : left join
            case '{ oxygen.sql.query.dsl.Q.join[a](using $tableRepr) }     => ParseResult.Success(tableRepr)
            case '{ oxygen.sql.query.dsl.Q.leftJoin[a](using $tableRepr) } => ParseResult.Success(tableRepr)
          }
          .unknownAsError
      p2 <- fc2.funct.parseParam1
      _ <- functionNames.withFilter.parse(fc2.nameRef).unknownAsError

      escapingParam = QueryReference.Query(p1, tableRepr, false)
      onParam = QueryReference.Query(p2, tableRepr, false)
      newRefs = refs.add(
        escapingParam,
        onParam,
      )

      onExpr <- RawQueryExpr.parse((fc2.funct.body.simplify, newRefs)).unknownAsError
      onExpr <- QueryExpr.parse(onExpr)

      // FIX-PRE-MERGE (KR) : remove
      /*
        _ = report.errorAndAbort(
          s"""--- term ---
             |${term.showAnsiCode}
             |
             |--- refs ---
             |$refs
             |
             |--- prevFunction ---
             |$prevFunction
             |
             |--- escapingParam ---
             |${escapingParam.show}
             |$escapingParam
             |
             |--- onParam ---
             |${onParam.show}
             |$onParam
             |
             |--- onExpr ---
             |${onExpr.show}
             |$onExpr
             |
             |--- f1Name ---
             |$f1Name
             |
             |--- newRefs ---
             |$newRefs
             |
             |--- fc1.lhs ---
             |${fc1.lhs.showAnsiCode}
             |
             |--- fc1.funct.body ---
             |${fc1.funct.body.showAnsiCode}
             |
             |--- fc2.lhs ---
             |${fc2.lhs.showAnsiCode}
             |
             |--- fc2.funct.body.simplify ---
             |${fc2.funct.body.simplify.showAnsiCode}
             |
             |""".stripMargin,
        )
       */

    } yield (Join(escapingParam, onParam, onExpr, tableRepr), f1Name, newRefs, fc1.funct.body)

}
