package oxygen.sql.generic

import oxygen.predef.color.given
import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.schema.*
import scala.quoted.*

private[generic] object QueryBlock {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      From
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class From(param: Function.Param, schema: Expr[TableRepr[?, ?]]) {

    def show: String =
      s"  FROM ${param.tpe.showCode} ${param.name.greenFg}"

  }
  object From extends QueryBlockParser.Required[From] {

    override def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(From, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        schema <- f1Lhs.asExpr match {
          case '{ oxygen.sql.query.dsl.from[a](using $schema) } => ParseResult.Success(schema)
          case _                                                => ParseResult.error(f1Lhs, "invalid `from` invocation")
        }
        p1 <- f1Function.parseSingleParam
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError

        newRefs = refs.add(p1.tree.symbol -> QueryReference.Query(p1, schema, true))

      } yield (From(p1, schema), f1Name, newRefs, f1Function.body)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Join
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Join(escapingParam: Function.Param, onParam: Function.Param, on: QueryExpr, schema: Expr[TableRepr[?, ?]]) {

    def show: String =
      s"  JOIN ${escapingParam.tpe.showCode} ${escapingParam.name.greenFg}\n    ON ${on.show}"

  }
  object Join extends QueryBlockParser.Required[Join] {

    override def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(Join, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        FunctionCall(f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { FunctionCall.parse(f1Lhs) }
        schema <- f2Lhs.asExpr match {
          case '{ oxygen.sql.query.dsl.join[a](using $schema) } => ParseResult.Success(schema)
          case _                                                => ParseResult.unknown(f2Lhs, "invalid `join` invocation")
        }
        p1 <- f1Function.parseSingleParam
        p2 <- f2Function.parseSingleParam
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError
        _ <- functionNames.withFilter.parse(f2Name).unknownAsError

        newRefs = refs.add(
          p1.tree.symbol -> QueryReference.Query(p1, schema, false),
          p2.tree.symbol -> QueryReference.Query(p2, schema, false),
        )

        onExpr <- RawQueryExpr.parse((f2Function.body.simplify, newRefs)).unknownAsError
        onExpr <- QueryExpr.parse(onExpr)

      } yield (Join(p1, p2, onExpr, schema), f1Name, newRefs, f1Function.body)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Where
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Where(where: QueryExpr) {

    def show: String =
      s"  WHERE ${where.show}"

  }
  object Where extends QueryBlockParser.Required[Where] {

    override def parse(term: Term, refs: RefMap)(using ParseContext, Quotes): ParseResult[(Where, String, RefMap, Term)] =
      for {
        FunctionCall(f1Lhs, f1Name, f1Function) <- ParseContext.add("function 1") { FunctionCall.parse(term) }
        FunctionCall(f2Lhs, f2Name, f2Function) <- ParseContext.add("function 2") { FunctionCall.parse(f1Lhs) }
        _ <- f2Lhs.asExpr match {
          case '{ oxygen.sql.query.dsl.where } => ParseResult.Success(())
          case _                               => ParseResult.unknown(f2Lhs, "invalid `where` invocation")
        }
        _ <- f1Function.parseEmptyParams
        _ <- f2Function.parseEmptyParams
        f1Name <- functionNames.mapOrFlatMap.parse(f1Name).unknownAsError
        _ <- functionNames.withFilter.parse(f2Name).unknownAsError

        whereExpr <- RawQueryExpr.parse((f2Function.body.simplify, refs)).unknownAsError
        whereExpr <- QueryExpr.parse(whereExpr)

      } yield (Where(whereExpr), f1Name, refs, f1Function.body)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Returning
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Returning(term: Term, exprs: List[QueryExpr]) {

    def show: String =
      s"SELECT  ${exprs.map(_.show).mkString(" , ")}"

  }
  object Returning extends Parser[(Term, RefMap), Returning] {

    override def parse(input: (Term, RefMap))(using ParseContext, Quotes): ParseResult[Returning] =
      for {
        (term, refs) <- ParseResult.Success(input)
        terms = term match
          case unitExpr()       => Nil
          case tupleApply(args) => args.toList
          case _                => term :: Nil
        exprs <- terms.traverse(t => RawQueryExpr.parse((t, refs)))
        exprs <- exprs.traverse(t => QueryExpr.parse(t))
      } yield Returning(term, exprs)

  }

}
