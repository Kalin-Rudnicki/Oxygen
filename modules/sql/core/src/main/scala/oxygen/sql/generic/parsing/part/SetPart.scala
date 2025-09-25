package oxygen.sql.generic.parsing.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class SetPart(
    parts: NonEmptyList[SetPart.SingleSet],
) {

  def show(using Quotes): String =
    s"    SET ${parts.head.show}${parts.tail.map { p => s"\n        ${p.show}" }.mkString}"

}
object SetPart extends QueryParser[SetPart] {

  final case class SingleSet(
      lhsExpr: QueryExpr.QueryLike,
      rhsExpr: QueryExpr.Unary,
  ) {

    def show(using Quotes): String = s"${lhsExpr.show} := ${rhsExpr.show}"

  }

  private def parseSetPart(term: Term, refs: RefMap, rootQueryRef: QueryReference.Query)(using ParseContext, Quotes): ParseResult[SingleSet] =
    for {
      fun <- Function.parse(term).unknownAsError
      p1 <- fun.parseParam1
      (lhs, rhs) <- fun.body match {
        case Apply(Apply(TypeApply(Ident(":="), _ :: Nil), lhs :: Nil), rhs :: Nil) => ParseResult.Success((lhs, rhs))
        case _                                                                      => ParseResult.error(fun.body, "invalid set part")
      }

      _ <- ParseResult.validate(p1.tpe =:= rootQueryRef.param.tpe)(p1.tree, "set param does not match type of update table?")
      widenedLhsTpe = lhs.tpe.widen
      widenedRhsTpe = rhs.tpe.widen

      refsWithParam = refs.addAlias(p1, rootQueryRef.param)
      lhsExpr <- RawQueryExpr.Unary.parse((lhs, refsWithParam)).unknownAsError
      lhsExpr <- QueryExpr.Unary.parse(lhsExpr).unknownAsError
      lhsExpr <- lhsExpr match {
        case lhsExpr: QueryExpr.QueryLike if lhsExpr.param.sym == rootQueryRef.param.sym => ParseResult.Success(lhsExpr)
        case _                                                                           => ParseResult.error(lhsExpr.rootIdent, "root of set lhs is not the update table?")
      }

      rhsExpr <- RawQueryExpr.Unary.parse((rhs, refs)).unknownAsError
      rhsExpr <- QueryExpr.Unary.parse(rhsExpr).unknownAsError

      _ <- ParseResult.validate(widenedLhsTpe <:< widenedRhsTpe)(fun.body, s"set types do not match:  ${widenedLhsTpe.showAnsiCode} := ${widenedRhsTpe.showAnsiCode}")
    } yield SingleSet(lhsExpr, rhsExpr)

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): ParseResult[(SetPart, String, RefMap, Term)] =
    for {
      fc1 <- FunctionCall.parseTyped[T.UpdateSet](term, "function 1").ignore
      _ <- fc1.funct.parseEmptyParams
      f1Name <- functionNames.mapOrFlatMap.parse(fc1.nameRef).unknownAsError
      (_, setTerms) <- fc1.lhs match { // TODO (KR) : validate `setIdent`
        case Apply(Select(setIdent: Ident, "apply"), set0 :: Repeated.ignoreTyped(setN, _) :: Nil) => ParseResult.Success((setIdent, NonEmptyList(set0, setN)))
        case _                                                                                     => ParseResult.error(fc1.lhs, "does not look like `set(...)`")
      }

      rootQueryRef <- refs.getRootQueryRef(fc1.lhs)
      parts <- setTerms.traverse(parseSetPart(_, refs, rootQueryRef))
    } yield (SetPart(parts), f1Name, refs, fc1.funct.body)

}
