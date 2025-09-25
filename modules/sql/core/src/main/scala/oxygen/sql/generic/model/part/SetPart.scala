package oxygen.sql.generic.model.part

import oxygen.predef.core.*
import oxygen.quoted.*
import oxygen.sql.generic.model.*
import oxygen.sql.generic.parsing.*
import oxygen.sql.query.dsl.T
import scala.quoted.*

final case class SetPart(
    setExprs: NonEmptyList[SetPart.SetExpr],
) {

  def show(using Quotes): String =
    s"    SET ${setExprs.head.show}${setExprs.tail.map { p => s"\n        ${p.show}" }.mkString}"

}
object SetPart extends MapChainParser[SetPart] {

  final case class SetExpr(
      fieldToSetExpr: QueryExpr.UnaryQuery,
      setValueExpr: QueryExpr.Unary,
  ) {

    def show(using Quotes): String = s"${fieldToSetExpr.show} := ${setValueExpr.show}"

  }

  private def parseSingleSet(term: Term, refs: RefMap, rootQueryRef: QueryParam.Query)(using ParseContext, Quotes): ParseResult[SetExpr] =
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
        case lhsExpr: QueryExpr.UnaryQuery if lhsExpr.param.sym == rootQueryRef.param.sym => ParseResult.Success(lhsExpr)
        case _                                                                            => ParseResult.error(lhsExpr.rootIdent, "root of set lhs is not the update table?")
      }

      rhsExpr <- RawQueryExpr.Unary.parse((rhs, refs)).unknownAsError
      rhsExpr <- QueryExpr.Unary.parse(rhsExpr).unknownAsError

      _ <- ParseResult.validate(widenedLhsTpe <:< widenedRhsTpe)(fun.body, s"set types do not match:  ${widenedLhsTpe.showAnsiCode} := ${widenedRhsTpe.showAnsiCode}")
    } yield SetExpr(lhsExpr, rhsExpr)

  override def parse(term: Term, refs: RefMap, prevFunction: String)(using ParseContext, Quotes): MapChainParseResult[SetPart] =
    for {
      mapAAFC <- AppliedAnonFunctCall.parseTyped[T.UpdateSet](term, "map function").ignore
      _ <- mapAAFC.funct.parseEmptyParams
      mapFunctName <- functionNames.mapOrFlatMap.parse(mapAAFC.nameRef).unknownAsError
      (_, setTerms) <- mapAAFC.lhs match { // TODO (KR) : validate `setIdent`
        case Apply(Select(setIdent: Ident, "apply"), set0 :: Repeated.ignoreTyped(setN, _) :: Nil) => ParseResult.Success((setIdent, NonEmptyList(set0, setN)))
        case _                                                                                     => ParseResult.error(mapAAFC.lhs, "does not look like `set(...)`")
      }

      rootQueryRef <- refs.getRootQueryRef(mapAAFC.lhs)
      parts <- setTerms.traverse(parseSingleSet(_, refs, rootQueryRef))
    } yield MapChainResult(SetPart(parts), mapFunctName, refs, mapAAFC.appliedFunctionBody)

}
