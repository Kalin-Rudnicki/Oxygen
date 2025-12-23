package oxygen.meta

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

object ExprEvaluator {

  private def simplify(term: Term)(using Quotes): Term =
    term match {
      case Inlined(_, Nil, term)   => simplify(term)
      case Block(statements, term) =>
        simplify(term) match
          case Block(statements2, term2) => Block.companion.apply(statements ++ statements2, term2)
          case other                     => Block.companion.apply(statements, other)
      case _ => term
    }

  private def simplify(tree: Tree)(using Quotes): Tree =
    tree match
      case term: Term => simplify(term)
      case _          => tree

  private def showParamClause(pc: ParamClause): IndentedString =
    pc match {
      case clause: TypeParamClause =>
        clause.params.map(_.name).mkString("[", ", ", "]")
      case clause: TermParamClause =>
        val prefix: String = if clause.isGiven then "using " else if clause.isImplicit then "implicit " else ""
        clause.params.map { vd => s"${vd.name}: ${vd.tpt.tpe.showAnsiCode}" }.mkString("(" + prefix, ", ", ")")
    }

  private def recurse(typeRepr: TypeRepr)(using Quotes): IndentedString =
    typeRepr match {
      case typeRef: TypeRef                           => typeRef.showAnsiCode
      case AppliedType(_, reprs) if typeRepr.isTupleN => reprs.map(_.showAnsiCode).mkString("tuple: ( ", " , ", " )")
      case AppliedType(repr, reprs)                   =>
        IndentedString.keyValueSection("AppliedType:")(
          "tycon: " -> recurse(repr),
          "args: " -> reprs.map(recurse),
        )
      case _ =>
        typeRepr.toIndentedString
    }

  private def recurse(tree: Tree)(using Quotes): IndentedString =
    simplify(tree) match {
      case Block(statements, result) =>
        IndentedString.keyValueSection("Block:")(
          "statements: " -> statements.zipWithIndex.map { (statement, idx) => IndentedString.section(s":[block.$idx]:")(recurse(statement)) },
          "result: " -> recurse(result),
        )
      case ValDef(name, tpt, rhs) =>
        IndentedString.keyValueSection("ValDef:")(
          "name: " -> name,
          "tpt: " -> recurse(tpt),
          "rhs: " -> rhs.map(recurse),
        )
      case DefDef(name, paramss, tpt, rhs) =>
        IndentedString.keyValueSection("DefDef:")(
          "name: " -> name,
          "params: " -> paramss.map(showParamClause),
          "tpt: " -> recurse(tpt),
          "rhs: " -> rhs.map(recurse),
        )
      case ClassDef(name, primaryConstructor, parents, _, body) =>
        IndentedString.keyValueSection("ClassDef:")(
          "name: " -> name,
          "primaryConstructor: " -> recurse(primaryConstructor),
          "parents: " -> parents.map(_.showAnsiCode),
          "body: " -> body.map(recurse),
        )
      case Inlined(_, Nil, term)      => recurse(term)
      case Inlined(call, defns, term) =>
        IndentedString.keyValueSection("Inlined:")(
          s"call (${call.nonEmpty}): " -> call.map(recurse),
          s"defns (${defns.size}): " -> defns.map(recurse),
          "body: " -> recurse(term),
        )
      case Typed(term, tpt) =>
        IndentedString.keyValueSection("Typed:")(
          "tpt: " -> recurse(tpt),
          "term: " -> recurse(term),
        )
      case Apply(fun, args) =>
        IndentedString.keyValueSection("Apply:")(
          "fun: " -> recurse(fun),
          "args: " -> args.map(recurse),
        )
      case TypeApply(fun, args) =>
        IndentedString.keyValueSection("TypeApply:")(
          "fun: " -> recurse(fun),
          "args: " -> args.map(recurse),
        )
      case Select(qualifier, ident) =>
        IndentedString.keyValueSection("Select:")(
          "qualifier: " -> recurse(qualifier),
          "ident: " -> ident,
        )
      case New(tpt) =>
        IndentedString.section("New:")(recurse(tpt))
      case tpt: TypeTree => recurse(tpt.tpe)
      case _             => tree.toIndentedString
    }

  def evaluateExpr[A: Type](expr: Expr[A])(using Quotes): A = {
    val term: Term = expr.toTerm
    report.errorAndAbort(recurse(term).toStringColorized)
  }

}
