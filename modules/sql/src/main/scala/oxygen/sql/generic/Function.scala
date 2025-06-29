package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] final case class Function(
    rootTree: Tree,
    params: List[Function.Param],
    body: Term,
) {

  def parseEmptyParams(using ParseContext): ParseResult.Known[Unit] = params match
    case Nil => ParseResult.Success(())
    case _   => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

  def parseSingleParam(using ParseContext): ParseResult.Known[Function.Param] = params match
    case p :: Nil => ParseResult.Success(p)
    case _        => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

  def showParams: String = Function.showParams(params)

}
private[generic] object Function extends Parser[Term, Function] {

  final case class Param(
      name: String,
      tpe: TypeRepr,
      tree: Tree,
      fromInput: Option[Expr[Any] => Expr[Any]],
  ) {

    def toIndentedString: IndentedString =
      IndentedString.section("Param:")(
        s"name: $name",
        s"type: ${tpe.showCode}",
      )

  }

  sealed trait RootParam {

    def valDef: ValDef

    final def symbol: Symbol = valDef.symbol

  }
  object RootParam {

    final case class Ignored(valDef: ValDef) extends RootParam

    final case class Named(valDef: ValDef) extends RootParam

    final case class TupleUnapply(valDef: ValDef, children: List[TupleUnapplyPart]) extends RootParam

    sealed trait TupleUnapplyPart {

      def tree: Tree

      final def symbol: Symbol = tree.symbol

    }
    object TupleUnapplyPart {

      final case class Ignored(tree: Tree) extends TupleUnapplyPart

      final case class Named(name: String, tpe: TypeRepr, tree: Tree, convert: Expr[Any] => Expr[Any]) extends TupleUnapplyPart

    }

  }

  def showParams(params: List[Function.Param]): String =
    s"params(${params.map { p => s"\n  ${p.name}: ${p.tpe.showCode} <${p.tree.symbol.fullName}>," }.mkString}\n) "

  private def makeTupleAccess(idx: Int)(using Quotes): Expr[Any] => Expr[Any] =
    _.toTerm.select(s"_${idx + 1}").asExpr

  private def extractMatch(valDef: ValDef, mat: Match)(using Quotes, ParseContext): ParseResult[(Function.RootParam, Term)] =
    for {
      caseDef <- mat.cases match {
        case kase :: Nil => ParseResult.Success(kase)
        case _           => ParseResult.error(mat, s"expected single case-def, but got ${mat.cases.size}")
      }
      param <- caseDef.pattern match {
        case Ident("_")                  => ParseResult.Success(Function.RootParam.Ignored(valDef))
        case Unapply(fun, Nil, patterns) =>
          for {
            _ <- fun match {
              case TypeApply(Select(Ident(name), "unapply"), _) if name.startsWith("Tuple") => ParseResult.Success(())
              case _                                                                        => ParseResult.error(fun, "unapply a non-tuple")
            }
            parts <- patterns.zipWithIndex.traverse {
              case (bind @ Bind("_", _), _)                     => ParseResult.Success(Function.RootParam.TupleUnapplyPart.Ignored(bind))
              case (bind @ Bind(name, ident @ Ident("_")), idx) => ParseResult.success(Function.RootParam.TupleUnapplyPart.Named(name, ident.tpe, bind, makeTupleAccess(idx)))
              case (pat, _)                                     => ParseResult.error(pat, "invalid case pattern")
            }
          } yield Function.RootParam.TupleUnapply(valDef, parts)
        case _ => ParseResult.error(caseDef, "unable to parse match case")
      }
    } yield (param, caseDef.rhs)

  private def convertNormalParam(valDef: ValDef): Function.RootParam =
    if (valDef.name == "_") Function.RootParam.Ignored(valDef)
    else Function.RootParam.Named(valDef)

  private def mergeParamsAndRHS(tree: Tree, params: List[ValDef], rhs: Term)(using Quotes, ParseContext): ParseResult[(List[Function.RootParam], Term)] =
    (params, rhs) match {
      case (valDef :: Nil, mat: Match) => extractMatch(valDef, mat).map { case (p, t) => (p :: Nil, t) }
      case (_, _: Match)               => ParseResult.error(tree, "found match RHS with non-single param")
      case (valDefs, rhs)              => ParseResult.Success((valDefs.map(convertNormalParam), rhs))
    }

  override def parse(term: Term)(using ParseContext, Quotes): ParseResult[Function] =
    for {
      defDef <- ParseContext.add("def-def structure") {
        term match {
          case Block((defDef: DefDef) :: Nil, Closure(_, _)) => ParseResult.Success(defDef)
          case _                                             => ParseResult.unknown(term, "not a DefDef Block with a Closure")
        }
      }
      paramClause <- ParseContext.add("param clause") {
        defDef.paramss match {
          case (params: TermParamClause) :: Nil => ParseResult.Success(params)
          case _                                => ParseResult.error(defDef, "expected single term param clause")
        }
      }
      rhs <- defDef.rhs match {
        case Some(rhs) => ParseResult.Success(rhs)
        case None      => ParseResult.error(defDef, "function doesn't have RHS !?")
      }
      (_, _) <- ParseContext.add("resolve params") { mergeParamsAndRHS(defDef, paramClause.params, rhs) }
      _ <- ParseResult.error(defDef, "parse this")
      // (valDefs, rhs) <- ParseContext.add("core function structure") { parseDefDef(term) }
      // function <- ParseContext.add("function rhs") { parseRHS(term, valDefs, rhs) }
    } yield ???

}
