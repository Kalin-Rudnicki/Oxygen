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

  def showParams(params: List[Function.Param]): String =
    s"params(${params.map { p => s"\n  ${p.name}: ${p.tpe.showCode} <${p.tree.symbol.fullName}>," }.mkString}\n) "

  private def makeTupleAccess(size: Int, idx: Int)(using Quotes): Expr[Any] => Expr[Any] = {
    val selectSym: Symbol = Symbol.tupleClass(size).fieldMember(s"_${idx + 1}")
    _.toTerm.select(selectSym).asExpr
  }

  private def parseDefDef(term: Term)(using ParseContext): ParseResult[(ValDef, Term)] =
    for {
      defDef <- term match {
        case Block((defDef: DefDef) :: Nil, Closure(_, _)) => ParseResult.Success(defDef)
        case _                                             => ParseResult.unknown(term, "not a block with a closure")
      }

      params <- defDef.paramss match {
        case (head: TermParamClause) :: Nil => ParseResult.Success(head)
        case params                         => ParseResult.error(defDef, s"Expected DefDef to have single param group, but had (${params.size})")
      }
      valDef <- params.params match {
        case valDef :: Nil => ParseResult.Success(valDef)
        case valDefs       => ParseResult.error(defDef, s"Expected DefDef to have single val def, but had (${valDefs.size})")
      }

      rhs <- defDef.rhs match {
        case Some(rhs) => ParseResult.Success(rhs)
        case None      => ParseResult.error(defDef, "DefDef does not have RHS")
      }
    } yield (valDef, rhs)

  private def parseRHSCaseDef(caseDef: CaseDef)(using ParseContext, Quotes): ParseResult[Function] =
    caseDef.pattern match {
      case Unapply(fun, Nil, patterns) =>
        for {
          _ <- fun match {
            case TypeApply(Select(Ident(name), "unapply"), _) if name.startsWith("Tuple") => ParseResult.Success(())
            case _                                                                        => ParseResult.error(fun, "unapply a non-tuple")
          }
          params <- patterns.zipWithIndex.traverse {
            case (bind @ Bind("_", _), _)                     => ParseResult.error(bind, "ignored tuple args currently not supported")
            case (bind @ Bind(name, ident @ Ident("_")), idx) => ParseResult.success(Param(name, ident.tpe, bind, makeTupleAccess(patterns.size, idx).some))
            case (pat, _)                                     => ParseResult.error(pat, "invalid case pattern")
          }
        } yield Function(caseDef, params, caseDef.rhs)
      case Ident("_") => ParseResult.Success(Function(caseDef, Nil, caseDef.rhs))
      case _          => ParseResult.error(caseDef, "unable to parse match case")
    }

  private def parseRHSMatch(valDef: ValDef, mat: Match)(using ParseContext, Quotes): ParseResult[Function] =
    for {
      _ <- ParseResult.validate(mat.scrutinee.symbol == valDef.symbol)(mat, "function match doesnt reference input?")
      caseDef <- mat.cases match {
        case caseDef :: Nil => ParseResult.Success(caseDef)
        case cases          => ParseResult.error(mat, s"match has non-single case (${cases.size})")
      }
      function <- parseRHSCaseDef(caseDef)
    } yield function

  private def parseRHS(root: Term, valDef: ValDef, rhs: Term)(using ParseContext, Quotes): ParseResult[Function] =
    rhs match {
      case mat: Match => parseRHSMatch(valDef, mat)
      case _          => ParseResult.Success(Function(root, Param(valDef.name, valDef.tpt.tpe, valDef, None) :: Nil, rhs))
    }

  override def parse(term: Term)(using ParseContext, Quotes): ParseResult[Function] =
    for {
      (valDef, rhs) <- ParseContext.add("core function structure") { parseDefDef(term) }
      function <- ParseContext.add("function rhs") { parseRHS(term, valDef, rhs) }
    } yield function

}
