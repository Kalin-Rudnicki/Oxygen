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

  sealed trait Params
  object Params {

    final case class Wild(valDef: ValDef, wild: Wildcard) extends Params

    final case class SingleVal(valDef: ValDef) extends Params

    // TODO (KR) :

  }

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
    val term: Term
    final lazy val tpe: TypeRepr = term.tpe
  }
  object RootParam {

    final case class Wild(term: Wildcard) extends RootParam
    final case class Identifier(term: Ident) extends RootParam
    final case class Tupled(term: Match, children: List[TupledParam]) extends RootParam

  }

  final case class TupledParam(tree: Ident, fromParent: Expr[Any] => Expr[Any])

  def showParams(params: List[Function.Param]): String =
    s"params(${params.map { p => s"\n  ${p.name}: ${p.tpe.showCode} <${p.tree.symbol.fullName}>," }.mkString}\n) "

  // FIX-PRE-MERGE (KR) : remove
  /*
  private def makeTupleAccess(size: Int, idx: Int)(using Quotes): Expr[Any] => Expr[Any] = {
    val selectSym: Symbol = Symbol.tupleClass(size).fieldMember(s"_${idx + 1}")
    _.toTerm.select(selectSym).asExpr
  }

  private def parseDefDef(term: Term)(using ParseContext): ParseResult[(List[ValDef], Term)] =
    for {
      defDef <- term match {
        case Block((defDef: DefDef) :: Nil, Closure(_, _)) => ParseResult.Success(defDef)
        case _                                             => ParseResult.unknown(term, "not a block with a closure")
      }

      params <- defDef.paramss match {
        case (head: TermParamClause) :: Nil => ParseResult.Success(head)
        case params                         => ParseResult.error(defDef, s"Expected DefDef to have single param group, but had (${params.size})")
      }
      valDefs = params.params

      rhs <- defDef.rhs match {
        case Some(rhs) => ParseResult.Success(rhs)
        case None      => ParseResult.error(defDef, "DefDef does not have RHS")
      }
    } yield (valDefs, rhs)

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

  private def parseRHS(root: Term, valDefs: List[ValDef], rhs: Term)(using ParseContext, Quotes): ParseResult[Function] =
    rhs match {
      case mat: Match => parseRHSMatch(valDef, mat)
      case _          => ParseResult.Success(Function(root, Param(valDef.name, valDef.tpt.tpe, valDef, None) :: Nil, rhs))
    }
   */

  //           _ <-   : ...
  //           a <-   : ...
  //      (a, b) <-   : ...
  // case (a, b) <-   : ...

  private def extractMatch(valDef: ValDef, mat: Match)(using ParseContext): ParseResult[(Function.Param, Term)] =
    ??? // FIX-PRE-MERGE (KR) :

  private def convertNormalParam(valDef: ValDef)(using ParseContext): ParseResult[Function.Param] =
    ??? // FIX-PRE-MERGE (KR) :

  // FIX-PRE-MERGE (KR) :
  @scala.annotation.nowarn
  private def mergeParamsAndRHS(tree: Tree, params: List[ValDef], rhs: Term)(using ParseContext): ParseResult[(List[Function.Param], Term)] =
    (params, rhs) match {
      case (valDef :: Nil, mat: Match) => extractMatch(valDef, mat).map { case (p, t) => (p :: Nil, t) }
      case (_, _: Match)               => ParseResult.error(tree, "found match RHS with non-single param")
      case (valDefs, rhs)              => valDefs.traverse(convertNormalParam(_)).map((_, rhs))
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
