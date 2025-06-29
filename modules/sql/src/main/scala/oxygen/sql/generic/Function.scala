package oxygen.sql.generic

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

private[generic] final case class Function(
    rootTree: Tree,
    params: List[Function.RootParam],
    body: Term,
) {

  def parseEmptyParams(using ParseContext): ParseResult.Known[Unit] = params match
    case Nil => ParseResult.Success(())
    case _   => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

  def parseSingleParam(using ParseContext): ParseResult.Known[Function.Param] = params match
    case (p: Function.RootParam.Named) :: Nil => ParseResult.Success(p)
    case _ => ParseResult.error(body, s"expected single function param, but got ${params.size} - ${params.map(_.name).mkString(", ")}") // TODO (KR) : whole function pos

  def toIndentedString: IndentedString =
    IndentedString.section("Function:")(
      Function.showParams(params),
    )

  override def toString: String =
    toIndentedString.toStringColorized

}
private[generic] object Function extends Parser[Term, Function] {

  /**
    * Represents any kind of function param, whether it can be referenced/used, or not.
    */
  sealed trait AnyParam {
    def tree: Tree

    def toIndentedString: IndentedString
    override final def toString: String = toIndentedString.toStringColorized
  }

  /**
    * Represents a function param that can be referenced and used.
    */
  sealed trait Param extends AnyParam, TermTransformer {
    def name: String
    final lazy val sym: Symbol = tree.symbol
  }

  sealed trait RootParam extends AnyParam {

    def valDef: ValDef
    final lazy val name: String = valDef.name
    override final lazy val tree: Tree = valDef
    final lazy val tpe: TypeRepr = valDef.tpt.tpe.widen
    final lazy val inTpe: TypeRepr = tpe
    final lazy val outTpe: TypeRepr = tpe

    override final def toIndentedString: IndentedString =
      this match {
        case RootParam.Ignored(_) =>
          IndentedString.keyValueSection("RootParam.Ignored")("tpe: " -> tpe.showAnsiCode)
        case RootParam.Named(_) =>
          IndentedString.keyValueSection("RootParam.Named")("name: " -> name, "tpe: " -> tpe.showAnsiCode)
        case RootParam.TupleUnapply(_, children) =>
          val childrenIdt: IndentedString = IndentedString.inline(children.map(_.toIndentedString)*)
          IndentedString.keyValueSection("RootParam.Ignored")("name: " -> name, "tpe: " -> tpe.showAnsiCode, "children: " -> childrenIdt)
      }

  }
  object RootParam {

    final case class Ignored(valDef: ValDef) extends RootParam, AnyParam

    final case class Named(valDef: ValDef) extends RootParam, Param {

      override def convertTerm(term: Term)(using Quotes): Term = term

    }

    final case class TupleUnapply(valDef: ValDef, children: List[TupleUnapplyPart]) extends RootParam, AnyParam

  }

  sealed trait TupleUnapplyPart extends AnyParam {
    def parentValDef: ValDef

    override final def toIndentedString: IndentedString =
      this match {
        case TupleUnapplyPart.Ignored(_, _, idx) =>
          IndentedString.keyValueSection("TupleUnapplyPart.Ignored")("idx: " -> idx.toString)
        case TupleUnapplyPart.Named(_, name, tpe, _, idx) =>
          IndentedString.keyValueSection("TupleUnapplyPart.Ignored")("name: " -> name, "tpe: " -> tpe.showAnsiCode, "idx: " -> idx.toString)
      }

  }
  object TupleUnapplyPart {

    final case class Ignored(parentValDef: ValDef, tree: Tree, idx: Int) extends TupleUnapplyPart, AnyParam

    final case class Named(parentValDef: ValDef, name: String, tpe: TypeRepr, tree: Tree, idx: Int) extends TupleUnapplyPart, Param {

      override def inTpe: TypeRepr = parentValDef.tpt.tpe.widen
      override def outTpe: TypeRepr = tpe.widen

      override def convertTerm(term: Term)(using Quotes): Term = term.select(s"_${idx + 1}")

    }

  }

  def showParams(params: List[Function.RootParam]): IndentedString =
    IndentedString.section("params:")(params.map(_.toIndentedString)*)

  private def extractMatch(valDef: ValDef, mat: Match)(using ParseContext): ParseResult[(Function.RootParam, Term)] =
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
              case (bind @ Bind("_", _), idx)                   => ParseResult.Success(Function.TupleUnapplyPart.Ignored(valDef, bind, idx))
              case (bind @ Bind(name, ident @ Ident("_")), idx) => ParseResult.success(Function.TupleUnapplyPart.Named(valDef, name, ident.tpe, bind, idx))
              case (pat, idx)                                   => ParseResult.error(pat, s"invalid case pattern @ idx=$idx")
            }
          } yield Function.RootParam.TupleUnapply(valDef, parts)
        case _ => ParseResult.error(caseDef, "unable to parse match case")
      }
    } yield (param, caseDef.rhs)

  private def convertNormalParam(valDef: ValDef): Function.RootParam =
    if (valDef.name == "_") Function.RootParam.Ignored(valDef)
    else Function.RootParam.Named(valDef)

  private def mergeParamsAndRHS(tree: Tree, params: List[ValDef], rhs: Term)(using ParseContext): ParseResult[(List[Function.RootParam], Term)] =
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
