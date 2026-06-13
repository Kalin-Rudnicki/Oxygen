package oxygen.cli

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      CliFunctionParamType
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class CliFunctionParamType extends scala.annotation.Annotation
object CliFunctionParamType {
  sealed abstract class nonCustom extends CliFunctionParamType

  // Hand-written so `@envVar` / `@envConfig`'s defaulted args parse: read literal string args off the
  // `new ...(...)` tree, treating a missing/synthetic-default arg as the empty-string sentinel.
  given FromExprT[CliFunctionParamType] =
    new FromExprT[CliFunctionParamType] {
      override def unapply(x: Expr[CliFunctionParamType])(using Type[CliFunctionParamType], Quotes): Option[CliFunctionParamType] = {
        import quotes.reflect.*
        val term: Term = x.asTerm.underlyingArgument
        def strArgs: List[Option[String]] = term match
          case Apply(_, args) =>
            args.map {
              case Literal(StringConstant(s)) => Some(s)
              case _                          => None
            }
          case _ => Nil
        def arg(i: Int): String = strArgs.lift(i).flatten.getOrElse("")
        term.tpe.typeSymbol.name match
          case "positional" => positional().some
          case "named"      => named().some
          case "flag"       => flag().some
          case "toggle"     => toggle().some
          case "custom"     => custom().some
          case "envVar"     => envVar(arg(0)).some
          case "envConfig"  => envConfig(arg(0), arg(1)).some
          case _            => None
      }
    }
}

final case class custom() extends CliFunctionParamType
final case class positional() extends CliFunctionParamType.nonCustom
final case class named() extends CliFunctionParamType.nonCustom
final case class flag() extends CliFunctionParamType.nonCustom
final case class toggle() extends CliFunctionParamType.nonCustom

// Environment / config sources (see cli-decisions.md, D3/D6):
//   @envVar                     -> read env var, name auto-derived (SCREAMING_SNAKE_CASE) from the param name
//   @envVar("NAME")             -> read env var "NAME", decode its raw value directly
//   @envConfig("NAME")          -> "NAME" holds a path/value, resolved + decoded as config
//   @envConfig("NAME", "path")  -> same, falling back to "path" when "NAME" is unset
// Sentinel: empty string means "auto-derive name" (envVar) / "no default path" (envConfig).
final case class envVar(name: String = "") extends CliFunctionParamType.nonCustom
final case class envConfig(env: String, defaultPath: String = "") extends CliFunctionParamType.nonCustom

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Tweaks
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class longName(name: String) extends scala.annotation.Annotation derives FromExprT
object longName {
  final case class truePrefix(prefix: String) extends scala.annotation.Annotation derives FromExprT
  final case class falsePrefix(prefix: String) extends scala.annotation.Annotation derives FromExprT

  final case class trueName(name: String) extends scala.annotation.Annotation derives FromExprT
  final case class falseName(name: String) extends scala.annotation.Annotation derives FromExprT
}

final case class shortName(name: Char) extends shortName.Base
object shortName {
  sealed abstract class Base extends scala.annotation.Annotation derives FromExprT

  final case class none() extends shortName.Base
  final case class auto() extends shortName.Base

  final case class trueName(name: Char) extends scala.annotation.Annotation derives FromExprT
  final case class falseName(name: Char) extends scala.annotation.Annotation derives FromExprT

}

final case class doc(parts: List[String]) extends scala.annotation.Annotation {
  def this(parts: String*) = this(parts.toList)
}
object doc {

  given FromExprT[doc] =
    new FromExprT[doc] {
      override def unapply(x: Expr[doc])(using Type[doc], Quotes): Option[doc] = x match
        case '{ new `doc`((${ Varargs(parts) }: Seq[String])*) } => parts.toList.traverse(_.evalOption).map(doc(_))
        case '{ new `doc`(${ parts }: List[String]) }            => parts.evalOption.map(doc(_))
        case _                                                   => None
    }

}
