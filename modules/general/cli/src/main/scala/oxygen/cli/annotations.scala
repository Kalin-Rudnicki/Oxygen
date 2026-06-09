package oxygen.cli

import oxygen.meta.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      CliFunctionParamType
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class CliFunctionParamType extends scala.annotation.Annotation derives FromExprT
object CliFunctionParamType {
  sealed abstract class nonCustom extends CliFunctionParamType
}

final case class custom() extends CliFunctionParamType
final case class positional() extends CliFunctionParamType.nonCustom
final case class named() extends CliFunctionParamType.nonCustom
final case class flag() extends CliFunctionParamType.nonCustom
final case class toggle() extends CliFunctionParamType.nonCustom
final case class config(env: String) extends CliFunctionParamType.nonCustom

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
