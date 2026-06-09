package oxygen.executable

import oxygen.meta.FromExprT
import oxygen.predef.core.*
import scala.quoted.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      CliFunctionAnnotation
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class CliFunctionAnnotation extends scala.annotation.Annotation
object CliFunctionAnnotation {

  given FromExprT[CliFunctionAnnotation] =
    new FromExprT[CliFunctionAnnotation] {
      override def unapply(x: Expr[CliFunctionAnnotation])(using Type[CliFunctionAnnotation], Quotes): Option[CliFunctionAnnotation] = x match
        case '{ new `command`(${ Expr(name) }) } => command(name).some
        case '{ new `command`() }                => command().some
        case '{ new `execute`() }                => execute().some
        case '{ new `inlineApp`() }              => inlineApp().some
        case _                                   => None
    }

}

final case class command(name: String = "") extends CliFunctionAnnotation
final case class execute() extends CliFunctionAnnotation
final case class inlineApp() extends CliFunctionAnnotation
