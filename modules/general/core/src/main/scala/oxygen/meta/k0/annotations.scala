package oxygen.meta.k0

import oxygen.core.syntax.option.*
import oxygen.meta.{given, *}
import oxygen.quoted.*
import scala.annotation.Annotation
import scala.quoted.*

final class showAllDerivation extends Annotation

final class showDerivation[F[_]] extends Annotation

final case class overrideAllUnrollStrategy(strategy: SumGeneric.UnrollStrategy) extends Annotation
object overrideAllUnrollStrategy {

  given FromExprT[overrideAllUnrollStrategy] =
    new FromExprT[overrideAllUnrollStrategy] {
      override def unapply(x: Expr[overrideAllUnrollStrategy])(using Type[overrideAllUnrollStrategy], Quotes): Option[overrideAllUnrollStrategy] = x match
        case '{ new `overrideAllUnrollStrategy`(${ Expr(strategy) }) } => new overrideAllUnrollStrategy(strategy).some
        case _                                                         => None
    }

}

final case class overrideUnrollStrategy[T](strategy: SumGeneric.UnrollStrategy) extends Annotation
object overrideUnrollStrategy {

  given [T] => FromExprT[overrideUnrollStrategy[T]] =
    new FromExprT[overrideUnrollStrategy[T]] {
      override def unapply(x: Expr[overrideUnrollStrategy[T]])(using Type[overrideUnrollStrategy[T]], Quotes): Option[overrideUnrollStrategy[T]] = x match
        case '{ new `overrideUnrollStrategy`[t](${ Expr(strategy) }) } => new overrideUnrollStrategy[T](strategy).some
        case _                                                         => None
    }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Helpers
//////////////////////////////////////////////////////////////////////////////////////////////////////

object annotationUtil {

  def specificPos[F[_]: Type](a: Annotations)(using Quotes): Option[Position] = a.optionalOf[showDerivation[F]].map(_.toTerm.pos)
  def generalPos(a: Annotations)(using Quotes): Option[Position] = a.optionalOf[showAllDerivation].map(_.toTerm.pos)

  def showSpecific[F[_]: Type, T: Type](a: Annotations, expr: Expr[?])(using Quotes): Unit =
    annotationUtil.specificPos[F](a).orElse(generalPos(a)).foreach { showPos =>
      report.info(s"derivation for ${TypeRepr.of[T].showAnsiCode}:\n\n${expr.showAnsiCode}", showPos)
    }

  def showGeneral[T: Type](a: Annotations, expr: Expr[?])(using Quotes): Unit =
    annotationUtil.generalPos(a).foreach { showPos =>
      report.info(s"derivation for ${TypeRepr.of[T].showAnsiCode}:\n\n${expr.showAnsiCode}", showPos)
    }

}
