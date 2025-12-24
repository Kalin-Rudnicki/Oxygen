package oxygen.quoted

import scala.quoted.*

class Annotations(
    _all: => List[Term],
    target: => String,
)(using Quotes) {

  lazy val all: List[Term] = _all

  def allOf[Annot: Type]: List[Expr[Annot]] =
    all.flatMap { tree => // TODO (KR) : now that `<:<` is fixed, use that, no try/catch?
      try { Some(tree.asExprOf[Annot]) }
      catch { case _ => None }
    }

  def optionalOf[Annot: Type]: Option[Expr[Annot]] =
    allOf[Annot] match
      case annot :: Nil => Some(annot)
      case Nil          => None
      case _ => report.errorAndAbort(s"Found multiple annotations of type ${TypeRepr.of[Annot].show} for `$target`. If you are OK with this, use `allOf[${TypeRepr.of[Annot].show}].headOption`.")

  def requiredOf[Annot: Type]: Expr[Annot] =
    optionalOf[Annot].getOrElse(report.errorAndAbort(s"Missing required annotation `${TypeRepr.of[Annot].show}` for `$target`"))

  def allOfValue[Annot: {Type, FromExpr}]: List[Annot] =
    allOf[Annot].map { expr =>
      expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$target`, but is unable to extract Expr.value\n\n${expr.show}"))
    }

  def optionalOfValue[Annot: {Type, FromExpr}]: Option[Annot] =
    optionalOf[Annot].map { expr =>
      expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$target`, but is unable to extract Expr.value\n\n${expr.show}"))
    }

  def requiredOfValue[Annot: {Type, FromExpr}]: Annot = {
    val expr = requiredOf[Annot]
    expr.value.getOrElse(report.errorAndAbort(s"Found annotation `${TypeRepr.of[Annot].show}` for `$target`, but is unable to extract Expr.value\n\n${expr.show}"))
  }

}

final class AnnotationsTyped[A](
    all: => List[Term],
    target: => String,
)(using Quotes, Type[A])
    extends Annotations(all, target) {

  def allOfT[Annot[_]: Type]: List[Expr[Annot[A]]] = allOf[Annot[A]]
  def optionalOfT[Annot[_]: Type]: Option[Expr[Annot[A]]] = optionalOf[Annot[A]]
  def requiredOfT[Annot[_]: Type]: Expr[Annot[A]] = requiredOf[Annot[A]]

  def allOfTValue[Annot[_]: Type](using FromExpr[Annot[A]]): List[Annot[A]] = allOfValue[Annot[A]]
  def optionalOfTValue[Annot[_]: Type](using FromExpr[Annot[A]]): Option[Annot[A]] = optionalOfValue[Annot[A]]
  def requiredOfTValue[Annot[_]: Type](using FromExpr[Annot[A]]): Annot[A] = requiredOfValue[Annot[A]]

}
