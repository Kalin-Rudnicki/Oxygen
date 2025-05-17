package oxygen.meta

import oxygen.core.collection.Contiguous
import oxygen.meta.helpers.*
import oxygen.predef.core.*
import scala.quoted.*

object instances {

  given contiguousToExpr: [A: Type] => (aToExpr: ToExpr[A]) => ToExpr[Contiguous[A]] =
    new ToExpr[Contiguous[A]] {
      override def apply(x: Contiguous[A])(using Quotes): Expr[Contiguous[A]] =
        x.map(aToExpr(_)).flattenExprs
    }

  given contiguousFromExpr: [A: Type] => (aFromExpr: FromExpr[A]) => FromExpr[Contiguous[A]] =
    new FromExpr[Contiguous[A]] {
      override def unapply(x: Expr[Contiguous[A]])(using Quotes): Option[Contiguous[A]] =
        x match {
          case '{ Contiguous[A](${ Varargs[A](exprs) }) } => Contiguous.from(exprs).traverse(aFromExpr.unapply(_))
          case '{ Contiguous.single[A]($expr) }           => aFromExpr.unapply(expr).map(Contiguous.single(_))
          case '{ Contiguous.empty[A] }                   => Contiguous.empty[A].some
          case '{ ($a: Contiguous[A]) ++ ($b: Contiguous[A]) } =>
            for {
              a2 <- unapply(a)
              b2 <- unapply(b)
            } yield a2 ++ b2
          case _ => None
        }
    }

}
