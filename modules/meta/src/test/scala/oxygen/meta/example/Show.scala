package oxygen.meta.example

import oxygen.core.syntax.seq.*
import oxygen.meta.*
import oxygen.predef.core.*
import scala.collection.mutable
import scala.quoted.*

trait Show[A] {
  def show(a: A): String
  def appendToStringBuilder(sb: mutable.StringBuilder, a: A): Unit = sb.append(show(a))
}
object Show extends K0.Derivable[Show] {

  trait PrioritizeStringBuilder[A] extends Show[A] {
    override final def show(a: A): String = {
      val sb = mutable.StringBuilder()
      appendToStringBuilder(sb, a)
      sb.toString
    }
    override def appendToStringBuilder(sb: mutable.StringBuilder, a: A): Unit
  }

  given Show[Int] = _.toString
  given Show[String] = _.unesc
  given Show[Boolean] = _.toString

  override protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[Show[A]] =
    g.builders.instanceFromLazyTypeClasses[Show] { tcs =>
      import k0.meta.*

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def appendToStringBuilder(sb: Expr[mutable.StringBuilder], a: Expr[A]): Expr[Unit] =
          Expr.unitBlock(
            g.builders
              .mapToSeq[Seq[
                Expr[Any],
              ]] {
                [i] =>
                  (field: g.Field[i]) =>
                    import field.given

                    val tc: Expr[Show[i]] = field.getExpr(tcs)
                    val value: Expr[i] = field.get(a)

                    val fieldRename: Option[Expr[fieldName]] = field.optionalAnnotation[fieldName]
                    val constFieldRename: Option[String] = fieldRename.flatMap(_.value).map(_.name)

                    (fieldRename, constFieldRename) match {
                      case (_, Some(constFieldRename)) =>
                        Seq[Expr[Any]](
                          '{ $sb.append(${ Expr(constFieldRename + " = ") }) },
                          '{ $tc.appendToStringBuilder($sb, $value) },
                        )
                      case (Some(fieldRename), _) =>
                        Seq[Expr[Any]](
                          '{ $sb.append($fieldRename.name) },
                          '{ $sb.append(" = ") },
                          '{ $tc.appendToStringBuilder($sb, $value) },
                        )
                      case _ =>
                        Seq[Expr[Any]](
                          '{ $sb.append(${ Expr(field.name + " = ") }) },
                          '{ $tc.appendToStringBuilder($sb, $value) },
                        )
                  }
              }
              .surround(
                '{ $sb.append("{ ") } :: Nil,
                '{ $sb.append(", ") } :: Nil,
                '{ $sb.append(" }") } :: Nil,
              )
              .flatten
              .toList,
          )

      }

      if (g.fields.isEmpty)
        '{
          new Show[A] {
            override def show(a: A): String = "{}"
          }
        }
      else
        '{
          new Show.PrioritizeStringBuilder[A] {

            override def appendToStringBuilder(sb: mutable.StringBuilder, a: A): Unit =
              ${ macros.appendToStringBuilder('sb, 'a) }

          }
        }
    }

  override protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[Show[A]] =
    g.builders.instanceFromLazyTypeClasses[Show] {
      [i <: A] =>
        (pg: k0.ProductGeneric[i]) =>
          import pg.given
          internalDeriveProduct(k0)(pg)
    } { tcs =>

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def appendToStringBuilder(sb: Expr[mutable.StringBuilder], a: Expr[A]): Expr[Unit] =
          g.builders.matchOnInstance[Unit](a) {
            [i <: A] =>
              (kase: g.Case[i], value: Expr[i]) =>
                import kase.given

                val tc: Expr[Show[i]] = kase.getExpr(tcs)

                '{
                  $sb.append(${ Expr(kase.name + ": ") })
                  $tc.appendToStringBuilder($sb, $value)
              }
          }

      }

      '{
        new Show.PrioritizeStringBuilder[A] {

          override def appendToStringBuilder(sb: StringBuilder, a: A): Unit = ${ macros.appendToStringBuilder('sb, 'a) }

        }
      }
    }

  inline def derived[A]: Show[A] = ${ derivedImpl[A] }

}
