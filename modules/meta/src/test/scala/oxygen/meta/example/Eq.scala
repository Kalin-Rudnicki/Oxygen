package oxygen.meta.example

import oxygen.meta.*
import scala.quoted.*

trait Eq[A] {
  def areEqual(a: A, b: A): Boolean
}
object Eq extends K0.Derivable[Eq] {

  def id[A]: Eq[A] = _ == _

  given Eq[Int] = id
  given Eq[String] = id
  given Eq[Boolean] = id
  given Eq[Double] = id

  @scala.annotation.nowarn
  given [S[a] <: IterableOnce[a], A: {Eq as eq}] => Eq[S[A]] = { (_a, _b) =>
    extension [B](self: S[B])
      def toSeq: Seq[B] = self.asInstanceOf[Matchable] match
        case seq: Seq[B] => seq
        case _           => self.iterator.toSeq

    val a = _a.toSeq
    val b = _b.toSeq

    a.size == b.size &&
    a.zip(b).forall(eq.areEqual(_, _))
  }

  override protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[Eq[A]] =
    g.builders.instanceFromLazyTypeClasses[Eq] { tcs =>

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def areEqual(a: Expr[A], b: Expr[A]): Expr[Boolean] = {
          val fieldExprs: Seq[Expr[Boolean]] =
            g.builders.mapToSeq[Expr[Boolean]] {
              [i] =>
                (field: g.Field[i]) =>
                  import field.given

                  val tc: Expr[Eq[i]] = field.typeClassInstance(tcs)

                  '{ $tc.areEqual(${ field.get(a) }, ${ field.get(b) }) }
            }

          if (fieldExprs.nonEmpty)
            fieldExprs.reduce { (a, b) => '{ $a && $b } }
          else
            Expr(true)
        }

      }

      '{
        new Eq[A] {
          override def areEqual(a: A, b: A): Boolean = ${ macros.areEqual('a, 'b) }
        }
      }
    }

  override protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[Eq[A]] =
    g.builders.instanceFromLazyTypeClasses[Eq] {
      [i <: A] =>
        (pg: k0.ProductGeneric[i]) =>
          import pg.given
          internalDeriveProduct(k0)(pg)
    } { tcs =>

      object macros {

        @scala.annotation.nowarn("msg=unused import")
        def areEqual(a: Expr[A], b: Expr[A]): Expr[Boolean] = {
          (g.MatchBuilder.instance(a) ++ g.MatchBuilder.instance(b)).build[Boolean] {
            [i <: A] => (_: g.Case[i]) => EmptyTuple
          } {
            [i <: A] =>
              // (kase: g.Case[i], a: Expr[i], b: Expr[i]) =>
              (kase: g.Case[i], tup: (Expr[i], Expr[i])) =>
                import kase.given

                val tc: Expr[Eq[i]] = kase.typeClassInstance(tcs)
                val a: Expr[i] = tup._1
                val b: Expr[i] = tup._2

                '{ $tc.areEqual($a, $b) }
          }(Expr(false))
        }

      }

      '{
        new Eq[A] {
          override def areEqual(a: A, b: A): Boolean = ${ macros.areEqual('a, 'b) }
        }
      }
    }

  inline def derived[A]: Eq[A] = ${ derivedImpl[A] }

}
