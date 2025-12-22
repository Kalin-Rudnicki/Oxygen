package oxygen.meta.k0

import scala.collection.immutable.ArraySeq
import scala.quoted.*

// TODO (KR) : Have a concept like `Expressions`, but without F being restricted to `[b] =>> Expr[F[b]]`
/**
  * Represents a set of expressions, which are accessible using [[Entity.Child.getExpr]].
  */
final class Expressions[F[_], A](
    private[k0] val fTpe: Type[F],
    private[k0] val aTpe: Type[A],
    private[k0] val expressions: ArraySeq[Expressions.Elem[F, ?]],
) {

  private given Type[F] = fTpe

  def at[B: Type](idx: Int)(using Quotes): Expr[F[B]] =
    expressions(idx).expr.asExprOf[F[B]]

  def mapK[G[_]: Type as gTpe](f: [b] => Type[b] ?=> Expr[F[b]] => Expr[G[b]]): Expressions[G, A] =
    Expressions[G, A](
      gTpe,
      aTpe,
      expressions.map(elem => elem.mapK(f(_))),
    )

}
object Expressions {

  final class Elem[F[_], B](
      val bTpe: Type[B],
      ctxExpr: Quotes ?=> Expr[F[B]],
  ) {

    def expr(using q: Quotes): Expr[F[B]] = ctxExpr(using q)

    def mapK[G[_]](f: Type[B] ?=> Expr[F[B]] => Expr[G[B]]): Elem[G, B] =
      Elem[G, B](bTpe, f(using bTpe)(expr))

  }

}
