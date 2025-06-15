package oxygen.meta2

import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*

final class ValDefinitions[F[_], A](
    fTpe: Type[F],
    aTpe: Type[A],
    make: Quotes ?=> Contiguous[(ValDef, Type[?])],
) {

  private given Type[F] = fTpe

  def defineAndUse[B: Type](f: Quotes ?=> Expressions[F, A] => Expr[B])(using quotes: Quotes): Expr[B] = {
    val vals: Contiguous[(ValDef, Type[?])] = make(using quotes)
    val newQuotes: Quotes = vals.lastOption.fold(quotes)(_._1.symbol.asQuotes) // this might be unnecessary
    val expressions: Expressions[F, A] =
      Expressions[F, A](
        fTpe,
        aTpe,
        vals.map { case (v, tpe) =>
          type C
          given cTpe: Type[C] = tpe.asInstanceOf[Type[C]]
          Expressions.Elem(
            cTpe,
            v.valRef.asExprOf[F[C]],
          )
        },
      )
    val res: Expr[B] = f(using newQuotes)(expressions)
    Block.companion.apply(vals.map(_._1).toList, res.toTerm(using newQuotes)).asExprOf[B]
  }

}

final class Expressions[F[_], A](
    fTpe: Type[F],
    aTpe: Type[A],
    expressions: Contiguous[Expressions.Elem[F, ?]],
) {

  private given Type[F] = fTpe

  def at[B: Type](idx: Int)(using Quotes): Expr[F[B]] =
    expressions.at(idx).expr.asExprOf[F[B]]

  def mapK[G[_]: Type as gTpe](f: [b] => Type[b] ?=> Expr[F[b]] => Expr[G[b]]): Expressions[G, A] =
    Expressions[G, A](
      gTpe,
      aTpe,
      expressions.map(elem => elem.mapK(f(_))),
    )

}
object Expressions {

  final case class Elem[F[_], B](
      bTpe: Type[B],
      expr: Expr[F[B]],
  ) {

    def mapK[G[_]](f: Type[B] ?=> Expr[F[B]] => Expr[G[B]]): Elem[G, B] =
      Elem[G, B](bTpe, f(using bTpe)(expr))

  }

}
