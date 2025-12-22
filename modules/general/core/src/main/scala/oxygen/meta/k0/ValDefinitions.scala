package oxygen.meta.k0

import scala.quoted.*

final class ValDefinitions[F[_], A](
    make: [b] => (Type[b], Quotes) ?=> (Expressions[F, A] => Expr[b]) => Expr[b],
) {

  def defineAndUse[B: Type](f: Expressions[F, A] => Expr[B])(using quotes: Quotes): Expr[B] =
    make(f)

}

final class ValDefinitionsWith[F[_], A, With](
    make: [b] => (Type[b], Quotes) ?=> ((Expressions[F, A], Expr[With]) => Expr[b]) => Expr[b],
) {

  def defineAndUse[B: Type](f: (Expressions[F, A], Expr[With]) => Expr[B])(using quotes: Quotes): Expr[B] =
    make(f)

}
