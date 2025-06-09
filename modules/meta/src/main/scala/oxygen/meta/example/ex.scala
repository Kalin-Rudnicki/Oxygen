package oxygen.meta.example

import scala.quoted.*

final class K0[Q <: Quotes](using val quotes: Q) {
  import quotes.reflect.*

  trait ProductGeneric[A] {

    val fields: Seq[Field[?]]

    final case class Field[I](
        idx: Int,
        symRepr: Symbol,
        constructorSymRepr: Symbol,
        typeRepr: TypeRepr,
        tpe: Type[I],
        valDef: ValDef,
        get: Expr[A] => Expr[I],
    )

  }

}
