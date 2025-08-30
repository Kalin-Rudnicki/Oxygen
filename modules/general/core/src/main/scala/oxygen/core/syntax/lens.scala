package oxygen.core.syntax

import oxygen.core.typeclass.Lens

object lens {

  extension [A](self: A) {

    inline def focus[B](inline f: A => B): Lens.Applied[A, B] = Lens.deriveLens(f)(self)

  }

}
