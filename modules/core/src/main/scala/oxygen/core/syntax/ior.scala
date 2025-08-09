package oxygen.core.syntax

import oxygen.core.Ior

object ior {

  extension [A](self: A) {

    inline def asIorRight[B]: Ior.Distinct[B, A] = Ior.Right(self)

    inline def asIorLeft[B]: Ior.Distinct[A, B] = Ior.Left(self)

  }

}
