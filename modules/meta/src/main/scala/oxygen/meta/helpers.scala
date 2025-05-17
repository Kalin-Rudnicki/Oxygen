package oxygen.meta

import oxygen.core.collection.Contiguous
import scala.quoted.*

object helpers {

  extension [A](self: Contiguous[Expr[A]])
    def flattenExprs(using Quotes, Type[A]): Expr[Contiguous[A]] =
      '{ Contiguous[A](${ Varargs(self.toSeq) }*) }

}
