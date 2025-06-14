package oxygen.core.syntax

import oxygen.core.syntax.seq.*
import oxygen.core.typeclass.SeqOps

object seqExtra {

  extension [F[_]: SeqOps as fOps, A](self: F[A]) {

    def foreach(f: A => Any): Unit =
      self.newIterator.foreach(f)

  }

  extension [F[_]: SeqOps as fOps, G[_]: SeqOps as gOps, A](self: F[G[A]]) {

    def flatten: F[A] = {
      val builder = fOps.newBuilder[A]
      self.foreach { gOps.addToBuilder(_)(builder) }
      builder.result()
    }

  }

}
