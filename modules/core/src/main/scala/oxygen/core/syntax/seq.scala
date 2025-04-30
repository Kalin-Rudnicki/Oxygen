package oxygen.core.syntax

import oxygen.core.collection.Contiguous
import oxygen.core.typeclass.SeqOps

object seq {

  extension [F[_], A](self: F[A]) {

    def newIterator(using fOps: SeqOps[F]): Iterator[A] =
      fOps.newIterator(self)

    def transformTo[G[_]](using f: SeqOps[F], g: SeqOps[G]): G[A] =
      SeqOps.transform(self)

    def toContiguous(using fOps: SeqOps[F]): Contiguous[A] =
      self.transformTo[Contiguous]

    def mapPassLeft[B, C](passInit: B)(f: (B, A) => (B, C))(using fOps: SeqOps[F]): (F[C], B) = {
      var pass: B = passInit
      val iterator = fOps.newIterator(self)
      val builder = fOps.newBuilder[C]

      iterator.foreach { e =>
        val (tmpPass, c) = f(pass, e)
        pass = tmpPass
        builder.addOne(c)
      }

      (builder.result(), pass)
    }

    def intersperse[B >: A](join: B)(using fOps: SeqOps[F]): F[B] = {
      val builder = fOps.newBuilder[B]
      val iterator = fOps.newIterator(self)

      val size = fOps.knownSize(self)
      if (size > 0)
        builder.sizeHint(size * 2 - 1)

      if (iterator.hasNext) {
        builder.addOne(iterator.next())
        while (iterator.hasNext) {
          builder.addOne(join)
          builder.addOne(iterator.next())
        }
      }

      builder.result()
    }

    def surround[B >: A](start: B, join: B, end: B)(using fOps: SeqOps[F]): F[B] = {
      val builder = fOps.newBuilder[B]
      val iterator = fOps.newIterator(self)

      val size = fOps.knownSize(self)
      if (size > 0)
        builder.sizeHint(size * 2 + 1)
      else
        builder.sizeHint(2)

      builder.addOne(start)
      if (iterator.hasNext) {
        builder.addOne(iterator.next())
        while (iterator.hasNext) {
          builder.addOne(join)
          builder.addOne(iterator.next())
        }
      }
      builder.addOne(end)

      builder.result()
    }

    def surround[B >: A](startJoinEnd: B)(using fOps: SeqOps[F]): F[B] =
      surround(startJoinEnd, startJoinEnd, startJoinEnd)

  }

}
