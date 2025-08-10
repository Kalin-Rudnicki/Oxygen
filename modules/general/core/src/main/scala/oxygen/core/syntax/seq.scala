package oxygen.core.syntax

import oxygen.core.collection.Contiguous
import oxygen.core.typeclass.SeqOps

object seq {

  extension [F[_]: SeqOps as fOps, A](self: F[A]) {

    def newIterator: Iterator[A] =
      fOps.newIterator(self)

    def knownSize: Int = fOps.knownSize(self)

    def size: Int = fOps.size(self)

    def transformTo[G[_]: SeqOps]: G[A] =
      SeqOps.transform(self)
    def into[G[_]: SeqOps]: G[A] =
      SeqOps.transform(self)

    def toContiguous: Contiguous[A] =
      self.transformTo[Contiguous]

    def mapPassLeft[B, C](passInit: B)(f: (B, A) => (B, C)): (F[C], B) = {
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

    def eitherFoldLeft[L, B](zero: B)(f: (B, A) => Either[L, B]): Either[L, B] = {
      var acc: B = zero
      val iter = fOps.newIterator(self)
      while (iter.hasNext) {
        f(acc, iter.next()) match {
          case Right(newAcc) => acc = newAcc
          case Left(value)   => return Left(value)
        }
      }
      Right(acc)
    }

    def intersperse[B >: A](join: B): F[B] = {
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

    def surround[B >: A](start: B, join: B, end: B): F[B] = {
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

    def surround[B >: A](startJoinEnd: B): F[B] =
      surround(startJoinEnd, startJoinEnd, startJoinEnd)

  }

}
