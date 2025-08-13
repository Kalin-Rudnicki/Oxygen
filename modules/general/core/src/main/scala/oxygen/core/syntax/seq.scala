package oxygen.core.syntax

import oxygen.core.collection.Growable
import oxygen.core.typeclass.{SeqOps, SeqRead, SeqWrite}
import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

object seq {

  extension [A](self: ArraySeq[A]) {

    def zipExact[B](that: ArraySeq[B]): ArraySeq[(A, B)] = {
      if (self.length != that.length)
        throw new RuntimeException(s"seqs do not have same length (${self.length} != ${that.length})")
      self.zip(that)
    }

    def zipWithIndexFrom(i: Int): ArraySeq[(A, Int)] = {
      var idx: Int = i
      self.map { a =>
        val i2: Int = idx
        idx = i2 + 1
        (a, i2)
      }
    }

    def zipUsing[B, C: ClassTag](that: ArraySeq[B])(leftOnly: A => C, rightOnly: B => C, both: (A, B) => C): ArraySeq[C] = {
      var idx: Int = 0

      val minLength = self.length min that.length
      val newArray: Array[C] = new Array[C](self.length max that.length)

      while (idx < minLength) {
        newArray(idx) = both(self(idx), that(idx))
        idx = idx + 1
      }
      while (idx < self.length) {
        newArray(idx) = leftOnly(self(idx))
        idx = idx + 1
      }
      while (idx < that.length) {
        newArray(idx) = rightOnly(that(idx))
        idx = idx + 1
      }

      ArraySeq.unsafeWrapArray(newArray)
    }

  }

  extension [F[_]: SeqRead as fOps, A](self: F[A]) {

    def toGrowable: Growable[A] = Growable.many(self)

    def newIterator: Iterator[A] =
      fOps.newIterator(self)

    def knownSize: Int = fOps.knownSize(self)

    def size: Int = fOps.size(self)

    def transformTo[G[_]: SeqWrite]: G[A] = SeqOps.transform(self)
    def into[G[_]: SeqWrite]: G[A] = SeqOps.transform(self)

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

    def toArraySeq(using ClassTag[A]): ArraySeq[A] =
      ArraySeq.from(self.newIterator)

  }

  extension [F[_]: SeqOps as fOps, A](self: F[A]) {

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
