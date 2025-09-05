package oxygen.core.typeclass

import oxygen.core.collection.NonEmptyList
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

trait SeqRead[F[_]] {

  def newIterator[A](self: F[A]): Iterator[A]

  def knownSize[A](self: F[A]): Int

  def size[A](self: F[A]): Int

  def toIterable[A](self: F[A]): Iterable[A]

  def addToBuilder[G[_], A, B >: A](self: F[A])(builder: mutable.Builder[B, G[B]]): Unit = builder.addAll(toIterable(self))

  final def nestedKnownSize[A](self: F[F[A]]): Int = {
    var total: Int = 0
    val iterator = newIterator(self)

    while (iterator.hasNext)
      knownSize(iterator.next()) match {
        case -1 => return -1
        case sz => total += sz
      }

    total
  }

}
object SeqRead extends SeqReadLowPriority.LowPriority1 {

  given option: SeqRead[Option] =
    new SeqRead[Option] {
      override def newIterator[A](self: Option[A]): Iterator[A] = self.iterator
      override def toIterable[A](self: Option[A]): Iterable[A] = self
      override def knownSize[A](self: Option[A]): Int = self.knownSize
      override def size[A](self: Option[A]): Int = self.size
    }

  given arraySeq: SeqRead[ArraySeq] =
    new SeqRead[ArraySeq] {
      override def newIterator[A](self: ArraySeq[A]): Iterator[A] = self.iterator
      override def toIterable[A](self: ArraySeq[A]): Iterable[A] = self
      override def knownSize[A](self: ArraySeq[A]): Int = self.knownSize
      override def size[A](self: ArraySeq[A]): Int = self.size
    }

  given nonEmptyList: SeqRead[NonEmptyList] =
    new SeqRead[NonEmptyList] {
      override def newIterator[A](self: NonEmptyList[A]): Iterator[A] = self.iterator
      override def toIterable[A](self: NonEmptyList[A]): Iterable[A] = self.toList
      override def knownSize[A](self: NonEmptyList[A]): Int = self.toList.knownSize
      override def size[A](self: NonEmptyList[A]): Int = self.size
    }

  given array: SeqRead[Array] =
    new SeqRead[Array] {
      override def newIterator[A](self: Array[A]): Iterator[A] = self.iterator
      override def toIterable[A](self: Array[A]): Iterable[A] = self
      override def knownSize[A](self: Array[A]): Int = self.length
      override def size[A](self: Array[A]): Int = self.length
    }

  given iArray: SeqRead[IArray] =
    new SeqRead[IArray] {
      override def newIterator[A](self: IArray[A]): Iterator[A] = self.iterator
      override def toIterable[A](self: IArray[A]): Iterable[A] = self
      override def knownSize[A](self: IArray[A]): Int = self.length
      override def size[A](self: IArray[A]): Int = self.length
    }

}

object SeqReadLowPriority {

  trait LowPriority1 {

    given fromSeqOps: [S[_]: SeqOps as seqOps] => SeqRead[S] = seqOps

  }

}
