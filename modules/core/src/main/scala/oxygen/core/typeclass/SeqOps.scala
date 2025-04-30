package oxygen.core.typeclass

import oxygen.core.collection.*
import oxygen.core.collection.Contiguous.typedAnyTag
import scala.collection.mutable

trait SeqOps[F[_]] {

  def newIterator[A](self: F[A]): Iterator[A]

  def newBuilder[A]: mutable.Builder[A, F[A]]

  def knownSize[A](self: F[A]): Int

  final def fromIterableOnce[A](i: IterableOnce[A]): F[A] = {
    val builder = newBuilder[A]
    builder.sizeHint(i.knownSize)
    builder.addAll(i)
    builder.result()
  }

}
object SeqOps {

  def transform[F[_], G[_], A](self: F[A])(using f: SeqOps[F], g: SeqOps[G]): G[A] = {
    val iter = f.newIterator(self)
    val builder = g.newBuilder[A]
    builder.sizeHint(f.knownSize(self))
    while (iter.hasNext)
      builder.addOne(iter.next())
    builder.result()
  }

  given contiguous: SeqOps[Contiguous] =
    new SeqOps[Contiguous] {
      override def newIterator[A](self: Contiguous[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Contiguous[A]] = Contiguous.newBuilder
      override def knownSize[A](self: Contiguous[A]): Int = self.length
    }

  given seq: SeqOps[Seq] =
    new SeqOps[Seq] {
      override def newIterator[A](self: Seq[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
      override def knownSize[A](self: Seq[A]): Int = self.knownSize
    }

  given indexedSeq: SeqOps[IndexedSeq] =
    new SeqOps[IndexedSeq] {
      override def newIterator[A](self: IndexedSeq[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, IndexedSeq[A]] = IndexedSeq.newBuilder[A]
      override def knownSize[A](self: IndexedSeq[A]): Int = self.knownSize
    }

  given iterableOnce: SeqOps[IterableOnce] =
    new SeqOps[IterableOnce] {
      override def newIterator[A](self: IterableOnce[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, IterableOnce[A]] = Vector.newBuilder[A]
      override def knownSize[A](self: IterableOnce[A]): Int = self.knownSize
    }

  given iterable: SeqOps[Iterable] =
    new SeqOps[Iterable] {
      override def newIterator[A](self: Iterable[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Iterable[A]] = Iterable.newBuilder[A]
      override def knownSize[A](self: Iterable[A]): Int = self.knownSize
    }

  given array: SeqOps[Array] =
    new SeqOps[Array] {
      override def newIterator[A](self: Array[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Array[A]] = Array.newBuilder
      override def knownSize[A](self: Array[A]): Int = self.length
    }

  given iArray: SeqOps[IArray] =
    new SeqOps[IArray] {
      override def newIterator[A](self: IArray[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, IArray[A]] = IArray.newBuilder
      override def knownSize[A](self: IArray[A]): Int = self.length
    }

  given list: SeqOps[List] =
    new SeqOps[List] {
      override def newIterator[A](self: List[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, List[A]] = List.newBuilder
      override def knownSize[A](self: List[A]): Int = self.length
    }

  given vector: SeqOps[Vector] =
    new SeqOps[Vector] {
      override def newIterator[A](self: Vector[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Vector[A]] = Vector.newBuilder
      override def knownSize[A](self: Vector[A]): Int = self.length
    }

}
