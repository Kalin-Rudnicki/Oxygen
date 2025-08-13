package oxygen.core.typeclass

import oxygen.core.collection.*
import scala.collection.mutable

trait SeqOps[F[_]] extends SeqRead[F], SeqWrite[F] {

  /**
    * Will attempt to calculate the known size of all children, and pass it to the created builder.
    * If the size of any child is not known, do a repetitive add all, without a hint.
    * This trades off an extra traversal for potentially making a builder re-size multiple times.
    */
  final def flattenAttemptKnownSize[A](self: F[F[A]]): F[A] =
    flattenWithSizeHint(self, nestedKnownSize(self))

  /**
    * Adds all children to a builder, with no attempt at a size hint.
    */
  final def flattenSimple[A](self: F[F[A]]): F[A] =
    flattenWithSizeHint(self, -1)

  final def flattenWithSizeHint[A](self: F[F[A]], sizeHint: Int): F[A] = {
    val iterator = newIterator(self)
    val builder = newBuilder[A]

    if (sizeHint != -1)
      builder.sizeHint(sizeHint)

    while (iterator.hasNext)
      builder.addAll(newIterator(iterator.next()))

    builder.result()
  }

}
object SeqOps extends SeqOpsLowPriority.LowPriority1 {

  def transform[F[_], G[_], A](self: F[A])(using f: SeqRead[F], g: SeqWrite[G]): G[A] =
    if (f eq g)
      self.asInstanceOf[G[A]]
    else {
      val iter = f.newIterator(self)
      val builder = g.newBuilder[A]
      builder.sizeHint(f.knownSize(self))
      while (iter.hasNext)
        builder.addOne(iter.next())
      builder.result()
    }

  given growable: SeqOps[Growable] =
    new SeqOps[Growable] {
      override def newIterator[A](self: Growable[A]): Iterator[A] = {
        val builder = Iterator.newBuilder[A]
        self.foreach(builder.addOne)
        builder.result()
      }
      override def newBuilder[A]: mutable.Builder[A, Growable[A]] = Vector.newBuilder[A].mapResult(Growable.many)
      override def addToBuilder[G[_], A, B >: A](self: Growable[A])(builder: mutable.Builder[B, G[B]]): Unit = self.addTo(builder)
      override def toIterable[A](self: Growable[A]): Iterable[A] = self.to[Iterable]
      override def knownSize[A](self: Growable[A]): Int = -1
      override def size[A](self: Growable[A]): Int = self.size
    }

  given list: SeqOps[List] =
    new SeqOps[List] {
      override def newIterator[A](self: List[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, List[A]] = List.newBuilder
      override def toIterable[A](self: List[A]): Iterable[A] = self
      override def knownSize[A](self: List[A]): Int = self.knownSize
      override def size[A](self: List[A]): Int = self.size
    }

  given vector: SeqOps[Vector] =
    new SeqOps[Vector] {
      override def newIterator[A](self: Vector[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Vector[A]] = Vector.newBuilder
      override def toIterable[A](self: Vector[A]): Iterable[A] = self
      override def knownSize[A](self: Vector[A]): Int = self.length
      override def size[A](self: Vector[A]): Int = self.length
    }

  given set: SeqOps[Set] =
    new SeqOps[Set] {
      override def newIterator[A](self: Set[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Set[A]] = Set.newBuilder
      override def toIterable[A](self: Set[A]): Iterable[A] = self
      override def knownSize[A](self: Set[A]): Int = self.knownSize
      override def size[A](self: Set[A]): Int = self.size
    }

}

object SeqOpsLowPriority {

  trait LowPriority1 extends LowPriority2 {

    given indexedSeq: SeqOps[IndexedSeq] =
      new SeqOps[IndexedSeq] {
        override def newIterator[A](self: IndexedSeq[A]): Iterator[A] = self.iterator
        override def newBuilder[A]: mutable.Builder[A, IndexedSeq[A]] = IndexedSeq.newBuilder[A]
        override def toIterable[A](self: IndexedSeq[A]): Iterable[A] = self
        override def knownSize[A](self: IndexedSeq[A]): Int = self.knownSize
        override def size[A](self: IndexedSeq[A]): Int = self.size
      }

  }

  trait LowPriority2 extends LowPriority3 {

    given seq: SeqOps[Seq] =
      new SeqOps[Seq] {
        override def newIterator[A](self: Seq[A]): Iterator[A] = self.iterator
        override def newBuilder[A]: mutable.Builder[A, Seq[A]] = Seq.newBuilder[A]
        override def toIterable[A](self: Seq[A]): Iterable[A] = self
        override def knownSize[A](self: Seq[A]): Int = self.knownSize
        override def size[A](self: Seq[A]): Int = self.size
      }

  }

  trait LowPriority3 {

    given iterable: SeqOps[Iterable] =
      new SeqOps[Iterable] {
        override def newIterator[A](self: Iterable[A]): Iterator[A] = self.iterator
        override def newBuilder[A]: mutable.Builder[A, Iterable[A]] = Iterable.newBuilder[A]
        override def toIterable[A](self: Iterable[A]): Iterable[A] = self
        override def knownSize[A](self: Iterable[A]): Int = self.knownSize
        override def size[A](self: Iterable[A]): Int = self.size
      }

  }

}
