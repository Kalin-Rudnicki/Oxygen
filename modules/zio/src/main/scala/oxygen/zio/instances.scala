package oxygen.zio

import oxygen.core.typeclass.SeqOps
import scala.collection.mutable
import zio.Chunk

object instances {

  given chunkSeqOps: SeqOps[Chunk] =
    new SeqOps[Chunk] {
      override def newIterator[A](self: Chunk[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Chunk[A]] = Chunk.newBuilder
      override def knownSize[A](self: Chunk[A]): Int = self.knownSize
    }

}
