package oxygen.zio

import oxygen.core.typeclass.SeqOps
import oxygen.json.JsonCodec
import scala.collection.mutable
import zio.{Chunk, FiberId, LogSpan, StackTrace, Trace}

object instances {

  given traceJsonCodec: JsonCodec[Trace] = JsonCodec[String].asInstanceOf[JsonCodec[Trace]]
  given logSpanJsonCodec: JsonCodec[LogSpan] = JsonCodec.derived
  given fiberIdJsonCodec: JsonCodec[FiberId] = JsonCodec.derived
  given stackTraceJsonCodec: JsonCodec[StackTrace] = JsonCodec.derived

  given chunkSeqOps: SeqOps[Chunk] =
    new SeqOps[Chunk] {
      override def newIterator[A](self: Chunk[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Chunk[A]] = Chunk.newBuilder
      override def knownSize[A](self: Chunk[A]): Int = self.knownSize
    }

}
