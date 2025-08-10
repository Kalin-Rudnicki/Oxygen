package oxygen.zio

import oxygen.core.typeclass.SeqOps
import oxygen.json.JsonCodec
import oxygen.zio.logging.RichLogLevel
import scala.collection.mutable
import zio.{Chunk, FiberId, LogLevel, LogSpan, StackTrace, Trace}

object instances {

  given traceJsonCodec: JsonCodec[Trace] = JsonCodec[String].asInstanceOf[JsonCodec[Trace]]
  given logSpanJsonCodec: JsonCodec[LogSpan] = JsonCodec.derived
  given fiberIdJsonCodec: JsonCodec[FiberId] = JsonCodec.derived
  given stackTraceJsonCodec: JsonCodec[StackTrace] = JsonCodec.derived
  given logLevelJsonCodec: JsonCodec[LogLevel] = JsonCodec[RichLogLevel].transform(_.level, RichLogLevel.fromLogLevel)

  given chunkSeqOps: SeqOps[Chunk] =
    new SeqOps[Chunk] {
      override def newIterator[A](self: Chunk[A]): Iterator[A] = self.iterator
      override def newBuilder[A]: mutable.Builder[A, Chunk[A]] = Chunk.newBuilder
      override def toIterable[A](self: Chunk[A]): Iterable[A] = self
      override def knownSize[A](self: Chunk[A]): Int = self.knownSize
      override def size[A](self: Chunk[A]): Int = self.size
    }

}
