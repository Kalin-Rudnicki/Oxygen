package oxygen.sql.query

import oxygen.predef.core.*
import scala.collection.mutable
import scala.reflect.ClassTag
import zio.*
import zio.stream.*

object Sinks {

  private def simpleBuilder[S[_], A](builder: => mutable.Builder[A, S[A]]): ZSink[Any, Nothing, A, Nothing, S[A]] =
    for {
      builder <- ZSink.succeed { builder }
      _ <- ZSink.foreachChunk[Any, Nothing, A] { chunk => ZIO.succeed(builder.addAll(chunk)) }
    } yield builder.result()

  def seq[S[_]: SeqOps as ops, A]: ZSink[Any, Nothing, A, Nothing, S[A]] = simpleBuilder { ops.newBuilder[A] }
  def arraySeq[A: ClassTag]: ZSink[Any, Nothing, A, Nothing, ArraySeq[A]] = simpleBuilder { ArraySeq.newBuilder[A] }

  def option[E, A](onMany: => E): ZSink[Any, E, A, Nothing, Option[A]] =
    ZSink.foldLeftZIO[Any, E, A, Option[A]](None) {
      case (None, a)    => ZIO.some(a)
      case (Some(_), _) => ZIO.fail(onMany)
    }

  // TODO (KR) : have a version of this builder where all chunks are read, a known size is calculated, and only a single array is created.
  //           : realistically, it might be worth going all the way down to the level of the ZChannel.
  //           : You might be able to glean efficiency benefits at that level if you knew you wanted to read either:
  //           : 0/1, many (non-streamed, aka: _.to[Seq]), many (streamed).

}
