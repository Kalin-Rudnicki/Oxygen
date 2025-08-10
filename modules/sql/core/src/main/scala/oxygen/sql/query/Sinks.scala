package oxygen.sql.query

import oxygen.predef.core.*
import zio.*
import zio.stream.*

object Sinks {

  def seq[S[_]: SeqOps as ops, A]: ZSink[Any, Nothing, A, Nothing, S[A]] =
    for {
      builder <- ZSink.succeed(ops.newBuilder[A])
      _ <- ZSink.foreachChunk[Any, Nothing, A] { chunk => ZIO.succeed(builder.addAll(chunk)) }
    } yield builder.result()

}
