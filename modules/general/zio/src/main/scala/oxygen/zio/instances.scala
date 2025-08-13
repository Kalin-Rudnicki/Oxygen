package oxygen.zio

import oxygen.core.typeclass.SeqOps
import oxygen.json.JsonCodec
import oxygen.meta.ExprMonad
import oxygen.zio.logging.RichLogLevel
import scala.collection.mutable
import scala.quoted.*
import scala.reflect.ClassTag
import zio.{Chunk, FiberId, LogLevel, LogSpan, StackTrace, Trace, ZIO}
import zio.stream.ZStream

object instances {

  given traceClassTag: ClassTag[Trace] = summon[ClassTag[String]].asInstanceOf[ClassTag[Trace]]

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

  type ProjectZIO[R, E] = [A] =>> ZIO[R, E, A]
  type ProjectStream[R, E] = [A] =>> ZStream[R, E, A]

  given zioExprMonad: [R: Type, E: Type] => ExprMonad[ProjectZIO[R, E]] =
    new ExprMonad[ProjectZIO[R, E]] {

      override def map[A, B](
          a: Expr[ZIO[R, E, A]],
      )(
          f: Expr[A => B],
      )(using quotes: Quotes, fType: Type[ProjectZIO[R, E]], aType: Type[A], bType: Type[B]): Expr[ZIO[R, E, B]] =
        '{ $a.map($f) }

      override def pure[A](
          a: Expr[A],
      )(using quotes: Quotes, fType: Type[ProjectZIO[R, E]], aType: Type[A]): Expr[ZIO[R, E, A]] =
        '{ ZIO.succeed($a) }

      override def flatMap[A, B](
          a: Expr[ZIO[R, E, A]],
      )(
          f: Expr[A => ZIO[R, E, B]],
      )(using quotes: Quotes, fType: Type[ProjectZIO[R, E]], aType: Type[A], bType: Type[B]): Expr[ZIO[R, E, B]] =
        '{ $a.flatMap($f) }

    }

  given zstreamExprMonad: [R: Type, E: Type] => ExprMonad[ProjectStream[R, E]] =
    new ExprMonad[ProjectStream[R, E]] {

      override def map[A, B](
          a: Expr[ZStream[R, E, A]],
      )(
          f: Expr[A => B],
      )(using quotes: Quotes, fType: Type[ProjectStream[R, E]], aType: Type[A], bType: Type[B]): Expr[ZStream[R, E, B]] =
        '{ $a.map($f) }

      override def pure[A](
          a: Expr[A],
      )(using quotes: Quotes, fType: Type[ProjectStream[R, E]], aType: Type[A]): Expr[ZStream[R, E, A]] =
        '{ ZStream.succeed($a) }

      override def flatMap[A, B](
          a: Expr[ZStream[R, E, A]],
      )(
          f: Expr[A => ZStream[R, E, B]],
      )(using quotes: Quotes, fType: Type[ProjectStream[R, E]], aType: Type[A], bType: Type[B]): Expr[ZStream[R, E, B]] =
        '{ $a.flatMap($f) }

    }

}
