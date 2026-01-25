package oxygen.http.core

import zio.*
import zio.stream.*

// TODO (KR) : Does it make sense to have a `LineStream[InitError, StreamError, A]`?
// TODO (KR) : Definitely need to have some representation of `BodyA` in the stream, and an initial `HeadersA` that can come from the headers.
final class LineStream[+E, +A] private (val raw: ZIO[Scope, E, UStream[A]]) {
  // seems not worth it to map back and forth a Covariant version of SSE, just to make the compiler happy, there seems to be no reason the initial SSE type isn't Covariant.

  def toStream: Stream[E, A] =
    ZStream.scoped(raw).flatten

  def handle[R](error: E => URIO[R & Scope, Unit])(success: A => URIO[R & Scope, Unit]): URIO[R & Scope, Unit] =
    raw.foldZIO(error, _.foreach(success))

}
object LineStream {

  def succeedBasic[A](stream: => UStream[A]): LineStream[Nothing, A] =
    LineStream { ZIO.succeed { stream } }

  def succeed[A](stream: => UStream[A]): LineStream[Nothing, A] =
    succeedBasic(stream)

  def makeBasic[E, A, B](init: => ZIO[Scope, E, A])(stream: A => UStream[B]): LineStream[E, B] =
    LineStream { ZIO.suspendSucceed { init }.flatMap { a => ZIO.succeed { stream(a) } } }

  def make[E, A, B](init: => ZIO[Scope, E, A])(stream: A => UStream[B]): LineStream[E, B] =
    makeBasic(init)(stream)

  def makeRaw[E, A](raw: ZIO[Scope, E, UStream[A]]): LineStream[E, A] =
    LineStream { raw }

}
