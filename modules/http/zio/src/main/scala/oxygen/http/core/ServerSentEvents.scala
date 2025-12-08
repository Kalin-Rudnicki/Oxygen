package oxygen.http.core

import scala.annotation.unchecked.uncheckedVariance
import zio.*
import zio.http.ServerSentEvent
import zio.stream.*

// TODO (KR) : Does it make sense to have a `ServerSentEvents[InitError, StreamError, A]`?
// TODO (KR) : Definitely need to have some representation of `BodyA` in the stream, and an initial `HeadersA` that can come from the headers.
final class ServerSentEvents[+E, +A] private (private[http] val raw: ZIO[Scope, E, UStream[ServerSentEvent[A @uncheckedVariance]]]) {
  // seems not worth it to map back and forth a Covariant version of SSE, just to make the compiler happy, there seems to be no reason the initial SSE type isn't Covariant.

  def toEventStream: Stream[E, ServerSentEvent[A @uncheckedVariance]] =
    ZStream.scoped(raw).flatten

  def toStream: Stream[E, A] =
    toEventStream.map(_.data)

  def handleEvent[R](error: E => URIO[R & Scope, Unit])(success: ServerSentEvent[A @uncheckedVariance] => URIO[R & Scope, Unit]): URIO[R & Scope, Unit] =
    raw.foldZIO(error, _.foreach(success))

  def handle[R](error: E => URIO[R & Scope, Unit])(success: A => URIO[R & Scope, Unit]): URIO[R & Scope, Unit] =
    handleEvent(error) { e => success(e.data) }

}
object ServerSentEvents {

  def succeedEvents[A](stream: => UStream[ServerSentEvent[A]]): ServerSentEvents[Nothing, A] =
    ServerSentEvents { ZIO.succeed { stream } }

  def succeedBasic[A](stream: => UStream[A]): ServerSentEvents[Nothing, A] =
    ServerSentEvents { ZIO.succeed { stream.map(ServerSentEvent(_)) } }

  def succeed[A](stream: => UStream[A]): ServerSentEvents[Nothing, A] =
    succeedBasic(stream)

  def makeEvents[E, A, B](init: => ZIO[Scope, E, A])(stream: A => UStream[ServerSentEvent[B]]): ServerSentEvents[E, B] =
    ServerSentEvents { ZIO.suspendSucceed { init }.flatMap { a => ZIO.succeed { stream(a) } } }

  def makeBasic[E, A, B](init: => ZIO[Scope, E, A])(stream: A => UStream[B]): ServerSentEvents[E, B] =
    ServerSentEvents { ZIO.suspendSucceed { init }.flatMap { a => ZIO.succeed { stream(a) } }.map(_.map(ServerSentEvent(_))) }

  def make[E, A, B](init: => ZIO[Scope, E, A])(stream: A => UStream[B]): ServerSentEvents[E, B] =
    makeBasic(init)(stream)

  def makeRaw[E, A](raw: ZIO[Scope, E, UStream[ServerSentEvent[A]]]): ServerSentEvents[E, A] =
    ServerSentEvents { raw }

}
