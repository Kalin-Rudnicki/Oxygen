package oxygen.ui.web

import oxygen.predef.core.*
import zio.*
import zio.stream.*

trait GlobalJob[-R] {

  // TODO (KR) : add the ability to auto-reschedule on errors

  val name: String
  val timeout: Specified[Duration]

  def effect: ZIO[R & Scope, UIError, Unit]

  final def delay(duration: Duration): GlobalJob[R] = GlobalJob.internal.DelayStartup(this, duration)

}
object GlobalJob {

  /**
    * Will run the effect immediately, as well as when the job is suspended, and then un-suspended by [[ActivityWatchdog]].
    * This is useful for updating a page immediately after the initial load, as well as updating things if a user leaves and comes back.
    */
  def nowAndOnReturn[R](name: String, timeout: Specified[Duration] = ___)(handle: ZIO[R & Scope, UIError, Unit]): GlobalJob[R] =
    GlobalJob.internal.NowAndOnReturn(name, timeout, handle)

  def fromStream[R, A](name: String, timeout: Specified[Duration] = ___)(stream: ZStream[R, UIError, A])(handle: A => ZIO[R & Scope, UIError, Unit]): GlobalJob[R] =
    GlobalJob.internal.FromStream(name, timeout, stream, handle)

  def simplePoll[R](name: String, timeout: Specified[Duration] = ___)(pollDelay: Duration)(handle: ZIO[R & Scope, UIError, Unit]): GlobalJob[R] =
    GlobalJob.internal.SimplePoll(name, timeout, pollDelay, handle)

  def backoffPoll[R](name: String, timeout: Specified[Duration] = ___)(pollDelay0: Duration, pollDelayN: Duration*)(handle: ZIO[R & Scope, UIError, Boolean]): GlobalJob[R] =
    GlobalJob.internal.BackoffPoll(name, timeout, NonEmptyList(pollDelay0, pollDelayN.toList), handle)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : allow for a configurable default at the PageApp level
  val defaultTimeout: Duration = 5.minutes

  private object internal {

    final case class NowAndOnReturn[R](
        name: String,
        timeout: Specified[Duration],
        handle: ZIO[R & Scope, UIError, Unit],
    ) extends GlobalJob[R] {

      override def effect: ZIO[R & Scope, UIError, Unit] =
        handle *> ZIO.never

    }

    final case class FromStream[R, A](
        name: String,
        timeout: Specified[Duration],
        stream: ZStream[R, UIError, A],
        handle: A => ZIO[R & Scope, UIError, Unit],
    ) extends GlobalJob[R] {

      override def effect: ZIO[R & Scope, UIError, Unit] =
        stream.foreach(handle)

    }

    final case class SimplePoll[R](
        name: String,
        timeout: Specified[Duration],
        pollDelay: Duration,
        handle: ZIO[R & Scope, UIError, Unit],
    ) extends GlobalJob[R] {

      override def effect: ZIO[R & Scope, UIError, Unit] =
        handle.repeat(Schedule.spaced(pollDelay)).unit

    }

    final case class BackoffPoll[R](
        name: String,
        timeout: Specified[Duration],
        pollDelays: NonEmptyList[Duration],
        handle: ZIO[R & Scope, UIError, Boolean],
    ) extends GlobalJob[R] {

      extension (self: NonEmptyList[Duration])
        def nextOrSelf: NonEmptyList[Duration] =
          NonEmptyList.fromList(self.tail).getOrElse(self)

      private def rec(currentPollDelays: NonEmptyList[Duration]): ZIO[R & Scope, UIError, Nothing] =
        handle.flatMap {
          case true  => Clock.sleep(pollDelays.head) *> rec(pollDelays.nextOrSelf)
          case false => Clock.sleep(currentPollDelays.head) *> rec(currentPollDelays.nextOrSelf)
        }

      override def effect: ZIO[R & Scope, UIError, Unit] =
        rec(pollDelays)

    }

    final case class DelayStartup[R](
        inner: GlobalJob[R],
        delay: Duration,
    ) extends GlobalJob[R] {

      override val name: String = inner.name
      override val timeout: Specified[Duration] = inner.timeout

      override def effect: ZIO[R & Scope, UIError, Unit] =
        Clock.sleep(delay) *> inner.effect

    }

  }

}
