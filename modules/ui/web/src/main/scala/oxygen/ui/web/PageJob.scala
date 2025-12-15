package oxygen.ui.web

import oxygen.predef.core.*
import zio.*
import zio.stream.*

trait PageJob[-R, S] {

  // TODO (KR) : add the ability to auto-reschedule on errors

  val name: String
  val timeout: Specified[Duration]

  def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit]

  final def delay(duration: Duration): PageJob[R, S] = PageJob.internal.DelayStartup(this, duration)

}
object PageJob {

  def withState[S]: WithState[S] = new WithState[S]

  final class WithState[S] {

    /**
      * Will run the effect immediately, as well as when the job is suspended, and then un-suspended by [[ActivityWatchdog]].
      * This is useful for updating a page immediately after the initial load, as well as updating things if a user leaves and comes back.
      */
    def nowAndOnReturn[R](name: String, timeout: Specified[Duration] = ___)(handle: WidgetState[S] => ZIO[R & Scope, UIError, Unit]): PageJob[R, S] =
      PageJob.internal.NowAndOnReturn(name, timeout, handle)

    def fromStream[R, A](name: String, timeout: Specified[Duration] = ___)(stream: ZStream[R, UIError, A])(handle: (WidgetState[S], A) => ZIO[R & Scope, UIError, Unit]): PageJob[R, S] =
      PageJob.internal.FromStream(name, timeout, stream, handle)

    def simplePoll[R](name: String, timeout: Specified[Duration] = ___)(pollDelay: Duration)(handle: WidgetState[S] => ZIO[R & Scope, UIError, Unit]): PageJob[R, S] =
      PageJob.internal.SimplePoll(name, timeout, pollDelay, handle)

    def backoffPoll[R](name: String, timeout: Specified[Duration] = ___)(pollDelay0: Duration, pollDelayN: Duration*)(handle: WidgetState[S] => ZIO[R & Scope, UIError, Boolean]): PageJob[R, S] =
      PageJob.internal.BackoffPoll(name, timeout, NonEmptyList(pollDelay0, pollDelayN.toList), handle)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : allow for a configurable default at the PageApp level
  val defaultTimeout: Duration = 5.minutes

  private object internal {

    final case class NowAndOnReturn[R, S](
        name: String,
        timeout: Specified[Duration],
        handle: WidgetState[S] => ZIO[R & Scope, UIError, Unit],
    ) extends PageJob[R, S] {

      override def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit] =
        handle(pageState) *> ZIO.never

    }

    final case class FromStream[R, S, A](
        name: String,
        timeout: Specified[Duration],
        stream: ZStream[R, UIError, A],
        handle: (WidgetState[S], A) => ZIO[R & Scope, UIError, Unit],
    ) extends PageJob[R, S] {

      override def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit] =
        stream.foreach(handle(pageState, _))

    }

    final case class SimplePoll[R, S](
        name: String,
        timeout: Specified[Duration],
        pollDelay: Duration,
        handle: WidgetState[S] => ZIO[R & Scope, UIError, Unit],
    ) extends PageJob[R, S] {

      override def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit] =
        handle(pageState).repeat(Schedule.spaced(pollDelay)).unit

    }

    final case class BackoffPoll[R, S](
        name: String,
        timeout: Specified[Duration],
        pollDelays: NonEmptyList[Duration],
        handle: WidgetState[S] => ZIO[R & Scope, UIError, Boolean],
    ) extends PageJob[R, S] {

      extension (self: NonEmptyList[Duration])
        def nextOrSelf: NonEmptyList[Duration] =
          NonEmptyList.fromList(self.tail).getOrElse(self)

      private def rec(currentPollDelays: NonEmptyList[Duration], pageState: WidgetState[S]): ZIO[R & Scope, UIError, Nothing] =
        handle(pageState).flatMap {
          case true  => Clock.sleep(pollDelays.head) *> rec(pollDelays.nextOrSelf, pageState)
          case false => Clock.sleep(currentPollDelays.head) *> rec(currentPollDelays.nextOrSelf, pageState)
        }

      override def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit] =
        rec(pollDelays, pageState)

    }

    final case class DelayStartup[R, S](
        inner: PageJob[R, S],
        delay: Duration,
    ) extends PageJob[R, S] {

      override val name: String = inner.name
      override val timeout: Specified[Duration] = inner.timeout

      override def effect(pageState: WidgetState[S]): ZIO[R & Scope, UIError, Unit] =
        Clock.sleep(delay) *> inner.effect(pageState)

    }

  }

}
