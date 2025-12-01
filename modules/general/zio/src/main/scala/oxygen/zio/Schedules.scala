package oxygen.zio

import java.time.OffsetDateTime
import oxygen.predef.core.*
import scala.Ordering.Implicits.infixOrderingOps
import zio.*

object Schedules {

  /**
    * Will attempt to wait until the specified duration from the start.
    */
  def timeout(duration: Duration): Schedule.WithState[Option[OffsetDateTime], Any, Any, Unit] =
    new Schedule[Any, Any, Unit] {
      override type State = Option[OffsetDateTime]
      override def initial: Option[OffsetDateTime] = None
      override def step(now: OffsetDateTime, in: Any, state: Option[OffsetDateTime])(using trace: Trace): ZIO[Any, Nothing, (Option[OffsetDateTime], Unit, Schedule.Decision)] =
        state match {
          case Some(target) if now < target =>
            ZIO.succeed((state, (), Schedule.Decision.Continue(Schedule.Interval.after(target))))
          case Some(_) =>
            ZIO.succeed((state, (), Schedule.Decision.Done))
          case None =>
            val target = now.plus(duration)
            ZIO.succeed((target.some, (), Schedule.Decision.Continue(Schedule.Interval.after(target))))
        }
    }

  extension [Env, In, Out1](a: Schedule[Env, In, Out1]) {

    /**
      * Combination of intersection and union.
      * Will only recur if both schedules wish to, but will take the min delay of the two.
      */
    def &|[Out2](b: Schedule[Env, In, Out2])(using zip: Zippable[Out1, Out2]): Schedule.WithState[(a.State, b.State), Env, In, zip.Out] =
      a.intersectWith(b) { _ union _ }

    def withTimeout(duration: Duration): Schedule.WithState[(a.State, Option[OffsetDateTime]), Env, In, Out1] =
      a &| timeout(duration)

  }

}
