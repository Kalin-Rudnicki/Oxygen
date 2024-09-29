package oxygen.zio.telemetry

import java.time.Instant
import oxygen.core.Enum
import zio.{Duration, Exit}

final case class TraceEntry(
    key: String,
    args: Map[String, String],
    start: Instant,
    end: Instant,
    result: TraceEntry.Result,
) {
  lazy val duration: Duration = Duration.fromInterval(start, end)
}
object TraceEntry {

  enum Result extends Enum[Result] { case Success, Failure, Defect }
  object Result extends Enum.Companion[Result] {

    def fromExit(exit: Exit[?, ?]): Result =
      exit match {
        case Exit.Success(_)                        => Success
        case Exit.Failure(cause) if cause.isFailure => Failure
        case _                                      => Defect
      }

  }

}
