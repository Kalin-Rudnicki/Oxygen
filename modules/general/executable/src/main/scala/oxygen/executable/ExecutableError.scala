package oxygen.executable

import oxygen.predef.core.*
import oxygen.zio.ExtractedCauses
import zio.*

sealed trait ExecutableError extends Throwable
object ExecutableError {

  // Exit code for usage errors (invalid/missing args, unknown/missing command), distinct from a genuine
  // runtime failure (exit 1). Mirrors the conventional CLI meaning of exit 2.
  val usageErrorExitCode: ExitCode = ExitCode(2)

  final case class ExitWith(code: ExitCode) extends ExecutableError
  object ExitWith {
    def exit(code: Int): UIO[Nothing] = ZIO.die(ExitWith(ExitCode(code)))

    def exitCodeFromCause(cause: Cause[Any]): Option[ExitCode] = cause match
      case Cause.Die(ExitWith(code), _) => Some(code)
      case Cause.Stackless(c, _)        => exitCodeFromCause(c)
      case Cause.Then(left, right)      => exitCodeFromCause(left).orElse(exitCodeFromCause(right))
      case Cause.Both(left, right)      => exitCodeFromCause(left).orElse(exitCodeFromCause(right))
      case _                            => None

    def messageFromCause(cause: Cause[Any]): String =
      ExtractedCauses.fromCause(cause).foldPriorityHead(
        fail = failure => errorMessage(failure.value),
        die = defect => defect.value.safeGetMessage,
        interrupt = _ => "Interrupted",
        empty = "Unknown error",
      )

    private def errorMessage(error: Any): String =
      error.asInstanceOf[AnyRef] match
        case throwable: Throwable => throwable.safeGetMessage
        case other                => other.toString
  }

}
