package oxygen.executable

import zio.*

sealed trait ExecutableError extends Throwable
object ExecutableError {

  final case class ExitWith(code: ExitCode) extends ExecutableError
  object ExitWith {
    def exit(code: Int): UIO[Nothing] = ZIO.die(ExitWith(ExitCode(code)))

    def exitCodeFromCause(cause: Cause[Any]): Option[ExitCode] = cause match
      case Cause.Die(ExitWith(code), _) => Some(code)
      case Cause.Stackless(c, _)        => exitCodeFromCause(c)
      case Cause.Then(left, right)      => exitCodeFromCause(left).orElse(exitCodeFromCause(right))
      case Cause.Both(left, right)      => exitCodeFromCause(left).orElse(exitCodeFromCause(right))
      case _                            => None
  }

}
