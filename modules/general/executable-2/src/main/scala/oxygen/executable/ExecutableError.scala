package oxygen.executable

import zio.*

sealed trait ExecutableError extends Throwable
object ExecutableError {

  final case class ExitWith(code: ExitCode) extends ExecutableError
  object ExitWith {
    def exit(code: Int): UIO[Nothing] = ZIO.die(ExitWith(ExitCode(code)))
  }

}
