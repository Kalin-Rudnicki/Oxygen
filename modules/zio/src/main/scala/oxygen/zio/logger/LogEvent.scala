package oxygen.zio.logger

import oxygen.predef.core.*
import zio.{LogLevel as _, *}

final case class LogEvent(
    level: Option[LogLevel],
    _message: () => String,
    _context: () => Logger.LogContext,
    cause: LogCause,
    trace: Trace,
    stackTrace: Option[StackTrace],
) {
  lazy val message: String = _message()
  lazy val context: Logger.LogContext = _context()
}
object LogEvent {

  def apply(
      level: LogLevel,
      message: => String,
      context: => Logger.LogContext,
      cause: LogCause,
      trace: Trace,
      stackTrace: Option[StackTrace],
  ): LogEvent =
    new LogEvent(
      level = level.some,
      _message = () => message,
      _context = () => context,
      cause = cause,
      trace = trace,
      stackTrace = stackTrace,
    )

  def apply(
      message: => String,
      context: => Logger.LogContext,
      cause: LogCause,
      trace: Trace,
      stackTrace: Option[StackTrace],
  ): LogEvent =
    new LogEvent(
      level = None,
      _message = () => message,
      _context = () => context,
      cause = cause,
      trace = trace,
      stackTrace = stackTrace,
    )

}
