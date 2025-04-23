package oxygen.zio.typeclass

import oxygen.predef.core.*
import oxygen.predef.json.{*, given}
import oxygen.zio.logger.*

final case class ErrorLogger[-E](
    logLevel: E => LogLevel,
    show: E => Json,
)
object ErrorLogger {

  def apply[E](implicit errorLogger: ErrorLogger[E]): ErrorLogger[E] = errorLogger

  def make[E](
      logLevel: E => LogLevel,
      show: E => Json | String,
  ): ErrorLogger[E] =
    ErrorLogger[E](
      logLevel,
      show(_) match {
        case string: String => Json.Str(string)
        case json: Json     => json
      },
    )

  // =====|  |=====

  def stringEncoded[E: StringEncoder]: ErrorLogger.Builder[E] = Builder.string[E](StringEncoder[E].encode(_))
  def jsonEncoded[E: JsonEncoder]: ErrorLogger.Builder[E] = Builder.json[E](_.toJsonAST)

  def withShow[E](f: E => String): ErrorLogger.Builder[E] = Builder.string[E](f)
  def withJsonShow[E](f: E => Json): ErrorLogger.Builder[E] = Builder.json[E](f)

  def withToString[E]: ErrorLogger.Builder[E] = Builder.string[E](_.toString)
  def throwableRepr[E <: Throwable]: ErrorLogger.Builder[E] = Builder.json[E](ThrowableRepr.fromThrowable(_).toJsonAST)
  def throwableGetMessage[E <: Throwable]: ErrorLogger.Builder[E] = Builder.string[E](_.safeGetMessage)

  // =====|  |=====

  final class Builder[E] private (show: E => Json) {

    def atLevel: WithLogLevel[ErrorLogger[E]] = WithLogLevel.make { logLevel => ErrorLogger[E](_ => logLevel, show) }

    def withLevel(f: E => LogLevel): ErrorLogger[E] = ErrorLogger[E](f, show)

  }
  object Builder {
    private[ErrorLogger] def json[E](f: E => Json): Builder[E] = new Builder(f)
    private[ErrorLogger] def string[E](f: E => String): Builder[E] = new Builder(e => Json.Str(f(e)))
  }

  implicit val nothingErrorLogger: ErrorLogger[Nothing] =
    ErrorLogger.withToString[Nothing].atLevel.always

  object ThrowableInstances {

    def throwableErrorLogger(level: LogLevel): ErrorLogger[Throwable] =
      ErrorLogger.throwableRepr[Throwable].atLevel(level)
    implicit def throwableErrorLogger: ErrorLogger[Throwable] =
      throwableErrorLogger(LogLevel.Error)

  }

}
