package oxygen.http.model

import oxygen.json.jsonDiscriminator
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.zio.ExtractedCauses
import zio.{Cause as ZioCause, StackTrace}

final case class ServerErrors(
    errors: NonEmptyList[ServerErrors.Error],
) derives JsonSchema
object ServerErrors {

  @jsonDiscriminator("errorType")
  sealed trait Error derives JsonSchema {

    final def simpleMessage: String = this match
      case Error.Failure(cause) => cause.message
      case Error.Defect(cause)  => cause.message
      case Error.Interrupt(_)   => "Interrupted"
      case Error.NoCause        => "No Cause"

  }
  object Error {

    final case class Failure(cause: Cause) extends Error
    final case class Defect(cause: Cause) extends Error
    final case class Interrupt(zioTrace: Option[ArraySeq[String]]) extends Error
    case object NoCause extends Error

    def failure(cause: ZioCause.Fail[Throwable], includeTrace: Boolean): Error =
      Error.Failure(Cause.fromThrowable(cause.value, cause.trace, includeTrace))
    def defect(cause: ZioCause.Die, includeTrace: Boolean): Error =
      Error.Defect(Cause.fromThrowable(cause.value, cause.trace, includeTrace))
    def interrupt(cause: ZioCause.Interrupt, includeTrace: Boolean): Error =
      Error.Interrupt(cause.trace.convertZioTrace(includeTrace))

  }

  final case class Cause(
      `class`: String,
      message: String,
      throwableTrace: Option[ArraySeq[String]],
      zioTrace: Option[ArraySeq[String]],
      cause: Option[Cause],
  ) derives JsonSchema
  object Cause {

    def fromThrowable(cause: Throwable, trace: StackTrace, includeTrace: Boolean): Cause =
      Cause(
        `class` = cause.getClass.getTypeName,
        message = cause.safeGetMessage,
        throwableTrace = cause.convertThrowableTrace(includeTrace),
        zioTrace = trace.convertZioTrace(includeTrace),
        cause = Option(cause.getCause).map(fromThrowable(_, StackTrace.none, includeTrace)),
      )

  }

  extension (self: StackTrace)
    private def convertZioTrace(includeTrace: Boolean): Option[ArraySeq[String]] =
      if includeTrace then ArraySeq.from(self.stackTrace.map(_.toString)).someWhen(_.nonEmpty)
      else None

  extension (self: Throwable)
    private def convertThrowableTrace(includeTrace: Boolean): Option[ArraySeq[String]] =
      if includeTrace then Option(self.getStackTrace).flatMap(ArraySeq.unsafeWrapArray(_).map(_.toString).someWhen(_.nonEmpty))
      else None

  extension (self: NonEmptyList[Error])
    def includeWhen[A](cond: Boolean)(other: List[A])(f: A => Error): NonEmptyList[Error] =
      if cond && other.nonEmpty then self :++ other.map(f)
      else self

  def fromCause(
      cause: ExtractedCauses[Throwable],
      includeTraces: Boolean,
      includeDefectsOnFailure: Boolean,
      includeInterruptsOnFailure: Boolean,
      includeInterruptsOnDefect: Boolean,
  ): ServerErrors = {
    val errors: NonEmptyList[Error] =
      cause match {
        case ExtractedCauses.Failures(failures, defects, interrupts) =>
          failures
            .map(Error.failure(_, includeTraces))
            .includeWhen(includeDefectsOnFailure)(defects)(Error.defect(_, includeTraces))
            .includeWhen(includeInterruptsOnFailure)(interrupts)(Error.interrupt(_, includeTraces))
        case ExtractedCauses.Defects(defects, interrupts) =>
          defects
            .map(Error.defect(_, includeTraces))
            .includeWhen(includeInterruptsOnDefect)(interrupts)(Error.interrupt(_, includeTraces))
        case ExtractedCauses.Interrupts(interrupts) =>
          interrupts.map(Error.interrupt(_, includeTraces))
        case ExtractedCauses.Empty =>
          NonEmptyList.one(Error.NoCause)
      }

    ServerErrors(errors)
  }

}
