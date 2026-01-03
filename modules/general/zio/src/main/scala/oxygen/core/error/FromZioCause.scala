package oxygen.core.error

import oxygen.core.LazyString
import scala.collection.immutable.ArraySeq
import zio.*

// TODO (KR) : also grab things like annotations?
final case class FromZioCause private (underlying: Error, zioTrace: StackTrace) extends Error.NoStackTrace {
  override lazy val errorType: Error.ErrorType = underlying.errorType
  override def errorMessage: LazyString = underlying.errorMessage
  override def causes: ArraySeq[Error] = underlying.causes
  override lazy val traces: ArraySeq[Error.StackTrace] = FromZioCause.convertZioTrace(zioTrace) +: underlying.traces
}
object FromZioCause {

  private def convertZioTrace(trace: StackTrace): Error.StackTrace =
    Error.StackTrace(
      "ZIO",
      ArraySeq.from {
        trace.stackTrace.map {
          case Trace(location, file, line) => Error.Trace.Rich(location, file, line)
          case trace                       => Error.Trace.Raw(trace.toString)
        }
      },
    )

  def fromFail(cause: Cause.Fail[?]): Error = FromZioCause(Error.fromAny(cause.value), cause.trace)
  def fromDie(cause: Cause.Die): Error = FromZioCause(Error.fromThrowable(cause.value), cause.trace)
  def fromInterrupt(cause: Cause.Interrupt): Error = FromZioCause(Error(s"Interrupted by ${cause.fiberId}"), cause.trace)

  def errorWithCause(message: LazyString.Auto, cause: Cause[?]): Error =
    Error(message, oxygen.zio.ExtractedCauses.fromCause(cause).toErrors)

  def errorCauseWithCause(message: LazyString.Auto, cause: Cause[?])(using trace: Trace): Cause[Error] = {
    val extracted = oxygen.zio.ExtractedCauses.fromCause(cause)
    val stack = StackTrace(FiberId.None, Chunk.single(trace)) // TODO (KR) : is there value in trying to get the actual FiberId?
    extracted match
      case _: oxygen.zio.ExtractedCauses.Failures[?] => Cause.fail(Error(message, extracted.toErrors), stack)
      case _                                         => Cause.die(Error(message, extracted.toErrors), stack)
  }

}
