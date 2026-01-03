package oxygen.core.error

import oxygen.core.TypeTag
import scala.collection.immutable.ArraySeq
import zio.*

// TODO (KR) : also grab things like annotations?
final case class FromZioCause private (underlying: Error, zioTrace: StackTrace) extends Error.NoStackTrace {
  override def tag: TypeTag[?] = underlying.tag
  override def errorMessage: String = underlying.errorMessage
  override def causes: ArraySeq[Error] = underlying.causes
  override lazy val traces: ArraySeq[Error.StackTrace] = underlying.traces :+ FromZioCause.convertZioTrace(zioTrace)
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
  def fromInterrupt(cause: Cause.Interrupt): Error = FromZioCause(s"Interrupted by ${cause.fiberId}", cause.trace)

}
