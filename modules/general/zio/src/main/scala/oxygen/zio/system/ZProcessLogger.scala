package oxygen.zio.system

import scala.sys.process.*
import zio.*

final case class ZProcessLogger(
    loggers: Set[ZLogger[String, Any]],
    trace: Trace,
    fiberId: FiberId,
    outLevel: LogLevel,
    errorLevel: LogLevel,
    fiberRefs: FiberRefs,
    spans: List[LogSpan],
    annotations: Map[String, String],
) extends ProcessLogger {
  override def out(s: => String): Unit = loggers.foreach { _.apply(trace, fiberId, outLevel, () => s, Cause.Empty, fiberRefs, spans, annotations) }
  override def err(s: => String): Unit = loggers.foreach { _.apply(trace, fiberId, errorLevel, () => s, Cause.Empty, fiberRefs, spans, annotations) }
  override def buffer[T](f: => T): T = f
}
object ZProcessLogger {

  def make(
      outLevel: LogLevel = LogLevel.Info,
      errorLevel: LogLevel = LogLevel.Error,
  )(using trace: Trace): UIO[ZProcessLogger] =
    for {
      loggers <- ZIO.loggers
      fiberId <- ZIO.fiberId
      runtime <- ZIO.runtime
      spans <- ZIO.logSpans
      annotations <- ZIO.logAnnotations
    } yield ZProcessLogger(loggers, trace, fiberId, outLevel, errorLevel, runtime.fiberRefs, spans, annotations)

}
