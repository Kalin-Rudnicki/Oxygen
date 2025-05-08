package oxygen.zio.logging

import zio.*

final case class OxygenZLogger(
    logger: ZLogger[String, String],
    minLevel: LogLevel,
    printStream: java.io.PrintStream,
) extends ZLogger[String, Unit] {

  override def apply(
      trace: Trace,
      fiberId: FiberId,
      logLevel: LogLevel,
      message: () => String,
      cause: Cause[Any],
      context: FiberRefs,
      spans: List[LogSpan],
      annotations: Map[String, String],
  ): Unit =
    if (logLevel >= minLevel) printStream.println(logger(trace, fiberId, logLevel, message, cause, context, spans, annotations))
    else ()

  def withMinLevel(minLevel: LogLevel): OxygenZLogger =
    copy(minLevel = minLevel)

}
