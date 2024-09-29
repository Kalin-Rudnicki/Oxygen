package oxygen.zio.logger

import java.time.Instant
import oxygen.predef.color.{*, given}
import oxygen.predef.core.*
import oxygen.predef.json.*
import zio.{LogLevel as _, *}

final case class ExecutedLogEvent(
    logLevel: Option[LogLevel],
    message: String,
    context: Logger.LogContext,
    spans: Logger.LogSpans,
    fiberId: FiberId,
    cause: LogCause,
    trace: Trace,
    stackTrace: Option[StackTrace],
    timestamp: Instant,
) derives JsonCodec {

  def formatted(
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
      logFiberId: Boolean,
  ): String = {

    // TODO (KR) : remove
    // =====|  |=====

    val (tmpMessage, tmpCause) = (message, cause) match {
      case ("", LogCause.Fail(Json.Str(value), None)) => (value, LogCause.Empty)
      case ("", LogCause.Fail(value, None))           => (value.toJsonPretty, LogCause.Empty)
      case _                                          => (message, cause)
    }

    // --- Base Message Part ---

    val msg: String =
      if (tmpMessage.contains('\n')) tmpMessage.replaceAll("\n", LogLevel.newLineIndent)
      else tmpMessage

    extension (self: String) {
      def unescIfNeeded: String = if (self.contains('\n')) self.unesc else self
    }

    val contextMsg =
      Seq(
        context.toSeq.map { case (k, v) => color"${k.unescIfNeeded.cyanFg}=${v.unescIfNeeded.hexFg("#A15E49")}" },
        Seq(
          Option.when(logFiberId)("fiber-id" -> fiberId.threadName),
          Option.when(logTimestamp)("timestamp" -> timestamp.toString), // TODO (KR) : format?
          Option.when(logTrace)(Trace.unapply(trace)).flatten.toSeq.flatMap { case (location, fileName, lineNo) =>
            Seq(
              "location" -> location,
              "file" -> fileName,
              "line" -> lineNo.toString,
            )
          },
        ).flatten.map { case (k, v) => color"${k.unescIfNeeded.cyanFg}=${v.unescIfNeeded.hexFg("#FC4A9D")}" },
        spans.map { span => color"${span.label.unescIfNeeded.cyanFg}=${(timestamp.toEpochMilli - span.startTime).millis.render.hexFg("#9CC4B2")}" },
      ).flatten.csMkString(" ; ").toString(colorMode)

    val baseMessagePart: String =
      if (contextMsg.isEmpty) msg
      else if (msg.isEmpty) contextMsg
      else s"$contextMsg${LogLevel.newLineIndent}$msg"

    // --- Cause ---

    extension (self: Json)
      def showJson: IndentedString =
        (self match {
          case Json.Str(self) => self
          case _              => self.toJsonPretty
        }).split('\n').toList

    extension (self: Option[StackTrace])
      def showStackTrace: IndentedString =
        self.map { stackTrace =>
          val traces: Seq[Trace] = stackTrace.stackTrace
          IndentedString.section("StackTrace:")(
            s"fiber-id=${stackTrace.fiberId.threadName}",
            IndentedString.section("trace")(traces.map(_.toString)),
          )
        }

    extension (self: LogCause.Base)
      def showCause: IndentedString =
        self match {
          case LogCause.Fail(value, trace) =>
            IndentedString.inline(
              IndentedString.section("Fail:")(value.showJson),
              trace.showStackTrace,
            )
          case LogCause.Die(value, trace) =>
            IndentedString.inline(
              IndentedString.section("Die:")(value.safeToJsonAST.showJson),
              trace.showStackTrace,
            )
          case LogCause.Interrupt(fiberId, trace) =>
            IndentedString.inline(
              IndentedString.section("Interrupt:")(s"fiber-id=${fiberId.threadName}"),
              trace.showStackTrace,
            )
        }

    val causeBases: Chunk[(LogCause.Base, Int)] =
      tmpCause.toBases.zipWithIndex

    def causeLabel(i: Int): String =
      s"cause-$i"

    val causeWidth: Int = causeLabel(causeBases.size - 1).length max (LogLevel.maxDisplayNameLength + 2)
    val emptyCauseString: String = s"\n${" " * causeWidth}: "

    val causeString: String =
      causeBases.map { case (cause, idx) =>
        s"\n${causeLabel(idx).alignLeft(causeWidth)}: ${cause.showCause.toString.replaceAll("\n", emptyCauseString)}"
      }.mkString

    // --- Stack Trace ---

    val stackString: String =
      (stackTrace, logStack) match {
        case (Some(stackTrace), true) =>
          val idtStr =
            IndentedString.inline(
              s"fiber-id=${stackTrace.fiberId}",
              (stackTrace.stackTrace: Seq[Trace]).map(t => s"- $t"),
            )
          s"\n${"stack".alignLeft(causeWidth)}: ${idtStr.toString.replaceAll("\n", emptyCauseString)}"
        case _ => ""
      }

    // --- Join All ---

    s"[${logLevel.fold(LogLevel.emptyDisplayName)(_.colorizedDisplayName(colorMode))}]: $baseMessagePart$causeString$stackString"
  }

}
