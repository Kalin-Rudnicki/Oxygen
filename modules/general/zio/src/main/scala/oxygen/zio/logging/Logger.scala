package oxygen.zio.logging

import oxygen.predef.color.*
import oxygen.predef.core.*
import scala.collection.mutable
import zio.*

final class Logger(val name: String)(val logger: ZLogger[String, String]) {
  override def toString: String = name
}
object Logger {

  /**
    * Default ZIO logger.
    */
  val zioDefault: Logger = Logger("zio-logger")(ZLogger.default)

  /**
    * Logs only the message.
    */
  val rawMessage: Logger = Logger("raw-logger")(ZLogger.simple(identity))

  private val metadataKeyColor: Color = Color.RGB.hex("#EC8C83")
  private val metadataValueColor: Color = Color.RGB.hex("#8272E9")
  private val annotationKeyColor: Color = Color.RGB.hex("#F26CA7")
  private val annotationValueColor: Color = Color.RGB.hex("#1247F8")
  private val spanKeyColor: Color = Color.RGB.hex("#7DCD85")
  private val spanValueColor: Color = Color.RGB.hex("#DF2935")

  def oxygen(
      colorMode: ColorMode,
      logTrace: Boolean,
      logFiberId: Boolean,
      logAnnotations: Boolean,
      logSpans: Boolean,
      logTimestamp: Boolean,
      ignoreStackless: Boolean,
  ): Logger =
    Logger("oxygen-logger") {
      (
          trace: Trace,
          fiberId: FiberId,
          logLevel: LogLevel,
          message0: () => String,
          cause: Cause[Any],
          _: FiberRefs,
          spans0: List[LogSpan],
          annotations: Map[String, String],
      ) =>
        val sb = mutable.StringBuilder(256)

        val richLevel = RichLogLevel.fromLogLevel(logLevel)

        val colorize: (String, Color) => Unit =
          colorMode match {
            case ColorMode.Colorless             => (string, _) => sb.append(string)
            case colored: ColorMode.NonColorless => {
              case (string, Color.Default) =>
                sb.append(string)
              case (string, color) =>
                colored.toConcrete(color) match {
                  case Some(concrete) =>
                    sb.append(colored.prefix)
                    sb.append(colored.fgMod(concrete))
                    sb.append(colored.suffix)
                    sb.append(string)
                    sb.append(colored.prefix)
                    sb.append(colored.fgMod(Color.Default))
                    sb.append(colored.suffix)
                  case None =>
                    sb.append(string)
                }
            }
          }

        inline def writeNewLine(): Unit = sb.append("\n[     ]: ")

        val now = java.time.Instant.now()
        val nowMillis = now.toEpochMilli

        var needsNewLine: Boolean = false
        var startOfLine: Boolean = true

        inline def startNewLine(): Unit = {
          if (needsNewLine)
            writeNewLine()
          needsNewLine = true
          startOfLine = true
        }

        inline def putPair(key: String, value: String)(keyColor: Color, valueColor: Color): Unit = {
          if (startOfLine) startOfLine = false
          else sb.append(' ')

          colorize(key, keyColor)
          sb.append('=')
          colorize(value, valueColor)
        }

        inline def writeSafe(str: String): Unit =
          str.foreach {
            case '\n' => sb.append("\n[     ]: ")
            case c    => sb.append(c)
          }

        // =====| Start |=====

        sb.append('[')
        colorize(richLevel.formattedShortName, richLevel.color)
        sb.append("]: ")

        // =====| Metadata |=====

        if (logTimestamp) {
          needsNewLine = true
          putPair("timestamp", now.toString)(metadataKeyColor, metadataValueColor)
        }

        if (logTrace)
          Trace.unapply(trace).foreach { case (location, file, line) =>
            needsNewLine = true

            putPair("location", location)(metadataKeyColor, metadataValueColor)
            putPair("file", file)(metadataKeyColor, metadataValueColor)
            putPair("line", line.toString)(metadataKeyColor, metadataValueColor)
          }

        if (logFiberId) {
          needsNewLine = true
          putPair("fiber-id", fiberId.threadName)(metadataKeyColor, metadataValueColor)
        }

        // =====| Annotations |=====

        if (logAnnotations && annotations.nonEmpty) {
          startNewLine()
          annotations.foreach { case (key, value) =>
            putPair(key, value)(annotationKeyColor, annotationValueColor)
          }
        }

        // =====| Spans |=====

        if (logSpans && spans0.nonEmpty) {
          startNewLine()
          spans0.foreach { span =>
            putPair(span.label, Duration.fromMillis(nowMillis - span.startTime).render)(spanKeyColor, spanValueColor)
          }
        }

        // =====| Message |=====

        val message = message0()

        if (message.nonEmpty) {
          if (needsNewLine)
            writeNewLine()
          needsNewLine = true

          writeSafe(message)
        }

        // =====| Cause |=====

        def printCauseChain(cause: Throwable): Unit = {
          var c: Throwable = cause

          while (c != null) {
            sb.append("\n[cause]: ")
            writeSafe(c.safeGetMessage)
            c = c.getCause
          }
        }

        def writeTrace(trace: StackTrace): Unit =
          trace.stackTrace.foreach { t =>
            sb.append("\n[     ]:     ")
            sb.append(t)
          }

        def writeCause(
            cause: Cause[Any],
            stackless: Boolean,
            writeInterrupt: Boolean,
        ): Unit =
          cause match {
            case Cause.Empty =>
              ()
            case Cause.Fail(value, trace) =>
              sb.append("\n[CAUSE]: <Fail> : ")
              writeSafe(String.valueOf(value))
              if (ignoreStackless || !stackless)
                writeTrace(trace)
              value.asInstanceOf[Matchable] match {
                case throwable: Throwable => printCauseChain(throwable.getCause)
                case _                    => ()
              }
            case Cause.Die(value, trace) =>
              sb.append("\n[CAUSE]: <Die> : ")
              writeSafe(value.safeGetMessage)
              if (ignoreStackless || !stackless)
                writeTrace(trace)
              printCauseChain(value.getCause)
            case Cause.Interrupt(fiberId, trace) if writeInterrupt =>
              sb.append("\n[CAUSE]: <Interrupt> : ")
              sb.append(fiberId.threadName)
              if (ignoreStackless || !stackless)
                writeTrace(trace)
            case Cause.Interrupt(_, _) =>
              ()
            case Cause.Stackless(cause, _stackless) =>
              writeCause(cause, stackless || _stackless, writeInterrupt)
            case Cause.Then(left, right) =>
              writeCause(left, stackless, writeInterrupt)
              writeCause(right, stackless, writeInterrupt)
            case Cause.Both(left, right) =>
              writeCause(left, stackless, writeInterrupt)
              writeCause(right, stackless, writeInterrupt)
          }

        writeCause(cause, false, cause.failureOption.isEmpty && cause.dieOption.isEmpty)

        sb.result()
    }

  val oxygenDefault: Logger =
    oxygen(ColorMode.Extended, true, true, true, true, true, true)

}
