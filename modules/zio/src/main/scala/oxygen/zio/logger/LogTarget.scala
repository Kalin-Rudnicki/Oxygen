package oxygen.zio.logger

import java.io.PrintStream
import oxygen.core.ColorMode
import oxygen.predef.json.*
import scala.collection.mutable
import zio.{Chunk, UIO, ZIO}

final case class LogTarget(
    name: String,
    log: ExecutedLogEvent => UIO[Unit],
    minLogLevel: Option[LogLevel],
    args: Chunk[(String, String)],
) {

  override def toString: String = {
    val allArgs: Chunk[(String, String)] = ("level", minLogLevel.fold("default")(_.rawDisplayName)) +: args
    s"Source[$name${allArgs.map { case (k, v) => s", $k=$v" }.mkString}]"
  }

}
object LogTarget {

  object names {

    val stdOut = "std-out"
    val stdOutJson = "std-out-json"
    val stringBuilder = "string-builder"

    val allStdOut: Set[String] = Set(stdOut, stdOutJson)

  }

  private def logToPrintStream(printStream: PrintStream, eventToString: ExecutedLogEvent => String): ExecutedLogEvent => UIO[Unit] =
    event => ZIO.succeed { printStream.println(eventToString(event)) }

  def stdOut(
      minLogLevel: Option[LogLevel],
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
      logFiberId: Boolean,
  ): LogTarget =
    LogTarget(
      names.stdOut,
      logToPrintStream(scala.Console.out, _.formatted(colorMode, logTimestamp, logTrace, logStack, logFiberId)),
      minLogLevel,
      Chunk("logTimestamp" -> logTimestamp.toString, "logTrace" -> logTrace.toString, "logStack" -> logStack.toString),
    )

  def stdOutJson(
      minLogLevel: Option[LogLevel],
  ): LogTarget =
    LogTarget(
      names.stdOutJson,
      logToPrintStream(scala.Console.out, _.toJson),
      minLogLevel,
      Chunk.empty,
    )

  def stringBuilder(
      sb: mutable.StringBuilder,
      minLogLevel: Option[LogLevel],
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
      logFiberId: Boolean,
  ): LogTarget =
    LogTarget(
      names.stdOut,
      event => ZIO.succeed { sb.append(event.formatted(colorMode, logTimestamp, logTrace, logStack, logFiberId)); sb.append('\n') },
      minLogLevel,
      Chunk("logTimestamp" -> logTimestamp.toString, "logTrace" -> logTrace.toString, "logStack" -> logStack.toString),
    )

}
