package oxygen.zio.logger

import java.io.PrintStream
import oxygen.core.ColorMode
import oxygen.predef.json.*
import scala.collection.mutable
import zio.{LogLevel as _, *}

trait LogTarget {

  val name: String
  val handle: ExecutedLogEvent => UIO[Unit]
  val minLogLevel: Option[LogLevel]
  val args: Chunk[(String, String)]

  override def toString: String = {
    val allArgs: Chunk[(String, String)] = ("level", minLogLevel.fold("default")(_.rawDisplayName)) +: args
    s"LogTarget[$name${allArgs.map { case (k, v) => s", $k=$v" }.mkString}]"
  }

}
object LogTarget {

  private def logToPrintStream(printStream: PrintStream, eventToString: ExecutedLogEvent => String): ExecutedLogEvent => UIO[Unit] =
    event => ZIO.succeed { printStream.println(eventToString(event)) }

  final case class StdOut(
      minLogLevel: Option[LogLevel],
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
      logFiberId: Boolean,
  ) extends LogTarget {

    override val name: String = "std-out"

    override val handle: ExecutedLogEvent => UIO[Unit] = logToPrintStream(scala.Console.out, _.formatted(colorMode, logTimestamp, logTrace, logStack, logFiberId))

    override val args: Chunk[(String, String)] = Chunk("logTimestamp" -> logTimestamp.toString, "logTrace" -> logTrace.toString, "logStack" -> logStack.toString)

  }
  object StdOut {

    val defaultWithoutAdditionalContext: StdOut =
      StdOut(None, ColorMode.Extended, false, false, false, false)

    val defaultWithAdditionalContext: StdOut =
      StdOut(None, ColorMode.Extended, true, true, true, true)

  }

  final case class StdOutJson(
      minLogLevel: Option[LogLevel],
  ) extends LogTarget {

    override val name: String = "std-out-json"

    override val handle: ExecutedLogEvent => UIO[Unit] = logToPrintStream(scala.Console.out, _.toJson)

    override val args: Chunk[(String, String)] = Chunk.empty

  }

  final case class StringBuilder(
      sb: mutable.StringBuilder,
      minLogLevel: Option[LogLevel],
      colorMode: ColorMode,
      logTimestamp: Boolean,
      logTrace: Boolean,
      logStack: Boolean,
      logFiberId: Boolean,
  ) extends LogTarget {

    override val name: String = "string-builder"

    override val handle: ExecutedLogEvent => UIO[Unit] = event => ZIO.succeed { sb.append(event.formatted(colorMode, logTimestamp, logTrace, logStack, logFiberId)); sb.append('\n') }

    override val args: Chunk[(String, String)] = Chunk("logTimestamp" -> logTimestamp.toString, "logTrace" -> logTrace.toString, "logStack" -> logStack.toString)

  }

}
