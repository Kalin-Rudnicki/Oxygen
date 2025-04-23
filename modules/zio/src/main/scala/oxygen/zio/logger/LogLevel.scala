package oxygen.zio.logger

import oxygen.json.syntax.interop.*
import oxygen.predef.color.*
import oxygen.predef.core.*
import oxygen.predef.json.*

private def zioLevelBetween(label: String, a: zio.LogLevel, b: zio.LogLevel): zio.LogLevel =
  zio.LogLevel((a.ordinal + b.ordinal) / 2, label, (a.syslog + b.syslog) / 2)

enum LogLevel(
    final val rawDisplayName: String,
    final val tolerancePriority: Int,
    final val logPriority: Int,
    final val color: Color,
    final val zioLogLevel: zio.LogLevel,
) extends java.lang.Enum[LogLevel]
    with Enum[LogLevel] {

  case Never
      extends LogLevel(
        rawDisplayName = "NEVER",
        tolerancePriority = 10,
        logPriority = 0,
        color = Color.Default,
        zioLogLevel = zio.LogLevel(Int.MinValue, "NEVER", 7),
      )

  case Trace
      extends LogLevel(
        rawDisplayName = "TRACE",
        tolerancePriority = 1,
        logPriority = 1,
        color = Color.RGB.hex("#30f2c2") :> Color.Named.Cyan,
        zioLogLevel = zio.LogLevel.Trace,
      )

  case Debug
      extends LogLevel(
        rawDisplayName = "DEBUG",
        tolerancePriority = 2,
        logPriority = 2,
        color = Color.RGB.hex("#0277bd") :> Color.Named.Cyan,
        zioLogLevel = zio.LogLevel.Debug,
      )

  case Detailed
      extends LogLevel(
        rawDisplayName = "DETLD",
        tolerancePriority = 3,
        logPriority = 3,
        color = Color.RGB.hex("#66bb6a") :> Color.Named.Blue,
        zioLogLevel = zioLevelBetween("DETAILED", zio.LogLevel.Debug, zio.LogLevel.Info),
      )

  case Info
      extends LogLevel(
        rawDisplayName = "INFO",
        tolerancePriority = 4,
        logPriority = 4,
        color = Color.RGB.hex("#1b5e20") :> Color.Named.Green,
        zioLogLevel = zio.LogLevel.Info,
      )

  case Important
      extends LogLevel(
        rawDisplayName = "IMPRT",
        tolerancePriority = 5,
        logPriority = 5,
        color = Color.RGB.hex("#880e4f") :> Color.Named.Yellow,
        zioLogLevel = zioLevelBetween("IMPORTANT", zio.LogLevel.Info, zio.LogLevel.Warning),
      )

  case Warning
      extends LogLevel(
        rawDisplayName = "WARN",
        tolerancePriority = 6,
        logPriority = 6,
        color = Color.RGB.hex("#ffff00") :> Color.Named.Yellow,
        zioLogLevel = zio.LogLevel.Warning,
      )

  case Error
      extends LogLevel(
        rawDisplayName = "ERROR",
        tolerancePriority = 7,
        logPriority = 7,
        color = Color.RGB.hex("#ff3d00") :> Color.Named.Red,
        zioLogLevel = zio.LogLevel.Error,
      )

  case Fatal
      extends LogLevel(
        rawDisplayName = "FATAL",
        tolerancePriority = 8,
        logPriority = 8,
        color = Color.RGB.hex("#d50000") :> Color.Named.Red,
        zioLogLevel = zio.LogLevel.Fatal,
      )

  case Always
      extends LogLevel(
        rawDisplayName = "ALWYS",
        tolerancePriority = 9,
        logPriority = 9,
        color = Color.Default,
        zioLogLevel = zio.LogLevel(Int.MaxValue, "ALWAYS", 0),
      )

  final lazy val displayName: String =
    s"$rawDisplayName${" " * (LogLevel.maxDisplayNameLength - rawDisplayName.length)}"

  final lazy val colorizedName: ColorString = ColorString.make(ColorState.fg(color))(displayName)

  private final lazy val colorModeDisplayNameMap: Map[ColorMode, String] =
    ColorMode.enumValues.map { cm =>
      cm -> colorizedName.toString(cm)
    }.toMap

  final def colorizedDisplayName(colorMode: ColorMode): String = colorModeDisplayNameMap(colorMode)

  override final def toString: String = rawDisplayName

}
object LogLevel extends Enum.Companion[LogLevel] {

  override protected val defaultToString: LogLevel => NonEmptyList[String] = l => NonEmptyList.of(l.rawDisplayName, l.name)

  lazy val maxDisplayNameLength: Int =
    enumValues.map(_.rawDisplayName.length).max

  lazy val emptyDisplayName: String =
    " " * maxDisplayNameLength

  lazy val newLineIndent: String =
    s"\n $emptyDisplayName : "

  implicit val jsonCodec: JsonCodec[LogLevel] = JsonCodec.usingStringCodec

}
