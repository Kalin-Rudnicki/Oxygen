package oxygen.zio.logging

import oxygen.predef.color.*
import oxygen.predef.core.*
import zio.LogLevel

final case class RichLogLevel(
    level: LogLevel,
    fullName: String,
    shortName: String,
    color: Color,
) {

  val formattedShortName: String =
    if (shortName.length == 5) shortName
    else if (shortName.length < 5) shortName.alignCenter(5)
    else shortName.substring(0, 5)

}
object RichLogLevel {

  val All: RichLogLevel = simple(LogLevels.All)
  val Fatal: RichLogLevel = simple(LogLevels.Fatal, Color.RGB.hex("#d50000") :> Color.Named.Red)
  val Error: RichLogLevel = simple(LogLevels.Error, Color.RGB.hex("#ff3d00") :> Color.Named.Red)
  val Warning: RichLogLevel = simple(LogLevels.Warning, Color.RGB.hex("#ffff00") :> Color.Named.Yellow)
  val Important: RichLogLevel = RichLogLevel(LogLevels.Important, "IMPORTANT", "IMPRT", Color.RGB.hex("#880e4f") :> Color.Named.Yellow)
  val Info: RichLogLevel = simple(LogLevels.Info, Color.RGB.hex("#1b5e20") :> Color.Named.Green)
  val Detailed: RichLogLevel = RichLogLevel(LogLevels.Detailed, "DETAILED", "DETLD", Color.RGB.hex("#66bb6a") :> Color.Named.Blue)
  val Debug: RichLogLevel = simple(LogLevels.Debug, Color.RGB.hex("#0277bd") :> Color.Named.Cyan)
  val Trace: RichLogLevel = simple(LogLevels.Trace, Color.RGB.hex("#30f2c2") :> Color.Named.Cyan)
  val None: RichLogLevel = simple(LogLevels.None)

  private val levels: Map[String, RichLogLevel] =
    Seq[RichLogLevel](
      RichLogLevel.All,
      RichLogLevel.Fatal,
      RichLogLevel.Error,
      RichLogLevel.Warning,
      RichLogLevel.Important,
      RichLogLevel.Info,
      RichLogLevel.Detailed,
      RichLogLevel.Debug,
      RichLogLevel.Trace,
      RichLogLevel.None,
    ).map { l => (l.level.label, l) }.toMap

  def simple(level: LogLevel, color: Color = Color.Default): RichLogLevel =
    RichLogLevel(level, level.label, level.label, color)

  def fromLogLevel(level: LogLevel): RichLogLevel =
    levels.getOrElse(level.label, simple(level))

  given strictEnum: StrictEnum[RichLogLevel] = StrictEnum.make[RichLogLevel](levels.values.toSeq, level => NonEmptyList.of(level.fullName, level.shortName).distinct)

}
