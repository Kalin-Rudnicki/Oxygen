package oxygen.zio.logging

import zio.LogLevel

object LogLevels {

  extension (self: LogLevel.type)
    private def between(name: String, a: LogLevel, b: LogLevel): LogLevel =
      LogLevel((a.ordinal + b.ordinal) / 2, name, (a.syslog + b.syslog) / 2)

  val All: LogLevel = LogLevel.All
  val Fatal: LogLevel = LogLevel.Fatal
  val Error: LogLevel = LogLevel.Error
  val Warning: LogLevel = LogLevel.Warning
  val Info: LogLevel = LogLevel.Info
  val Debug: LogLevel = LogLevel.Debug
  val Trace: LogLevel = LogLevel.Trace
  val None: LogLevel = LogLevel.None

  val Important: LogLevel = LogLevel.between("IMPRT", Warning, Info)
  val Detailed: LogLevel = LogLevel.between("DETLD", Info, Debug)

  val levels: Set[LogLevel] = Set(
    All,
    Fatal,
    Error,
    Warning,
    Important,
    Info,
    Detailed,
    Debug,
    Trace,
    None,
  )

}
