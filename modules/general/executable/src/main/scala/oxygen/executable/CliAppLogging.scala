package oxygen.executable

import oxygen.core.typeclass.StringDecoder
import oxygen.predef.core.*
import oxygen.zio.logging.{LogConfig, Logger, LogLevels, RichLogLevel}
import zio.*

private[executable] object CliAppLogging {

  private def oxygenLogger(defaults: OxygenLoggerDefaults): Logger =
    Logger.oxygen(
      colorMode = defaults.colorMode,
      logTrace = defaults.logTrace,
      logFiberId = defaults.logFiberId,
      logAnnotations = defaults.logAnnotations,
      logSpans = defaults.logSpans,
      logTimestamp = defaults.logTimestamp,
      ignoreStackless = defaults.ignoreStackless,
    )

  def logConfig(loggerType: DefaultLoggerType, level: LogLevel = LogLevels.Info): LogConfig =
    val logger: Logger = loggerType match
      case DefaultLoggerType.OxygenAll  => oxygenLogger(OxygenLoggerDefaults.all())
      case DefaultLoggerType.OxygenLean => oxygenLogger(OxygenLoggerDefaults.lean())
      case DefaultLoggerType.Zio        => Logger.zioDefault
    LogConfig(
      annotations = Map.empty,
      spans = Nil,
      loggers = ArraySeq(LogConfig.LoggerElem(logger, level)),
    )

  private def resolveLoggerType(default: DefaultLoggerType): URIO[Any, DefaultLoggerType] =
    System.env(Constants.oxygenLoggerType).orDie.map(_.flatMap(StrictEnum[DefaultLoggerType].decodeOption).getOrElse(default))

  private def resolveLogLevel(default: LogLevel): URIO[Any, LogLevel] =
    System.env(Constants.oxygenLogLevel).orDie.map { opt =>
      opt.flatMap(v => StringDecoder[RichLogLevel].decode(v).toOption.map(_.level)).getOrElse(default)
    }

  def install(defaultLoggerType: DefaultLoggerType = DefaultLoggerType.default): URIO[Any, Unit] =
    for {
      loggerType <- resolveLoggerType(defaultLoggerType)
      logLevel <- resolveLogLevel(LogLevels.Info)
      _ <- LogConfig.usingConfig(logConfig(loggerType, logLevel)).set.unit
    } yield ()

}
