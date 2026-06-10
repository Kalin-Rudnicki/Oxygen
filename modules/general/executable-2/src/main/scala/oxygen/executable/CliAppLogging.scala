package oxygen.executable

import oxygen.predef.core.*
import oxygen.zio.logging.{LogConfig, Logger, LogLevels}
import zio.*

private[executable] object CliAppLogging {

  val defaultDefaults: OxygenLoggerDefaults = OxygenLoggerDefaults.lean()

  def logConfig(defaults: OxygenLoggerDefaults = defaultDefaults, level: LogLevel = LogLevels.Info): LogConfig =
    LogConfig(
      annotations = Map.empty,
      spans = Nil,
      loggers = ArraySeq(
        LogConfig.LoggerElem(
          Logger.oxygen(
            colorMode = defaults.colorMode,
            logTrace = defaults.logTrace,
            logFiberId = defaults.logFiberId,
            logAnnotations = defaults.logAnnotations,
            logSpans = defaults.logSpans,
            logTimestamp = defaults.logTimestamp,
            ignoreStackless = defaults.ignoreStackless,
          ),
          level,
        ),
      ),
    )

  def install: URIO[Any, Unit] =
    LogConfig.usingConfig(logConfig()).set.unit

}