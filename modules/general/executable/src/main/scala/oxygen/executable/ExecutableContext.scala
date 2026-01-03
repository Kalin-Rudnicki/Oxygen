package oxygen.executable

import oxygen.cli.Params
import oxygen.json.KeyedMapDecoder
import oxygen.predef.core.*
import oxygen.zio.logging.{LogConfig, Logger}

private[executable] final case class ExecutableContext(
    logTargetDecoder: KeyedMapDecoder[LogConfig.LoggerElem],
    additionalLoggerParsers: ArraySeq[Params[Logger]],
    executableConfig: ExecutableApp.Config,
    oxygenLoggerDefaults: OxygenLoggerDefaults,
)
