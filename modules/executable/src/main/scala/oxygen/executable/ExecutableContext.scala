package oxygen.executable

import oxygen.cli.Params
import oxygen.core.collection.Contiguous
import oxygen.json.KeyedMapDecoder
import oxygen.zio.logging.{LogConfig, Logger}

private[executable] final case class ExecutableContext(
    logTargetDecoder: KeyedMapDecoder[LogConfig.LoggerElem],
    additionalLoggerParsers: Contiguous[Params[Logger]],
    executableConfig: ExecutableApp.Config,
)
