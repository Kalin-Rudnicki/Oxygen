package oxygen.executable

import oxygen.json.KeyedMapDecoder
import oxygen.zio.logger.LogTarget
import oxygen.zio.telemetry.TelemetryTarget

private[executable] final case class ExecutableContext(
    logTargetDecoder: KeyedMapDecoder[LogTarget.ConfigBuilder],
    telemetryTargetDecoder: KeyedMapDecoder[TelemetryTarget.ConfigBuilder],
    cfg: ExecutableApp.Config,
)
