package oxygen.zio

import oxygen.core.collection.Contiguous
import oxygen.zio.logger.*
import oxygen.zio.telemetry.*
import zio.{LogLevel as _, *}

object OxygenEnv {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Logger
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class LoggerEnv(
      targets: Contiguous[LogTarget],
      context: Logger.LogContext,
      spans: List[Logger.Span],
      level: LogLevel,
      logToZio: Boolean,
  )

  val logTargets: FiberRef[Contiguous[LogTarget]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(Contiguous.empty[LogTarget])
    }

  val minLogLevel: FiberRef[LogLevel] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(LogLevel.Info)
    }

  val logToZio: FiberRef[Boolean] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(true)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Telemetry
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class TelemetryEnv(
      targets: Contiguous[TelemetryTarget],
  )

  val telemetryTargets: FiberRef[Contiguous[TelemetryTarget]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(Contiguous.empty[TelemetryTarget])
    }

}
