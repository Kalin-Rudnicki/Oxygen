package oxygen.zio

import oxygen.zio.logger.*
import oxygen.zio.telemetry.*
import zio.{LogLevel as _, *}

object OxygenEnv {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Logger
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val logTargets: FiberRef[Chunk[LogTarget]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(Chunk.empty[LogTarget])
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

  val telemetryTargets: FiberRef[Chunk[TelemetryTarget]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(Chunk.empty[TelemetryTarget])
    }

}
