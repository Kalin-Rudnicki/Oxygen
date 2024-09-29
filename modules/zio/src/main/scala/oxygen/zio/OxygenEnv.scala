package oxygen.zio

import oxygen.zio.logger.*
import oxygen.zio.telemetry.*
import zio.{LogLevel as _, *}

object OxygenEnv {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Logging
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

  val traceTargets: FiberRef[Chunk[TraceTarget]] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(Chunk.empty[TraceTarget])
    }

}
