package oxygen.zio

import oxygen.zio.logger.*
import zio.{LogLevel as _, *}

object OxygenEnv {

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

}
