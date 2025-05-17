package zio.compat

import oxygen.core.collection.Contiguous
import oxygen.zio.*
import oxygen.zio.logging.OxygenZLogger
import zio.{Cause, Exit, FiberRef, LogLevel, Runtime, Trace, UIO, ZIO}

object logOps {

  def withLoggers(loggers: Contiguous[OxygenZLogger]): FiberRefModification =
    FiberRef.currentLoggers.modification.update { l =>
      (l -- Runtime.defaultLoggers).filter {
        case _: OxygenZLogger => false
        case _                => true
      } ++ loggers.toSeq.toSet
    }

  def withMinLogLevel(level: LogLevel): FiberRefModification =
    FiberRef.currentLoggers.modification.update {
      _.map {
        case logger: OxygenZLogger => logger.withMinLevel(level)
        case logger                => logger
      }
    }

  def logAtLevel(level: LogLevel)(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
    ZIO.withFiberRuntime[Any, Nothing, Unit] { (fiberState, _) =>
      fiberState.log(() => message, cause, Some(level), trace)

      Exit.unit
    }

}
