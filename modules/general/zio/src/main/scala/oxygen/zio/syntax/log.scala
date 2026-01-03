package oxygen.zio.syntax

import oxygen.zio.*
import oxygen.zio.logging.*
import zio.*
import zio.compat.logOps

object log {

  extension (self: ZIO.type) {

    // =====|  |=====

    def logAtLevel(level: LogLevel)(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      logOps.logAtLevel(level)(message, cause)

    /**
      * Note: This is only designed to work if you are leveraging [[oxygen.zio.logging.LogConfig]].
      */
    def withMinLogLevel(level: LogLevel): ZIOAspectPoly =
      logOps.withMinLogLevel(level)

    // =====|  |=====

    def logDetailedCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      logOps.logAtLevel(LogLevels.Detailed)(message, cause)

    def logDetailedCause(cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logDetailedCause("", cause)

    def logDetailed(message: => String)(using trace: Trace): UIO[Unit] =
      ZIO.logDetailedCause(message, Cause.empty)

    def logImportantCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      logOps.logAtLevel(LogLevels.Important)(message, cause)

    def logImportantCause(cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logImportantCause("", cause)

    def logImportant(message: => String)(using trace: Trace): UIO[Unit] =
      ZIO.logImportantCause(message, Cause.empty)

    // =====|  |=====

    inline def annotated(key: String, value: String): FiberRefModification =
      FiberRef.currentLogAnnotations.modification.update(_.updated(key, value))

    inline def annotated(annotations: (String, String)*): FiberRefModification =
      FiberRef.currentLogAnnotations.modification.update(_ ++ annotations)

    inline def annotated(annotations: Map[String, String]): FiberRefModification =
      FiberRef.currentLogAnnotations.modification.update(_ ++ annotations)

    def withLogSpan(label: String): FiberRefModification =
      FiberRef.currentLogSpan.modification.update { spans =>
        LogSpan(label, java.lang.System.currentTimeMillis()) :: spans
      }

    def withLogSpans(spans: List[LogSpan]): FiberRefModification =
      FiberRef.currentLogSpan.modification.update { spans ::: _ }

    def logTraceAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logTrace(message) @@ ZIO.annotated(annotations*)

    def logDebugAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logDebug(message) @@ ZIO.annotated(annotations*)

    def logDetailedAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logDetailed(message) @@ ZIO.annotated(annotations*)

    def logInfoAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logInfo(message) @@ ZIO.annotated(annotations*)

    def logImportantAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logImportant(message) @@ ZIO.annotated(annotations*)

    def logWarningAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logWarning(message) @@ ZIO.annotated(annotations*)

    def logErrorAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logError(message) @@ ZIO.annotated(annotations*)

    def logFatalAnnotated(message: => String, annotations: (String, String)*)(using trace: Trace): UIO[Unit] =
      ZIO.logFatal(message) @@ ZIO.annotated(annotations*)

    // =====|  |=====

    def logAtLevel_debugCause(level: LogLevel)(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] = {
      lazy val message1: String = message
      ZIO.logAtLevel(level)(message1, Cause.Empty) *> ZIO.logDebugCause(message1, cause)
    }

    def logTrace_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Trace)(message, cause)

    def logDebug_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Debug)(message, cause)

    def logDetailed_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Detailed)(message, cause)

    def logInfo_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Info)(message, cause)

    def logImportant_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Important)(message, cause)

    def logWarning_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Warning)(message, cause)

    def logError_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Error)(message, cause)

    def logFatal_debugCause(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logAtLevel_debugCause(LogLevels.Fatal)(message, cause)

    // =====|  |=====

    def logCauseAtLevel_debugCauseTrace(level: LogLevel)(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] = {
      lazy val message1: String = message
      lazy val cause1: Cause[Any] = cause
      ZIO.logAtLevel(level)(message1, cause1.untraced) *> ZIO.logDebugCause(message1, cause)
    }

    def logTraceCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Trace)(message, cause)

    def logDebugCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Debug)(message, cause)

    def logDetailedCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Detailed)(message, cause)

    def logInfoCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Info)(message, cause)

    def logImportantCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Important)(message, cause)

    def logWarningCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Warning)(message, cause)

    def logErrorCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Error)(message, cause)

    def logFatalCause_debugCauseTrace(message: => String, cause: => Cause[Any])(using trace: Trace): UIO[Unit] =
      ZIO.logCauseAtLevel_debugCauseTrace(LogLevels.Fatal)(message, cause)

  }

}
