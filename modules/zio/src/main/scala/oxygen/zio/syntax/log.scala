package oxygen.zio.syntax

import oxygen.zio.*
import oxygen.zio.logging.*
import zio.*
import zio.compat.logOps

object log {

  implicit class ZIOLogOps(self: ZIO.type) {

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

  }

}
