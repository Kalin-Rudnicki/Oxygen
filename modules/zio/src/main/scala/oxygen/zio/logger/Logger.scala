package oxygen.zio.logger

import java.time.Instant
import oxygen.predef.core.*
import oxygen.zio.*
import oxygen.zio.typeclass.ErrorLogger
import zio.{LogLevel as _, *}

object Logger {

  type LogContext = Map[String, String]
  object LogContext {

    inline def apply(logContext: Seq[(String, Any)]): LogContext =
      logContext.map { case (k, v) => k -> String.valueOf(v) }.toMap

  }

  type LogSpans = List[LogSpan]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // =====| Log |=====

  object log {

    sealed class LogAtLevel(logLevel: LogLevel) {
      def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
        Logger.execute(LogEvent(logLevel, String.valueOf(message), Logger.LogContext(context), LogCause.Empty, trace, None))
    }

    def apply(logLevel: LogLevel): LogAtLevel = new LogAtLevel(logLevel)

    def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
      Logger.execute(LogEvent(String.valueOf(message), Logger.LogContext(context), LogCause.Empty, trace, None))

    object never extends LogAtLevel(LogLevel.Never)
    object trace extends LogAtLevel(LogLevel.Trace)
    object debug extends LogAtLevel(LogLevel.Debug)
    object detailed extends LogAtLevel(LogLevel.Detailed)
    object info extends LogAtLevel(LogLevel.Info)
    object important extends LogAtLevel(LogLevel.Important)
    object warning extends LogAtLevel(LogLevel.Warning)
    object error extends LogAtLevel(LogLevel.Error)
    object fatal extends LogAtLevel(LogLevel.Fatal)
    object always extends LogAtLevel(LogLevel.Always)

  }

  object logStack {

    sealed class LogAtLevel(logLevel: LogLevel) {
      def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
        ZIO.stackTrace.flatMap { stackTrace =>
          Logger.execute(LogEvent(logLevel, String.valueOf(message), Logger.LogContext(context), LogCause.Empty, trace, stackTrace.some))
        }
    }

    def apply(logLevel: LogLevel): LogAtLevel = new LogAtLevel(logLevel)

    def apply(message: => Any, context: => (String, Any)*)(implicit trace: zio.Trace): UIO[Unit] =
      ZIO.stackTrace.flatMap { stackTrace =>
        Logger.execute(LogEvent(String.valueOf(message), Logger.LogContext(context), LogCause.Empty, trace, stackTrace.some))
      }

    object never extends LogAtLevel(LogLevel.Never)
    object trace extends LogAtLevel(LogLevel.Trace)
    object debug extends LogAtLevel(LogLevel.Debug)
    object detailed extends LogAtLevel(LogLevel.Detailed)
    object info extends LogAtLevel(LogLevel.Info)
    object important extends LogAtLevel(LogLevel.Important)
    object warning extends LogAtLevel(LogLevel.Warning)
    object error extends LogAtLevel(LogLevel.Error)
    object fatal extends LogAtLevel(LogLevel.Fatal)
    object always extends LogAtLevel(LogLevel.Always)

  }

  object logCause {

    def apply[E](causeLevel: Option[LogLevel], message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      cause.failures.map(errorLogger.logLevel).maxByOption(_.logPriority).orElse(causeLevel) match
        case Some(level) => Logger.execute(LogEvent(level, String.valueOf(message), Logger.LogContext(context), LogCause.fromZio(cause, errorLogger), trace, None))
        case None        => Logger.execute(LogEvent(String.valueOf(message), Logger.LogContext(context), LogCause.fromZio(cause, errorLogger), trace, None))

    sealed class LogCauseAtLevel(causeLevel: LogLevel) {

      def apply[E](cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCause(causeLevel.some, "", cause, context*)

      def apply[E](message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCause(causeLevel.some, message, cause, context*)

    }

    def apply[E](cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, "", cause, context*)

    def apply[E](message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, message, cause, context*)

    def apply(causeLevel: LogLevel): LogCauseAtLevel = new LogCauseAtLevel(causeLevel)

    object never extends LogCauseAtLevel(LogLevel.Never)
    object trace extends LogCauseAtLevel(LogLevel.Trace)
    object debug extends LogCauseAtLevel(LogLevel.Debug)
    object detailed extends LogCauseAtLevel(LogLevel.Detailed)
    object info extends LogCauseAtLevel(LogLevel.Info)
    object important extends LogCauseAtLevel(LogLevel.Important)
    object warning extends LogCauseAtLevel(LogLevel.Warning)
    object error extends LogCauseAtLevel(LogLevel.Error)
    object fatal extends LogCauseAtLevel(LogLevel.Fatal)
    object always extends LogCauseAtLevel(LogLevel.Always)

  }

  object logCauseStack {

    def apply[E](causeLevel: Option[LogLevel], message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      ZIO.stackTrace.flatMap { stackTrace =>
        val stackTrace2 = stackTrace.some
        cause.failures.map(errorLogger.logLevel).maxByOption(_.logPriority).orElse(causeLevel) match
          case Some(level) =>
            Logger.execute(LogEvent(level, String.valueOf(message), Logger.LogContext(context), LogCause.fromZio(cause, errorLogger), trace, stackTrace2))
          case None =>
            Logger.execute(LogEvent(String.valueOf(message), Logger.LogContext(context), LogCause.fromZio(cause, errorLogger), trace, stackTrace2))
      }

    sealed class LogCauseAtLevel(causeLevel: LogLevel) {

      def apply[E](cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCauseStack(causeLevel.some, "", cause, context*)

      def apply[E](message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
        Logger.logCauseStack(causeLevel.some, message, cause, context*)

    }

    def apply[E](cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, "", cause, context*)

    def apply[E](message: => Any, cause: Cause[E], context: => (String, Any)*)(implicit trace: zio.Trace, errorLogger: ErrorLogger[E]): UIO[Unit] =
      apply(None, message, cause, context*)

    def apply(causeLevel: LogLevel): LogCauseAtLevel = new LogCauseAtLevel(causeLevel)

    object never extends LogCauseAtLevel(LogLevel.Never)
    object trace extends LogCauseAtLevel(LogLevel.Trace)
    object debug extends LogCauseAtLevel(LogLevel.Debug)
    object detailed extends LogCauseAtLevel(LogLevel.Detailed)
    object info extends LogCauseAtLevel(LogLevel.Info)
    object important extends LogCauseAtLevel(LogLevel.Important)
    object warning extends LogCauseAtLevel(LogLevel.Warning)
    object error extends LogCauseAtLevel(LogLevel.Error)
    object fatal extends LogCauseAtLevel(LogLevel.Fatal)
    object always extends LogCauseAtLevel(LogLevel.Always)

  }

  // =====| Fiber Ref Modification |=====

  // --- Target ---

  def withTargets(targets: Chunk[LogTarget]): FiberRefModification = OxygenEnv.logTargets.modification.set(targets)
  def withTargets(targets: LogTarget*): FiberRefModification = OxygenEnv.logTargets.modification.set(Chunk.fromIterable(targets))

  def addTargets(targets: Chunk[LogTarget]): FiberRefModification = OxygenEnv.logTargets.modification.update(_ ++ targets)
  def addTargets(targets: LogTarget*): FiberRefModification = OxygenEnv.logTargets.modification.update(_ ++ Chunk.fromIterable(targets))

  // --- Context ---

  def withContext(context: Logger.LogContext): FiberRefModification = FiberRef.currentLogAnnotations.modification.set(context)
  def withContext(context: (String, Any)*): FiberRefModification = FiberRef.currentLogAnnotations.modification.set(Logger.LogContext(context))

  def addContext(context: Logger.LogContext): FiberRefModification = FiberRef.currentLogAnnotations.modification.update(_ ++ context)
  def addContext(context: (String, Any)*): FiberRefModification = FiberRef.currentLogAnnotations.modification.update(_ ++ Logger.LogContext(context))

  // --- Span ---

  def span(label: String): FiberRefModification =
    FiberRef.currentLogSpan.modification.updateUIO { spans => Clock.instant.map { instant => LogSpan(label, instant.toEpochMilli) :: spans } }

  // --- Level ---

  object level extends WithLogLevel[FiberRefModification](level => OxygenEnv.minLogLevel.modification.set(level))

  // --- Forward to Zio ---

  def withForwardToZio(forwardToZio: Boolean): FiberRefModification = OxygenEnv.logToZio.modification.set(forwardToZio)

  // --- Defaults ---

  def defaultToZio: FiberRefModification = Logger.withTargets() >>> Logger.withForwardToZio(true)
  def defaultToOxygen: FiberRefModification = Logger.withTargets(LogTarget.StdOut.defaultWithAdditionalContext) >>> Logger.withForwardToZio(false)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Execute
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def execute(e: LogEvent): UIO[Unit] =
    OxygenEnv.logToZio.getWith {
      case false =>
        for {
          now <- Clock.instant
          fiberId <- ZIO.fiberId

          oxygenMinLogLevel <- OxygenEnv.minLogLevel.get
          oxygenTargets <- OxygenEnv.logTargets.get

          logAnnotations <- ZIO.logAnnotations
          logSpans <- ZIO.logSpans
          // TODO (KR) : trace ids

          _ <- ZIO.foreachDiscard(oxygenTargets) { target =>
            handle(
              e,
              target.minLogLevel.getOrElse(oxygenMinLogLevel),
              target,
            )(
              fiberId,
              logAnnotations,
              logSpans,
              now,
            )
          }
        } yield ()
      case true =>
        for {
          now <- Clock.instant
          runtime <- ZIO.runtime[Any]
          fiberId <- ZIO.fiberId
          zioLoggers <- ZIO.loggers

          oxygenMinLogLevel <- OxygenEnv.minLogLevel.get
          oxygenTargets <- OxygenEnv.logTargets.get

          logAnnotations <- ZIO.logAnnotations
          logSpans <- ZIO.logSpans
          // TODO (KR) : trace ids

          _ <- ZIO.foreachDiscard(zioLoggers) { logger =>
            logger(
              e.trace,
              fiberId,
              e.level.getOrElse(LogLevel.Always).zioLogLevel,
              e._message,
              e.cause.toZio,
              runtime.fiberRefs,
              logSpans,
              logAnnotations,
            )

            ZIO.unit
          }
          _ <- ZIO.foreachDiscard(oxygenTargets) { target =>
            handle(
              e,
              target.minLogLevel.getOrElse(oxygenMinLogLevel),
              target,
            )(
              fiberId,
              logAnnotations,
              logSpans,
              now,
            )
          }
        } yield ()
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def handle(
      e: LogEvent,
      minLogLevel: LogLevel,
      target: LogTarget,
  )(
      fiberId: FiberId,
      logAnnotations: Logger.LogContext,
      logSpans: Logger.LogSpans,
      now: Instant,
  ): UIO[Unit] =
    e match {
      case LogEvent(Some(logLevel), _, _, _, _, _) if logLevel.logPriority < minLogLevel.tolerancePriority =>
        ZIO.unit
      case _ =>
        target.handle(ExecutedLogEvent(e.level, e.message, logAnnotations ++ e.context, logSpans, fiberId, e.cause, e.trace, e.stackTrace, now))
    }

}
