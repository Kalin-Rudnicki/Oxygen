package oxygen.zio.telemetry

import oxygen.predef.core.*
import oxygen.predef.json.*
import oxygen.zio.*
import oxygen.zio.syntax.seq.*
import zio.{LogLevel as _, *}

object Telemetry {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def traced(key: String, args: (String, Any)*): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        (for {
          start <- Clock.ClockLive.instant
          exit <- effect.interruptible.exit
          end <- Clock.ClockLive.instant
          _ <- handleTrace(TraceEntry(key, args.map { case (k, v) => k -> String.valueOf(v) }.toMap, start, end, Outcome.fromExit(exit)))
          res <- exit
        } yield res).uninterruptible
    }

  def traceScoped(key: String, args: (String, Any)*): URIO[Scope, Unit] =
    Clock.ClockLive.instant.flatMap { start =>
      ZIO.addFinalizerExit { exit =>
        Clock.ClockLive.instant.flatMap { end =>
          handleTrace(TraceEntry(key, args.map { case (k, v) => k -> String.valueOf(v) }.toMap, start, end, Outcome.fromExit(exit)))
        }
      }.unit
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Fiber Ref Modification
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // --- Target ---

  def withTargets(targets: Contiguous[TelemetryTarget]): FiberRefModification = OxygenEnv.telemetryTargets.modification.set(targets)
  def withTargets(targets: TelemetryTarget*): FiberRefModification = OxygenEnv.telemetryTargets.modification.set(Contiguous.from(targets))

  def addTargets(targets: Contiguous[TelemetryTarget]): FiberRefModification = OxygenEnv.telemetryTargets.modification.update(_ ++ targets)
  def addTargets(targets: TelemetryTarget*): FiberRefModification = OxygenEnv.telemetryTargets.modification.update(_ ++ Contiguous.from(targets))

  // --- Env ---

  def env(env: OxygenEnv.TelemetryEnv): FiberRefModification =
    Telemetry.withTargets(env.targets)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def handleTrace(e: TraceEntry): UIO[Unit] =
    OxygenEnv.telemetryTargets.getWith {
      _.foreachZIO(_.handle(e))
    }

  final case class Config(
      targets: Json,
  ) derives JsonDecoder {

    def decodeTargets(decoder: KeyedMapDecoder[TelemetryTarget.ConfigBuilder]): ZIO[Scope, Throwable, Contiguous[TelemetryTarget]] =
      ZIO.fromEither(decoder.decoder.decodeJsonAST(targets)).flatMap(_.flatMapZIO(_.telemetryTarget))

  }

}
