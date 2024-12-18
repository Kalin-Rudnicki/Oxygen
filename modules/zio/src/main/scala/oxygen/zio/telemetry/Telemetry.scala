package oxygen.zio.telemetry

import oxygen.json.KeyedMapDecoder
import oxygen.zio.*
import zio.{LogLevel as _, *}
import zio.json.JsonDecoder
import zio.json.ast.Json

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

  def withTargets(targets: Chunk[TelemetryTarget]): FiberRefModification = OxygenEnv.telemetryTargets.modification.set(targets)
  def withTargets(targets: TelemetryTarget*): FiberRefModification = OxygenEnv.telemetryTargets.modification.set(Chunk.fromIterable(targets))

  def addTargets(targets: Chunk[TelemetryTarget]): FiberRefModification = OxygenEnv.telemetryTargets.modification.update(_ ++ targets)
  def addTargets(targets: TelemetryTarget*): FiberRefModification = OxygenEnv.telemetryTargets.modification.update(_ ++ Chunk.fromIterable(targets))

  // --- Env ---

  def env(env: OxygenEnv.TelemetryEnv): FiberRefModification =
    Telemetry.withTargets(env.targets)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def handleTrace(e: TraceEntry): UIO[Unit] =
    OxygenEnv.telemetryTargets.getWith {
      ZIO.foreachDiscard(_)(_.handle(e))
    }

  final case class Config(
      targets: Json,
  ) derives JsonDecoder {

    def decodeTargets(decoder: KeyedMapDecoder[TelemetryTarget.ConfigBuilder]): ZIO[Scope, String | Throwable, Chunk[TelemetryTarget]] =
      ZIO.fromEither(decoder.decoder.decodeJson(targets.toString())).flatMap(ZIO.foreach(_)(_.telemetryTarget)).map(_.flatten)

  }

}
