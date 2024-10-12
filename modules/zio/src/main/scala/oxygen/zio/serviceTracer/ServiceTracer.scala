package oxygen.zio.serviceTracer

import oxygen.zio.*
import zio.*

object ServiceTracer {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def trace(closure: TraceClosure): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ServiceTracer.runStateRef.getWith {
          case RunState.Disabled | RunState.Enabled => effect
          case current: RunState.Tracing =>
            (for {
              fiberId <- ZIO.fiberId
              newState <- current.toTracing
              start <- Clock.ClockLive.instant
              exit <- ServiceTracer.runStateRef.locally(newState)(effect.interruptible.exit)
              end <- Clock.ClockLive.instant

              _ <- handleEvent(TraceElem.Start(newState.traceIds, closure, fiberId.threadName, start))
              _ <- handleEvent(TraceElem.End(newState.traceIds.id, end, Outcome.fromExit(exit)))
              res <- exit
            } yield res).uninterruptible
        }
    }

  def traceRoot: ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ServiceTracer.runStateRef.getWith {
          case RunState.Disabled             => effect
          case current: RunState.NotDisabled => current.toTracing.flatMap(ServiceTracer.runStateRef.locally(_)(effect))
        }
    }

  def traceScoped(closure: TraceClosure): URIO[Scope, Unit] =
    ServiceTracer.runStateRef.getWith {
      case RunState.Enabled | RunState.Disabled => ZIO.unit
      case current: RunState.Tracing =>
        (for {
          fiberId <- ZIO.fiberId
          newState <- current.toTracing
          _ <- ZIO.addFinalizerExit { exit =>
            ServiceTracer.runStateRef.update { s => if (s == newState) current else s } *>
              Clock.ClockLive.instant.flatMap { end =>
                handleEvent(TraceElem.End(newState.traceIds.id, end, Outcome.fromExit(exit)))
              }
          }
          _ <- ServiceTracer.runStateRef.set(newState)
          start <- Clock.ClockLive.instant
          _ <- handleEvent(TraceElem.Start(newState.traceIds, closure, fiberId.threadName, start))
        } yield ()).uninterruptible
    }

  /**
    * For use when entering the system from another system.
    * Eg: HTTP, Kafka, etc.
    */
  def setParent(traceIds: TraceIds): ZIOAspectPoly =
    new ZIOAspectPoly.Impl {
      override def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        ServiceTracer.runStateRef.getWith {
          case RunState.Disabled   => effect
          case RunState.Enabled    => ServiceTracer.runStateRef.locally(RunState.Tracing(traceIds)) { effect }
          case RunState.Tracing(_) => ZIO.logInfo("Attempted to set trace parent. Already in a tracing state. Ignoring specified parent.") *> effect
        }
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val runStateRef: FiberRef[RunState] =
    Unsafe.unsafely {
      FiberRef.unsafe.make(
        RunState.Disabled,
      )
    }

  private def handleEvent(e: TraceElem): UIO[Unit] =
    OxygenEnv.serviceTracerTargets.getWith(ZIO.foreachDiscard(_)(_.handle(e)))

}
