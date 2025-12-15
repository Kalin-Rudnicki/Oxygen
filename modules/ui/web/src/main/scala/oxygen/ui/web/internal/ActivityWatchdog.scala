package oxygen.ui.web.internal

import org.scalajs.dom
import oxygen.predef.core.*
import oxygen.ui.web.*
import scala.scalajs.js
import zio.*

final class ActivityWatchdog private (
    runtime: Runtime[Any],
    runningJobsRef: Ref[Map[ActivityWatchdog.Job, Scope.Closeable]],
    suspendedJobsRef: Ref[Set[ActivityWatchdog.Job]],
) {

  /////// API ///////////////////////////////////////////////////////////////

  private[ui] def scheduleEffect[R, S](name: String, timeout: Duration, effect: URIO[R & Scope, Unit]): URIO[R & Scope, Unit] =
    ActivityWatchdog.Job.make(name, timeout, effect).flatMap(attemptToRunJob)

  /////// Core Logic ///////////////////////////////////////////////////////////////

  private def suspendJob(job: ActivityWatchdog.Job): UIO[Unit] =
    for {
      _ <- ZIO.logDebug(s"${job.name}.suspend")
      currentScope <- runningJobsRef.get.map(_.get(job))
      _ <- ZIO.foreachDiscard(currentScope)(_.close(Exit.unit))
      _ <- suspendedJobsRef.update(_ + job)
      _ <- ZIO.succeed { hasSuspendedJobs = true }
    } yield ()

  private def calculateSleep(job: ActivityWatchdog.Job): UIO[Option[Long]] =
    for {
      currentActivityTime <- ZIO.succeed { lastActivityTime }
      now <- ZIO.succeed { js.Date.now() }
      millisSinceLastActivity = now - currentActivityTime
      checkAgainIn = job.timeoutMillis - millisSinceLastActivity
      _ <- ZIO.logDebug(s"${job.name}.checkAgainIn : $checkAgainIn")
    } yield checkAgainIn.toLong.someWhen(_ > 0)

  private def runJob(job: ActivityWatchdog.Job): UIO[Unit] = {
    lazy val waitToPounce: UIO[Unit] =
      calculateSleep(job).flatMap {
        case Some(checkAgainIn) =>
          ZIO.logDebug(s"${job.name}.startSleeping") *>
            Clock.sleep((checkAgainIn + 1).millis) *>
            ZIO.logDebug(s"${job.name}.doneSleeping") *>
            runningJobsRef.get.map(_.contains(job)).flatMap {
              case true  => waitToPounce
              case false => ZIO.unit
            }
        case None => suspendJob(job)
      }

    for {
      _ <- ZIO.logDebug(s"${job.name}.run")
      forkedJobScope <- job.originalScope.fork
      _ <- runningJobsRef.update(_.updated(job, forkedJobScope))
      _ <- forkedJobScope.extend { job.effect.onExit { _ => ZIO.logDebug(s"${job.name}.onExit") *> runningJobsRef.update(_.removed(job)) }.forkScoped }
      _ <- waitToPounce.forkDaemon
    } yield ()
  }

  private def attemptToRunJob(job: ActivityWatchdog.Job): UIO[Unit] =
    calculateSleep(job).flatMap:
      case Some(_) => runJob(job)
      case None    => suspendJob(job)

  private def restoreSuspendedJobs: UIO[Unit] =
    for {
      _ <- ZIO.succeed { hasSuspendedJobs = false }
      suspendedJobs <- suspendedJobsRef.getAndSet(Set.empty)
      _ <- ZIO.logInfo(s"Attempting to wake up ${suspendedJobs.size} job(s)")
      _ <- ZIO.foreachDiscard(suspendedJobs)(attemptToRunJob)
    } yield ()

  /////// Yucky Mutable Efficiency Stuff ///////////////////////////////////////////////////////////////

  private var lastActivityTime: Double = js.Date.now()
  private var hasSuspendedJobs: Boolean = false

  private val listenerOptions: dom.EventListenerOptions = js.Dynamic.literal(passive = true).asInstanceOf[dom.EventListenerOptions]
  private val eventTypes: Seq[String] = Seq("mousemove", "keydown", "touchstart")
  private val onInteraction: js.Function1[dom.Event, Unit] = { (_: dom.Event) =>
    lastActivityTime = js.Date.now()
    if hasSuspendedJobs then Unsafe.unsafely { runtime.unsafe.run { restoreSuspendedJobs } }
  }

  private def addEventListeners: URIO[Scope, Unit] = {
    def unsafeAdd(): Unit =
      eventTypes.foreach { tpe => dom.window.addEventListener(tpe, onInteraction, listenerOptions) }

    def unsafeRemove(): Unit =
      eventTypes.foreach { tpe => dom.window.removeEventListener(tpe, onInteraction, listenerOptions) }

    ZIO.succeed { unsafeAdd() }.withFinalizer { _ => ZIO.succeed { unsafeRemove() } }
  }

  private def cancelAllRunningJobs: UIO[Unit] =
    runningJobsRef.getAndSet(Map.empty).flatMap { running =>
      ZIO.foreachDiscard(running.values)(_.close(Exit.unit))
    }

}
object ActivityWatchdog {

  private final case class Job private (
      name: String,
      timeoutMillis: Double,
      effect: URIO[Scope, Unit],
      originalScope: Scope,
  )
  private object Job {

    def make[R](name: String, timeout: Duration, effect: URIO[R & Scope, Unit]): URIO[R & Scope, Job] =
      for {
        rEnv <- ZIO.environment[R]
        scope <- ZIO.scope
      } yield Job(
        name = name,
        timeoutMillis = timeout.toMillis.toDouble,
        effect = effect.provideSomeEnvironment[Scope](rEnv ++ _),
        originalScope = scope,
      )

  }

  val make: RIO[Scope, ActivityWatchdog] =
    for {
      runtime <- ZIO.runtime[Any]
      runningJobsRef <- Ref.make(Map.empty[ActivityWatchdog.Job, Scope.Closeable])
      suspendedJobsRef <- Ref.make(Set.empty[ActivityWatchdog.Job])
      watcher = new ActivityWatchdog(runtime, runningJobsRef, suspendedJobsRef)
      _ <- watcher.addEventListeners
      _ <- ZIO.addFinalizer { watcher.cancelAllRunningJobs }
    } yield watcher

}
