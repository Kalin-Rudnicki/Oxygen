package oxygen.test.container

import java.net.{InetSocketAddress, Socket}
import oxygen.predef.core.*
import oxygen.zio.instances.given
import oxygen.zio.system.Command
import zio.*

final class TestContainerService(config: TestContainerConfig, acquiredPortsRef: Ref.Synchronized[Set[Int]]) {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val acquirePort: ZIO[Scope, TestContainerError.UnableToAcquirePort, Int] =
    Random.RandomLive
      .nextIntBetween(config.minPort, config.maxPort)
      .flatMap(helpers.attemptAcquire)
      .retry(Schedule.recurs(config.maxRandomPortAttempts))
      .orElseFail { TestContainerError.UnableToAcquirePort(config) }

  private def run(container: TestContainer): ZIO[Scope, TestContainerError, Unit] =
    helpers.start(container).withFinalizer { _ => helpers.close(container).orDie } *>
      ZIO.foreachParDiscard(container.healthChecks.to[Chunk])(helpers.executeHealthCheck(container, _)) *>
      ZIO.logInfo(s"Container ${container.name} is up, running, and healthy")

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object helpers {

    private val host: String = "localhost"

    def attemptSocketConnect(port: Int): IO[Int, Int] =
      ZIO.scoped {
        ZIO.logDebug(s"Attempting to connect to $host:$port") *>
          ZIO
            .attempt { new Socket() }
            .withFinalizerAuto
            .tap { socket => ZIO.attempt { socket.connect(new InetSocketAddress(host, port)) } }
            .foldCauseZIO(
              _ => ZIO.succeed(port),
              _ => ZIO.fail(port),
            )
      }

    def attemptAcquire(port: Int): ZIO[Scope, Int, Int] =
      acquiredPortsRef
        .modifyZIO { acquired =>
          ZIO.fail(port).whenDiscard(acquired.contains(port)) *>
            attemptSocketConnect(port).map(_ -> (acquired + port))
        }
        .withFinalizer(_ => acquiredPortsRef.update(_ - port))
        .tap { port => ZIO.logDebug(s"Acquired port: $port") }

    def start(container: TestContainer): IO[TestContainerError.UnableToStartContainer, Unit] =
      Command("docker")(
        Seq("run", "-d", "--name", container.name),
        container.params.flatMap(_.toArgs),
        s"${container.imageName}:${container.imageVersion}",
      ).executeSuccess(outLevel = LogLevel.Debug)
        .mapError(TestContainerError.UnableToStartContainer(config, container, _))

    def close(container: TestContainer): IO[TestContainerError.UnableToStopContainer, Unit] =
      Command("docker")("stop", container.name)
        .executeSuccess(outLevel = LogLevel.Debug)
        .foldCauseZIO(
          ZIO.logErrorCause("Error stopping container, going to attempt force stop", _) *> Command("docker")("rm", container.name, "--force", "--volumes").executeSuccess(outLevel = LogLevel.Debug),
          _ => Command("docker")("rm", container.name, "--volumes").executeSuccess(outLevel = LogLevel.Debug),
        )
        .mapError(TestContainerError.UnableToStopContainer(config, container, _))

    def executeHealthCheck(container: TestContainer, healthCheck: HealthCheck): IO[TestContainerError.NeverBecameHealthy, Unit] =
      ZIO
        .fail(new RuntimeException("Not ready yet"))
        .unlessZIODiscard(healthCheck.effect)
        .mapError(TestContainerError.NeverBecameHealthy(config, container, healthCheck, _))
        .retry(config.healthCheckSchedule)
        .withClock(Clock.ClockLive)

  }

}
object TestContainerService {

  val layer: URLayer[TestContainerConfig, TestContainerService] =
    ZLayer {
      for {
        config <- ZIO.service[TestContainerConfig]
        ports <- Ref.Synchronized.make(Set.empty[Int])
      } yield TestContainerService(config, ports)
    }

  val acquirePort: ZIO[TestContainerService & Scope, TestContainerError.UnableToAcquirePort, Int] =
    ZIO.serviceWithZIO[TestContainerService](_.acquirePort)

  def containerLayer[R <: TestContainerService, E >: TestContainerError, A: Tag](make: ZIO[R & Scope, E, (A, Growable[TestContainer])]): ZLayer[R, E, A] =
    ZLayer.scoped {
      for {
        service <- ZIO.service[TestContainerService]
        (a, containers) <- make
        _ <- ZIO.foreachDiscard(containers.to[Chunk])(service.run)
        _ <- ZIO.logInfo("All test containers are now available")
      } yield a
    }

  def randomAlphaString(length: Int): UIO[String] =
    Random.nextIntBetween('a', 'z').map(_.toChar).replicateZIO(length).map(_.mkString)

}
