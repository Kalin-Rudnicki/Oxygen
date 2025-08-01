package oxygen.test.container

sealed trait TestContainerError extends Throwable {
  override final def getMessage: String = this match
    case TestContainerError.UnableToAcquirePort(config)                               => s"Unable to acquire port, config=$config"
    case TestContainerError.UnableToStartContainer(config, container, cause)          => s"Unable to start container, config=$config, container=$container, cause=$cause"
    case TestContainerError.UnableToStopContainer(config, container, cause)           => s"Unable to stop container, config=$config, container=$container, cause=$cause"
    case TestContainerError.NeverBecameHealthy(config, container, healthCheck, cause) => s"Never became healthy, healthCheck=$healthCheck, config=$config, container=$container, cause=$cause"
}
object TestContainerError {
  final case class UnableToAcquirePort(config: TestContainerConfig) extends TestContainerError
  final case class UnableToStartContainer(config: TestContainerConfig, container: TestContainer, cause: Throwable) extends TestContainerError
  final case class UnableToStopContainer(config: TestContainerConfig, container: TestContainer, cause: Throwable) extends TestContainerError
  final case class NeverBecameHealthy(config: TestContainerConfig, container: TestContainer, healthCheck: HealthCheck, cause: Throwable) extends TestContainerError
}
