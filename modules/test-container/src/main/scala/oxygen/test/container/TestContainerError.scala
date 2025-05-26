package oxygen.test.container

sealed trait TestContainerError extends Throwable {
  // TODO (KR) : improve
  override final def getMessage: String = toString
}
object TestContainerError {
  final case class UnableToAcquirePort(config: TestContainerConfig) extends TestContainerError
  final case class UnableToStartContainer(config: TestContainerConfig, container: TestContainer, cause: Throwable) extends TestContainerError
  final case class UnableToStopContainer(config: TestContainerConfig, container: TestContainer, cause: Throwable) extends TestContainerError
  final case class NeverBecameHealthy(config: TestContainerConfig, container: TestContainer, healthCheck: HealthCheck, cause: Throwable) extends TestContainerError
}
