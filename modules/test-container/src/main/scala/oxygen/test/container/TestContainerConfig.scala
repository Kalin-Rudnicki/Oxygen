package oxygen.test.container

import _root_.oxygen.zio.Schedules.*
import zio.*

final case class TestContainerConfig(
    minPort: Int,
    maxPort: Int,
    maxRandomPortAttempts: Int,
    healthCheckSchedule: Schedule[Any, Any, Any],
)
object TestContainerConfig {

  val default: TestContainerConfig =
    TestContainerConfig(
      minPort = 6000,
      maxPort = 7000,
      maxRandomPortAttempts = 100,
      healthCheckSchedule = Schedule.fibonacci(100.millis).withTimeout(15.seconds),
    )

  val defaultLayer: ULayer[TestContainerConfig] =
    ZLayer.succeed(default)

}
