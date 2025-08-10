package oxygen.test.container

import zio.*

final case class HealthCheck(name: String, effect: Task[Boolean])
