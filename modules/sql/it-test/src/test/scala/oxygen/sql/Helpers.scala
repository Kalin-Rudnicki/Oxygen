package oxygen.sql

import oxygen.sql.test.PostgresTestContainer
import oxygen.test.container.*
import zio.*

object Helpers {

  val testContainerLayer: Layer[TestContainerError, TestContainerService] =
    ZLayer.make[TestContainerService](
      TestContainerConfig.defaultLayer,
      TestContainerService.layer,
    )

  val databaseLayer: ZLayer[TestContainerService, TestContainerError, Database] =
    ZLayer.makeSome[TestContainerService, Database](
      ZLayer.succeed(DbConfig.Pool(8, 16, 5.minutes)),
      ZLayer.succeed(DbConfig.Logging(LogLevel.Trace, true)),
      ZLayer.succeed(DbConfig.Execution.default),
      PostgresTestContainer.layer,
      Database.layer,
    )

}
