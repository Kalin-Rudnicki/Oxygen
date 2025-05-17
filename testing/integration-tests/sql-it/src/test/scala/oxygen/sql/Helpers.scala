package oxygen.sql

import oxygen.predef.zio.*
import oxygen.test.container.*
import oxygen.test.container.sql.PostgresTestContainer

object Helpers {

  val testContainerLayer: Layer[TestContainerError, TestContainerService] =
    ZLayer.make[TestContainerService](
      TestContainerConfig.defaultLayer,
      TestContainerService.layer,
    )

  val databaseLayer: ZLayer[TestContainerService, TestContainerError, Database] =
    ZLayer.makeSome[TestContainerService, Database](
      ZLayer.succeed(DbConfig.Pool(10, 20, 5.minutes)),
      ZLayer.succeed(DbConfig.Logging(LogLevel.Trace, true)),
      PostgresTestContainer.layer,
      Database.layer,
    )

}
