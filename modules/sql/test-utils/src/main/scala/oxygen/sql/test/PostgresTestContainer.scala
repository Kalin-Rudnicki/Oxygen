package oxygen.sql.test

import oxygen.predef.core.*
import oxygen.predef.zio.*
import oxygen.sql.DbConfig
import oxygen.test.container.*
import zio.*

object PostgresTestContainer {

  val layer: ZLayer[TestContainerService & DbConfig.Pool & DbConfig.Logging & DbConfig.Execution, TestContainerError, DbConfig] =
    TestContainerService.containerLayer {
      for {
        pool <- ZIO.service[DbConfig.Pool]
        logging <- ZIO.service[DbConfig.Logging]
        execution <- ZIO.service[DbConfig.Execution]

        port <- TestContainerService.acquirePort
        username <- TestContainerService.randomAlphaString(10)
        password <- TestContainerService.randomAlphaString(10)
        database <- TestContainerService.randomAlphaString(10)

        dbConfig = DbConfig(DbConfig.Target(database, "localhost", port), DbConfig.Credentials(username, password).some, pool, logging, execution)
        container =
          TestContainer
            .make("postgres", "postgres", "latest")
            .envVar("POSTGRES_DB", database)
            .envVar("POSTGRES_USER", username)
            .envVar("POSTGRES_PASSWORD", password)
            .port(port, 5432)
            .healthCheck("select 1") {
              Command("psql")("-c", "SELECT 1 AS query_result;", "-h", "localhost", "-p", port.toString, "-d", database, "-U", username)
                .envVar("PGPASSWORD", password)
                .executeString(errorLevel = LogLevel.Debug)
                .map(_.contains("query_result"))
            }
      } yield (dbConfig, Growable.single(container))
    }

}
