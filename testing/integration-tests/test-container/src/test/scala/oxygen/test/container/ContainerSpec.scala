package oxygen.test.container

import oxygen.predef.test.*

// TODO (KR) : move this into SQL once added
object ContainerSpec extends OxygenSpecDefault {

  final case class Config(
      username: String,
      password: String,
      database: String,
      port: Int,
  )
  object Config {

    val random: ZIO[TestContainerService & Scope, TestContainerError, Config] =
      for {
        port <- TestContainerService.acquirePort
        username <- randomString
        password <- randomString
        database <- randomString
      } yield Config(username, password, database, port)

  }

  private val randomString: UIO[String] =
    Random.nextIntBetween('a', 'z').map(_.toChar).replicateZIO(10).map(_.mkString)

  def healthCheck(config: Config): Task[Boolean] =
    Command("psql")("-c", "SELECT 1 AS query_result;", "-h", "localhost", "-p", config.port.toString, "-d", config.database, "-U", config.username)
      .envVar("PGPASSWORD", config.password)
      .executeString(errorLevel = LogLevel.Debug)
      .map(_.contains("query_result"))

  val postgresLayer: Layer[TestContainerError, Config] =
    ZLayer.make[Config](
      TestContainerConfig.defaultLayer,
      TestContainerService.layer,
      TestContainerService.containerLayer {
        for {
          config <- Config.random
          container =
            TestContainer
              .make("postgres", "postgres", "latest")
              .envVar("POSTGRES_DB", config.database)
              .envVar("POSTGRES_USER", config.username)
              .envVar("POSTGRES_PASSWORD", config.password)
              .port(config.port, 5432)
              .healthCheck("select 1")(healthCheck(config))
        } yield (config, Growable.single(container))
      },
    )

  override def testSpec: TestSpec =
    suite("ContainerSpec")(
      test("a") {
        for {
          config <- ZIO.service[Config]
          _ <- healthCheck(config)
          _ <- Command("docker")("container", "ls", "-a").executeSuccess()
        } yield assertCompletes
      }.provide(postgresLayer),
    )

  override def defaultLogLevel: LogLevel = LogLevel.Trace

}
