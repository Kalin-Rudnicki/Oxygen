package oxygen.example.webServer

import oxygen.crypto.service.{JWTService, Key}
import oxygen.example.api.model.user.User
import oxygen.example.api.service.*
import oxygen.example.domain.repo.*
import oxygen.example.domain.service.*
import oxygen.example.webServer.api.{*, given}
import oxygen.http.server.*
import oxygen.json.JsonCodec
import oxygen.predef.executable.*
import oxygen.schema.instances.jsonCodecFromSchema
import oxygen.sql.{Atomically, Database, DbConfig}
import oxygen.sql.migration.*
import oxygen.sql.migration.persistence.MigrationRepo
import zio.*

object WebServerMain extends ExecutableApp {

  final case class Config(
      http: Config.Http,
      token: Config.Token,
      db: DbConfig,
      ui: UIApiImpl.Config,
  ) derives JsonCodec
  object Config {

    final case class Http(
        exposeInternalErrors: Boolean,
        port: Int,
    ) derives JsonCodec

    final case class Token(
        key: Key.CanIssue.Config,
        timeToLive: Duration,
    ) derives JsonCodec

  }

  type Env = Server & Server.Config & CompiledEndpoints & MigrationService
  object Env {

    def layer(config: Config): TaskLayer[Env] =
      ZLayer.make[Env](
        // server
        ZLayer.succeed(Server.Config(config.http.exposeInternalErrors)),
        Server.layer.simple(config.http.port),
        Endpoints.layer {
          _ ++
            UserApiImpl.layer ++
            ConnectionApiImpl.layer ++
            PostApiImpl.layer ++
            UIApiImpl.layer
        },
        CompiledEndpoints.endpointLayer(
        ),
        Server.layer.serving,
        ZLayer.succeed(config.ui),
        // db
        ZLayer.succeed(config.db),
        Database.layer,
        Atomically.LiveDB.layer,
        MigrationService.layer,
        MigrationRepo.layer,
        MigrationConfig.defaultLayer,
        // token
        ZLayer.succeed(config.token.key),
        ZLayer.succeed(JWTService.Issuer.Config(config.token.timeToLive)),
        JWTService.Issuer.configLayer[User],
        TokenService.layer,
        // repo
        PostgresUserRepo.layer,
        PostgresConnectionRepo.layer,
        PostgresPostRepo.layer,
        // service
        UserService.layer,
        ConnectionService.layer,
        PostService.layer,
      )

  }

  override val executable: Executable =
    Executable
      .withJsonConfig[Config]
      .withEnv[Env] { (config, _) => Env.layer(config) }
      .withExecute {
        MigrationService.migrate(oxygen.example.db.migrations.migrations) *>
          ZIO.logInfo("Press space+enter to stop server") *>
          ZIO.succeed { java.lang.System.in.read() } *>
          ZIO.logInfo("Stopping...")
      }
}
