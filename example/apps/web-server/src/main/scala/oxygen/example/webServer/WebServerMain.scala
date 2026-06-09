package oxygen.example.webServer

import oxygen.cli.*
import oxygen.crypto.service.{HashKey, JWTService, PasswordService}
import oxygen.example.api.*
import oxygen.example.api.model.user.User
import oxygen.example.api.service.*
import oxygen.example.domain.repo.*
import oxygen.example.domain.service.*
import oxygen.example.webServer.api.{*, given}
import oxygen.executable.*
import oxygen.http.server.*
import oxygen.json.JsonCodec
import oxygen.schema.instances.jsonCodecFromSchema
import oxygen.sql.{Atomically, Database, DbConfig}
import oxygen.sql.migration.*
import oxygen.sql.migration.persistence.MigrationRepo
import zio.*

final case class WebServerMain() extends CliApp[Any, WebServerMain.Env] {

  def env(@config("APP_CONFIG") config: WebServerMain.Config): EnvLayer =
    WebServerMain.Env.layer(config)

  @execute
  def run(): Effect =
    MigrationService.migrate(oxygen.example.db.migrations.migrations) *>
      ZIO.logInfo("Press space+enter to stop server") *>
      ZIO.attempt(java.lang.System.in.read()).unit *>
      ZIO.logInfo("Stopping...")

}
object WebServerMain extends CliApp.Executable[WebServerMain] {

  final case class Config(
      http: Config.Http,
      token: Config.Token,
      db: DbConfig,
      ui: UIApiImpl.Config,
  ) derives JsonCodec
  object Config {

    final case class Http(
        errors: ServerErrorConfig,
        port: Int,
    ) derives JsonCodec

    final case class Token(
        key: HashKey,
        timeToLive: Duration,
    ) derives JsonCodec

  }

  type Env = Server & Server.Config & CompiledEndpoints & MigrationService
  object Env {

    private val endpoints: Endpoints[UserApi & ConnectionApi & PostApi & UIApi & StreamApi] =
      Endpoints.empty.add[UserApi].add[ConnectionApi].add[PostApi].add[UIApi].add[StreamApi]

    def layer(config: Config): TaskLayer[Env] =
      ZLayer.make[Env](
        // server
        ZLayer.succeed(Server.Config(config.http.errors)),
        Server.layer.simple(config.http.port),
        endpoints.toLayer,
        UserApiImpl.layer,
        ConnectionApiImpl.layer,
        PostApiImpl.layer,
        UIApiImpl.layer,
        StreamApiImpl.layer,
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
        ZLayer.succeed(JWTService.Issuer.Config(JWTService.Issuer.ValidAfter.Empty, JWTService.Issuer.Expiry.IssueDelay(config.token.timeToLive))),
        JWTService.Issuer.untypedKeyLayer[User],
        TokenService.layer,
        // repo
        PostgresUserRepo.layer,
        PostgresConnectionRepo.layer,
        PostgresPostRepo.layer,
        // service
        UserService.layer,
        ConnectionService.layer,
        PostService.layer,
        PasswordService.layer,
      )

  }

}
