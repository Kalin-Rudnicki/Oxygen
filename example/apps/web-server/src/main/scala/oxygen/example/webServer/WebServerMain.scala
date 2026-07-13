package oxygen.example.webServer

import oxygen.cli.*
import oxygen.crypto.service.{HashKey, JWTService, PasswordService}
import oxygen.example.api.*
import oxygen.example.api.model.user.User
import oxygen.example.api.service.*
import oxygen.example.db.ExampleSchema
import oxygen.example.domain.repo.*
import oxygen.example.domain.service.*
import oxygen.example.webServer.api.{*, given}
import oxygen.example.webServer.mcp.{NoteApi, NoteApiImpl}
import oxygen.executable.*
import oxygen.http.api.{*, given}
import oxygen.http.server.*
import oxygen.http.server.mcp.{McpAuthService, McpEndpointMiddleware}
import oxygen.json.JsonCodec
import oxygen.payments.stripe.service.LiveStripeService
import oxygen.schema.instances.jsonCodecFromSchema
import oxygen.sql.{Database, DbConfig}
import oxygen.sql.migration.*
import zio.*

final case class WebServerMain() extends CliApp[Any, WebServerMain.Env] {

  def env(@envConfig("APP_CONFIG") config: WebServerMain.Config): EnvLayer =
    WebServerMain.Env.layer(config)

  @execute
  def run(): Effect =
    ZIO.never

}
object WebServerMain extends CliApp.Executable[WebServerMain](CliApp.derive) {

  final case class Config(
      http: Config.Http,
      token: Config.Token,
      db: DbConfig,
      migration: MigrationConfig,
      ui: LiveUIApi.Config,
      resources: FileSystemResourceApi.Config,
      stripe: LiveStripeService.LiveOrUnimplementedConfig,
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

  type Env = Server & Server.Config & CompiledEndpoints
  object Env {

    // A tiny in-memory, auth-less API (NoteApi) exposed over HTTP *and* as MCP tools. Its impl is a
    // normal Ref-backed ZLayer; the MCP middleware resolves it (and the McpAuthService) from the env.
    private val endpoints: Endpoints[UIApi & ResourceApi & UserApi & ConnectionApi & PostApi & StreamApi & NoteApi] =
      Endpoints.empty.add[UserApi].add[ConnectionApi].add[PostApi].add[StreamApi].add[NoteApi].add[UIApi].add[ResourceApi]

    private val middlewares: Middlewares[McpAuthService] =
      McpEndpointMiddleware.defaultMiddleware("oxygen-example") >>>
        ApiSpecEndpointMiddleware.defaultMiddleware

    def layer(config: Config): TaskLayer[Env] =
      ZLayer.make[Env](
        // stripe
        ZLayer.succeed(config.stripe),
        LiveStripeService.liveOrUnimplementedLayer,
        // server
        ZLayer.succeed(Server.Config(config.http.errors)),
        ZLayer.succeed(config.ui),
        ZLayer.succeed(config.resources),
        Server.layer.simple(config.http.port),
        endpoints.toLayer,
        UserApiImpl.layer,
        ConnectionApiImpl.layer,
        PostApiImpl.layer,
        FileSystemResourceApi.layer,
        LiveUIApi.layer.withoutCustomUIConfig,
        StreamApiImpl.layer,
        NoteApiImpl.layer,
        ZLayer.succeed[McpAuthService](McpAuthService.NoAuth),
        middlewares.toLayer,
        CompiledEndpoints.layer,
        Server.layer.serving,
        // db
        ZLayer.succeed(config.db),
        Database.layer >>> MigrationService.migrateVerifiedLayer(ExampleSchema.schema),
        ZLayer.succeed(config.migration),
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
