package oxygen.http.server.mcp

import oxygen.http.model.internal.ReceivedRequest
import oxygen.http.server.{EndpointInput, McpInput, ServerErrorConfig}
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.zio.ExtractedCauses
import zio.*

/**
  * The stateless MCP JSON-RPC dispatcher. Routes the four methods the server supports
  * (`initialize`, `notifications/initialized`, `tools/list`, `tools/call`) and applies the OAuth gate
  * to authed `tools/call`s. It returns a transport-neutral [[McpEndpointResult]] which the
  * [[McpEndpointMiddleware]] maps to HTTP (200 / 401 / 403) — so the per-message auth model is
  * exercised here and unit-testable without a live server.
  */
final class McpServer(
    tools: ArraySeq[AppliedMcpEndpoint],
    authService: McpAuthService,
    serverInfo: McpServer.ServerInfo,
) {
  import McpServer.*

  private val toolsByName: Map[String, AppliedMcpEndpoint] = tools.iterator.map(t => t.schema.toolName -> t).toMap

  /** The RFC 9728 Protected Resource Metadata to serve, if auth is configured. */
  def protectedResourceMetadata: Option[Json] = authService.protectedResourceMetadata

  /** True if an authed tool is exposed but no real auth service is wired (a startup misconfiguration). */
  def hasAuthedToolWithoutConfig: Boolean = !authService.isConfigured && tools.exists(_.requiresAuth)

  private def initializeResult(id: Json): McpEndpointResult.InitializeResult =
    McpEndpointResult.InitializeResult(
      id,
      McpServer.protocolVersion,
      McpEndpointResult.InitializeResult.Capabilities(
        McpEndpointResult.InitializeResult.ToolCapability(supportsListChanged = false).some, // TODO (KR) : support this?
      ),
      serverInfo,
    )

  private def toolsListResult(id: Json): McpEndpointResult.ToolResult =
    McpEndpointResult.ToolResult(id, tools)

  def safeHandleRequest(in: EndpointInput): URIO[Scope, McpEndpointResult] =
    parseRequest(in.request)
      .safeMcpError(Json.Null, in.errorConfig)
      .flatMap { res =>
        mcpExecute(res.id, res.body, res.auth, in.errorConfig)
          .safeMcpError(res.id, in.errorConfig)
      }
      .merge[McpEndpointResult]

  private def parseRequest(request: ReceivedRequest): ZIO[Scope, McpEndpointResult.Failure, (id: Json, body: Json, auth: Option[String])] =
    for {
      bodyStr <- request.body.asString.orDie
      auth = request.headers.rawHeader("Authorization")
      body <- Json.parse(bodyStr) match
        case Right(value) => ZIO.succeed(value)
        case Left(error)  => ZIO.fail(McpEndpointResult.ParseError(error.getMessage))
      id = field(body, "id").getOrElse(Json.Null)
    } yield (id, body, auth)

  def mcpExecute(id: Json, body: Json, auth: Option[String], errorConfig: ServerErrorConfig): ZIO[Scope, McpEndpointResult.Failure, McpEndpointResult] =
    McpMethod.parse(id, body).flatMap {
      case McpMethod.CallTool =>
        for {
          params <- ZIO.getOrFailWith(McpEndpointResult.InvalidParam(id, "params", "Missing required field"))(field(body, "params"))
          toolName <- parseString(id, params, "name")()
          tool <- ZIO.getOrFailWith(McpEndpointResult.InvalidTool(id, toolName))(toolsByName.get(toolName))
          _ <- authGate(tool, auth)
          arguments: Json = field(params, "arguments").getOrElse(Json.obj())
          toolResult <- tool.handle(McpInput(id, arguments, auth, errorConfig))
        } yield McpEndpointResult.fromMcpToolResult(id, toolResult)
      case McpMethod.ListTools        => ZIO.succeed(toolsListResult(id))
      case McpMethod.Initialize       => ZIO.succeed(initializeResult(id))
      case McpMethod.Notifications(_) => ZIO.succeed(McpEndpointResult.NoContent)
    }

  /** Per-message auth: 401 on no/invalid token, 403 on insufficient scope, else proceed. */
  private def authGate(tool: AppliedMcpEndpoint, bearer: Option[String]): ZIO[Scope, McpEndpointResult.Failure, Unit] =
    ZIO.whenDiscard(tool.requiresAuth) { // Should this function return something?
      for {
        claims <- authService.authenticate(bearer).mapError(McpEndpointResult.Unauthorized(_))
        required: Set[String] = authService.requiredScopes
        held: Set[String] = claims.scope.map(_.values).getOrElse(Set.empty)
        missing: List[String] = (required &~ held).toList
        _ <- ZIO.foreachDiscard(NonEmptyList.fromList(missing)) { missing => ZIO.fail(McpEndpointResult.MissingScopes(missing.sorted)) }
      } yield ()
    }

  extension [R, A](self: ZIO[R, McpEndpointResult.Failure, A])
    private def safeMcpError(id: Json, errorConfig: ServerErrorConfig): ZIO[R, McpEndpointResult.Failure, A] =
      self.catchAllCause { cause =>
        ExtractedCauses.fromCause(cause) match {
          case ExtractedCauses.Failures(failures, _, _) =>
            ZIO.refailCause(failures.head)
          case causes: ExtractedCauses.NoFailures =>
            ZIO.logErrorCause("Unhandled error in MCP endpoint", cause) *>
              ZIO.fail(McpEndpointResult.InternalError(id, errorConfig.serverErrors(causes)))
        }
      }

}
object McpServer {

  /** Targeted MCP spec revision (current stable). */
  val protocolVersion: String = "2025-11-25"

  final case class ServerInfo(name: String, version: String) derives JsonEncoder

  def field(j: Json, name: String): Option[Json] =
    j match
      case Json.Obj(fields) => fields.collectFirst { case (`name`, v) => v }
      case _                => None

  def asString(j: Json): Option[String] =
    j match
      case Json.Str(s) => s.some
      case _           => None

  def parseString(id: Json, body: Json, fieldName: String)(
      onMissing: => String = "Missing required field",
      onNotString: => String = "Param must be a string",
  ): IO[McpEndpointResult.InvalidParam, String] =
    for {
      fieldJson <- ZIO.getOrFailWith(McpEndpointResult.InvalidParam(id, fieldName, onMissing))(field(body, fieldName))
      fieldString <- ZIO.getOrFailWith(McpEndpointResult.InvalidParam(id, fieldName, onNotString))(asString(fieldJson))
    } yield fieldString

}
