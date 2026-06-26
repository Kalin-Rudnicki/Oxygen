package oxygen.http.server.mcp

import oxygen.http.core.BodyUtil
import oxygen.http.schema.*
import oxygen.http.server.*
import oxygen.json.Json
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import zio.*
import zio.http.{Server as _, *}

/**
  * An [[EndpointMiddleware]] that appends the MCP server's HTTP surface to a set of endpoints:
  *
  *   - `POST /mcp` — the Streamable-HTTP JSON-RPC endpoint (single JSON response per call; no SSE).
  *     Reads the bearer from `Authorization`, delegates to [[McpServer]], and maps the transport-neutral
  *     result to HTTP: `Respond` → 200, notification → 202, `Unauthorized` → 401 + `WWW-Authenticate`,
  *     `Forbidden` → 403 + `WWW-Authenticate` (`insufficient_scope`).
  *   - `GET /.well-known/oauth-protected-resource` — RFC 9728 Protected Resource Metadata, appended
  *     only when auth is configured.
  *
  * Fails fast (`ZIO.die`) at startup if an `@mcp.auth` tool is exposed while `McpAuthService.NoAuth`
  * is wired.
  */
final case class McpEndpointMiddleware(
    config: McpEndpointMiddleware.Config,
    serverInfo: McpServer.ServerInfo,
    authService: McpAuthService,
) extends EndpointMiddleware {

  override def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints] = {
    val evalEndpoints: ArraySeq[AppliedEndpoint] = endpoints.arraySeq
    val evalMcpEndpoints: ArraySeq[AppliedMcpEndpoint] = evalEndpoints.flatMap(_.mcp)
    val evalEndpointsGrowable: Growable[AppliedEndpoint] = Growable.WrappedArraySeq(evalEndpoints)
    val server: McpServer = McpServer(evalMcpEndpoints, authService, serverInfo)

    if evalMcpEndpoints.isEmpty then
      ZIO.logWarning("HTTP server provided McpEndpointMiddleware, but has no MCP endpoints").as(AppliedEndpoints(evalEndpointsGrowable))
    else if server.hasAuthedToolWithoutConfig then
      ZIO.die(new IllegalStateException("MCP: an @mcp.auth tool is exposed but McpAuthService.NoAuth is configured — wire an McpAuthService.Live"))
    else
      ZIO.succeed {
        val extras: List[AppliedEndpoint] = makeMcpEndpoint(server) :: server.protectedResourceMetadata.map(makePrmEndpoint).toList
        AppliedEndpoints(endpoints.endpoints ++ Growable.many(extras))
      }
  }

  private val resourceMetadataUrl: String = "/" + config.resourceMetadataPath.mkString("/")

  private def resultToResponse(result: McpEndpointResult): Response =
    result match {
      case result: McpEndpointResult.Basic =>
        // The Streamable-HTTP client rejects a response whose Content-Type is not application/json
        // (or text/event-stream), so set it explicitly — zio-http does not serialize Body.contentType
        // into a header here.
        Response(status = Status.Ok, body = BodyUtil.fromString(result.jsonString, MediaType.application.json))
          .addHeader(Header.ContentType(MediaType.application.json))
      case McpEndpointResult.NoContent              => Response.status(Status.Accepted)
      case response: McpEndpointResult.Unauthorized =>
        Response(status = Status.Unauthorized, body = BodyUtil.fromString(response.message))
          .addHeader(Header.ContentType(MediaType.text.plain))
          .addHeader("WWW-Authenticate", s"""Bearer resource_metadata="$resourceMetadataUrl", error="invalid_token"""")
      case response: McpEndpointResult.MissingScopes =>
        Response(status = Status.Forbidden, body = BodyUtil.fromString(response.message))
          .addHeader(Header.ContentType(MediaType.text.plain))
          .addHeader("WWW-Authenticate", s"""Bearer error="insufficient_scope", resource_metadata="$resourceMetadataUrl"""")
    }

  private def constPathRequestSchema(method: Method, path: List[String]): RequestSchema =
    RequestSchema(
      method = method.some,
      paths = NonEmptyList.one(RequestPathsSchema(path.map(RequestPathsSchema.Const(_)).toArraySeq, None)),
      queryParams = ArraySeq.empty,
      headers = ArraySeq.empty,
      body = RequestBodySchema.Empty,
    )

  private def makeMcpEndpoint(server: McpServer): AppliedEndpoint = {
    AppliedEndpoint(
      schema = EndpointSchema(
        apiName = "oxygen-mcp".some,
        endpointName = "mcp",
        requestSchema = constPathRequestSchema(Method.POST, config.mcpPath),
        successResponseSchema = ResponseSchema(ExpectedStatuses.Exact(Status.Ok), ArraySeq.empty, ResponseBodySchema.Empty),
        errorResponseSchema = ResponseSchema(ExpectedStatuses.None, ArraySeq.empty, ResponseBodySchema.Empty),
        doc = "MCP (Model Context Protocol) JSON-RPC endpoint.".some,
        mcp = None,
      ),
      handle = input =>
        Option.when(input.request.method == Method.POST && input.request.fullPath == config.mcpPath) {
          server.safeHandleRequest(input).map(resultToResponse).asSome
        },
      mcp = None,
    )
  }

  private def makePrmEndpoint(prm: Json): AppliedEndpoint = {
    val response: Response = Response(status = Status.Ok, body = BodyUtil.fromString(prm.showCompact, MediaType.application.json))
    AppliedEndpoint(
      schema = EndpointSchema(
        apiName = "oxygen-mcp".some,
        endpointName = "protectedResourceMetadata",
        requestSchema = constPathRequestSchema(Method.GET, config.resourceMetadataPath),
        successResponseSchema = ResponseSchema(ExpectedStatuses.Exact(Status.Ok), ArraySeq.empty, ResponseBodySchema.Empty),
        errorResponseSchema = ResponseSchema(ExpectedStatuses.None, ArraySeq.empty, ResponseBodySchema.Empty),
        doc = "OAuth 2.0 Protected Resource Metadata (RFC 9728).".some,
        mcp = None,
      ),
      handle = input => Option.when(input.request.method == Method.GET && input.request.fullPath == config.resourceMetadataPath)(ZIO.succeed(response.some)),
      mcp = None,
    )
  }

}
object McpEndpointMiddleware {

  val layer: URLayer[McpEndpointMiddleware.Config & McpServer.ServerInfo & McpAuthService, McpEndpointMiddleware] =
    ZLayer.fromFunction { McpEndpointMiddleware.apply }

  def middleware: Middlewares[McpEndpointMiddleware.Config & McpServer.ServerInfo & McpAuthService] =
    Middlewares.endpointMiddlewareFromZLayer(layer)

  def middleware(serverName: String, serverVersion: String = "1.0.0"): Middlewares[McpEndpointMiddleware.Config & McpAuthService] =
    Middlewares.endpointMiddlewareFromZLayer(ZLayer.succeed(McpServer.ServerInfo(serverName, serverVersion)) >>> layer)

  def defaultMiddleware: Middlewares[McpServer.ServerInfo & McpAuthService] =
    Middlewares.endpointMiddlewareFromZLayer(ZLayer.succeed(Config.default) >>> layer)

  def defaultMiddleware(serverName: String, serverVersion: String = "1.0.0"): Middlewares[McpAuthService] =
    Middlewares.endpointMiddlewareFromZLayer(ZLayer.succeedEnvironment(ZEnvironment(Config.default, McpServer.ServerInfo(serverName, serverVersion))) >>> layer)

  final case class Config(
      mcpPath: List[String],
      resourceMetadataPath: List[String],
  ) derives JsonSchema
  object Config {

    val default: Config =
      Config(
        mcpPath = List("mcp"),
        resourceMetadataPath = List(".well-known", "oauth-protected-resource"),
      )

  }

}
