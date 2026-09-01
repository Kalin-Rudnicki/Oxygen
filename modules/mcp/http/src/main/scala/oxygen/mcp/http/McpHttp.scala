package oxygen.mcp.http

import oxygen.http.server.*
import oxygen.json.Json
import oxygen.mcp.api.model.RequestId
import oxygen.mcp.api.model.request.ServerRequest
import oxygen.mcp.domain.*
import oxygen.mcp.domain.model.*
import oxygen.predef.core.*
import zio.*
import zio.http.{Body, Header, MediaType, Method, Response, Status}

/**
  * The Streamable-HTTP transport for an MCP [[McpServer]] (MCP `2026-07-28`), built as hand-rolled
  * **oxygen-http** endpoints (routes in the endpoint tree) via [[HttpRoute]] — NOT a standalone
  * zio-http `Routes` and NOT an [[oxygen.http.server.EndpointMiddleware]]. Mount the returned
  * [[AppliedEndpoint]]s alongside any other oxygen-http endpoints:
  *
  *   - `POST /mcp` — the JSON-RPC endpoint (single JSON response per call; no SSE). Decodes with
  *     [[McpCodec]], runs the OAuth gate ([[McpHttp.Auth]]), dispatches, renders. A protocol error is
  *     HTTP `200` with a JSON-RPC error body; auth failures are a real HTTP `401`/`403` with a
  *     `WWW-Authenticate` challenge (clients require a real 401 — never a 200, never a 404 on a
  *     discovery path).
  *   - `GET /.well-known/oauth-protected-resource` (+ the path-suffixed variant) — RFC 9728 PRM, only
  *     when a [[ProtectedResourceMetadata]] is configured.
  *
  * A tool that `requiresAuth` while no [[McpTokenVerifier]] is configured answers `401` per request
  * (rather than failing at construction) — surfacing the misconfiguration without a hard crash.
  */
final case class McpHttp(
    server: McpServer,
    auth: McpHttp.Auth,
    config: McpHttp.Config,
) {

  private val resourceMetadataUrl: String = "/" + config.resourceMetadataPath.mkString("/")
  private val resourceMetadataPathSuffixed: List[String] = config.resourceMetadataPath ++ config.mcpPath

  /** The MCP HTTP surface as oxygen-http endpoints, ready to mount on a server. */
  def endpoints: Growable[AppliedEndpoint] =
    Growable.single(mcpEndpoint) ++
      Growable.many(auth.protectedResourceMetadata.toList.flatMap(prmEndpoints))

  private def mcpEndpoint: AppliedEndpoint =
    HttpRoute(
      apiName = "mcp".some,
      endpointName = "rpc",
      method = Method.POST,
      path = config.mcpPath,
      doc = "MCP Streamable-HTTP JSON-RPC endpoint.".some,
    ) { input =>
      input.request.body.asString.orDie.flatMap { body =>
        handleRpc(body, bearerToken(input.request.headers.rawHeader("Authorization")))
      }
    }

  private def prmEndpoints(prm: ProtectedResourceMetadata): List[AppliedEndpoint] = {
    def prmEndpoint(name: String, path: List[String]): AppliedEndpoint =
      HttpRoute(
        apiName = "mcp".some,
        endpointName = name,
        method = Method.GET,
        path = path,
        doc = "RFC 9728 Protected Resource Metadata.".some,
      )(_ => ZIO.succeed(jsonResponse(prm.toJson)))

    List(
      prmEndpoint("protectedResourceMetadata", config.resourceMetadataPath),
      prmEndpoint("protectedResourceMetadataSuffixed", resourceMetadataPathSuffixed),
    )
  }

  /**
    * Extract the raw bearer token from an `Authorization` header value: strip the `Bearer ` scheme
    * (case-insensitive) if present, trim, and drop an empty result — so a [[McpTokenVerifier]] always
    * receives the stripped, non-empty token its contract promises (and a blank/`"Bearer "` header maps
    * to `None`, i.e. a missing token).
    */
  private def bearerToken(header: Option[String]): Option[String] =
    header.map(_.trim).flatMap { raw =>
      val token = if raw.regionMatches(true, 0, "Bearer ", 0, 7) then raw.substring(7).trim else raw
      Option.when(token.nonEmpty)(token)
    }

  // ---- request handling ----

  private def handleRpc(bodyStr: String, bearer: Option[String]): URIO[Scope, Response] =
    McpCodec.decodeRequest(bodyStr) match {
      case Left(err) =>
        ZIO.succeed(jsonResponse(McpCodec.renderResponse(McpCodec.recoverId(bodyStr), Left(err))))
      case Right(req) =>
        authGate(req, bearer).flatMap {
          case Left(err)        => ZIO.succeed(renderOutcome(req.id, err))
          case Right(principal) => server.dispatch(req, principal).map(outcome => jsonResponse(McpCodec.renderResponse(req.id, outcome)))
        }
    }

  /** Resolve the caller: authed tools require a valid, sufficiently-scoped token; others pass through with no principal. */
  private def authGate(req: ServerRequest, bearer: Option[String]): URIO[Scope, Either[McpError, Option[McpPrincipal]]] =
    req match {
      case tc: ServerRequest.ToolsCall =>
        server.tool(tc.params.name) match {
          case Some(tool) if tool.requiresAuth =>
            auth.verifier match {
              case None =>
                ZIO.succeed(Left(McpError.Unauthorized("authentication is not configured")))
              case Some(verifier) =>
                bearer match {
                  case None =>
                    ZIO.succeed(Left(McpError.Unauthorized("missing bearer token")))
                  case Some(raw) =>
                    verifier.verify(raw).either.map {
                      case Left(failure) =>
                        Left(McpError.Unauthorized(failure))
                      case Right(principal) =>
                        val missing = (auth.scopePolicy.requiredScopes(tc.params.name) -- principal.scopes).toList.sorted
                        if missing.isEmpty then Right(Some(principal))
                        else Left(McpError.Forbidden(missing))
                    }
                }
            }
          case _ =>
            ZIO.succeed(Right(None))
        }
      case _ =>
        ZIO.succeed(Right(None))
    }

  // ---- HTTP rendering ----

  private def jsonResponse(json: Json): Response =
    Response(status = Status.Ok, body = Body.fromCharSequence(json.showCompact))
      .addHeader(Header.ContentType(MediaType.application.json))

  private def bearerChallenge(error: String): String =
    if auth.protectedResourceMetadata.isDefined then s"""Bearer resource_metadata="$resourceMetadataUrl", error="$error""""
    else s"""Bearer error="$error""""

  private def renderOutcome(id: RequestId, err: McpError): Response =
    err.kind match {
      case McpError.Kind.Protocol =>
        jsonResponse(McpCodec.renderResponse(id, Left(err)))
      case McpError.Kind.Unauthorized =>
        Response(status = Status.Unauthorized, body = Body.fromCharSequence(err.message))
          .addHeader(Header.ContentType(MediaType.text.plain))
          .addHeader("WWW-Authenticate", bearerChallenge("invalid_token"))
      case McpError.Kind.Forbidden =>
        Response(status = Status.Forbidden, body = Body.fromCharSequence(err.message))
          .addHeader(Header.ContentType(MediaType.text.plain))
          .addHeader("WWW-Authenticate", s"""Bearer error="insufficient_scope", resource_metadata="$resourceMetadataUrl"""")
    }

}
object McpHttp {

  /** The OAuth posture of the server. [[unauthenticated]] is the zero-config default. */
  final case class Auth(
      verifier: Option[McpTokenVerifier],
      scopePolicy: ScopePolicy,
      protectedResourceMetadata: Option[ProtectedResourceMetadata],
  )
  object Auth {

    /** No verifier: identity passthrough — no PRM, no 401, no scope checks. */
    val unauthenticated: Auth =
      Auth(None, ScopePolicy.none, None)

    /** Bearer auth with a verifier + PRM, no scope requirement. */
    def bearer(verifier: McpTokenVerifier, prm: ProtectedResourceMetadata): Auth =
      Auth(verifier.some, ScopePolicy.none, prm.some)

    /** Bearer auth with a verifier + PRM + a scope policy. */
    def bearer(verifier: McpTokenVerifier, prm: ProtectedResourceMetadata, scopePolicy: ScopePolicy): Auth =
      Auth(verifier.some, scopePolicy, prm.some)

  }

  final case class Config(mcpPath: List[String], resourceMetadataPath: List[String])
  object Config {
    val default: Config = Config(List("mcp"), List(".well-known", "oauth-protected-resource"))
  }

  /** An unauthenticated MCP server's endpoints on the default paths. */
  def endpoints(server: McpServer): Growable[AppliedEndpoint] =
    McpHttp(server, Auth.unauthenticated, Config.default).endpoints

  def endpoints(server: McpServer, auth: Auth): Growable[AppliedEndpoint] =
    McpHttp(server, auth, Config.default).endpoints

  def endpoints(server: McpServer, auth: Auth, config: Config): Growable[AppliedEndpoint] =
    McpHttp(server, auth, config).endpoints

}
