package oxygen.mcp.domain

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import zio.*

/**
  * The stateless MCP JSON-RPC dispatcher (MCP `2026-07-28`). Routes the three request methods the
  * server supports — `server/discover`, `tools/list`, `tools/call` — over a decoded [[ServerRequest]],
  * producing either the method's result payload (as [[oxygen.json.Json]], ready to wrap in a
  * [[ServerResponse.Success]]) or a protocol [[McpError]].
  *
  * Deliberately transport-neutral and auth-free: **no** token validation lives here. The HTTP layer
  * ([[McpPrincipal]] resolution, the `401`/`403` OAuth gate, PRM) is `mcp-http`'s job; the dispatcher
  * only threads an already-resolved principal into each tool call. A tool's typed error (or an
  * argument-decode failure) is forced into a protocol [[McpError]] and flows out `Left` exactly like the
  * dispatcher's other protocol errors — so an auth error becomes a `401` at the HTTP boundary.
  */
final class McpServer(
    val tools: AppliedMcpTools,
    val config: McpServer.Config,
) {

  private val applied: ArraySeq[AppliedMcpTool] = tools.arraySeq
  private val byName: Map[String, AppliedMcpTool] = applied.iterator.map(t => t.tool.name -> t).toMap

  /** Look a hosted tool up by name (e.g. for the HTTP layer's per-tool auth gate). */
  def tool(name: String): Option[AppliedMcpTool] = byName.get(name)

  /** Dispatch a request, yielding the method result payload (`Right`) or a protocol error (`Left`). */
  def dispatch(request: API.request.ServerRequest, principal: Option[McpPrincipal]): URIO[Scope, Either[McpError, API.response.ServerResponse]] =
    request match {
      case _: API.request.ServerRequest.ServerDiscover =>
        ZIO.succeed(Right(API.response.ServerResponse.wrap(encode(discoverResult))))
      case r: API.request.ServerRequest.ToolsList =>
        versionGate(r.params.meta) match {
          case Some(err) => ZIO.succeed(Left(err))
          case None      => ZIO.succeed(Right(API.response.ServerResponse.wrap(encode(toolsListResult))))
        }
      case r: API.request.ServerRequest.ToolsCall =>
        versionGate(r.params.meta) match {
          case Some(err) => ZIO.succeed(Left(err))
          case None      => callTool(r, principal)
        }
    }

  private def callTool(r: API.request.ServerRequest.ToolsCall, principal: Option[McpPrincipal]): URIO[Scope, Either[McpError, API.response.ServerResponse]] =
    byName.get(r.params.name) match {
      case None =>
        ZIO.succeed(Left(McpError.InvalidTool(r.params.name)))
      case Some(tool) =>
        val input =
          McpToolInput(
            arguments = r.params.arguments.getOrElse(API.request.ToolArguments.wrap(Json.obj())),
            meta = r.params.meta,
            inputResponses = r.params.inputResponses,
            requestState = r.params.requestState,
            principal = principal,
          )
        tool
          .handle(input)
          .foldCauseZIO(
            cause =>
              cause.failureOption match {
                case Some(err) => ZIO.succeed(Left(err))
                case None      => ZIO.succeed(Left(McpError.InternalError(Some(cause.failureOrCause.fold(_.toString, _.prettyPrint)))))
              },
            res => ZIO.succeed(Right(API.response.ServerResponse.wrap(encode(toCallResult(res))))),
          )
    }

  private def versionGate(meta: API.request.RequestMeta): Option[McpError] =
    if config.supportedVersions.contains(meta.protocolVersion) then None
    else Some(McpError.UnsupportedProtocolVersion(meta.protocolVersion, config.supportedVersions))

  private def discoverResult: API.response.ServerDiscoverResponse =
    API.response.ServerDiscoverResponse(
      resultType = API.response.ResultType.Complete,
      supportedVersions = config.supportedVersions,
      capabilities = config.capabilities,
      instructions = config.instructions,
      serverInfo = config.serverInfo,
      ttlMs = config.discoverTtlMs,
      cacheScope = config.cacheScope,
    )

  private def toolsListResult: API.response.ListToolsResponse =
    API.response.ListToolsResponse(
      resultType = API.response.ResultType.Complete,
      tools = applied.map(_.tool).toList,
      nextCursor = None,
      ttlMs = config.listTtlMs,
      cacheScope = config.cacheScope,
    )

  private def toCallResult(res: McpToolResult): API.response.CallToolResponse =
    API.response.CallToolResponse(
      resultType = API.response.ResultType.Complete,
      content = res.content,
      structuredContent = res.structuredContent,
      isError = Some(res.isError),
    )

  private def encode[A](a: A)(using schema: JsonSchema[A]): Json =
    schema.jsonEncoder.encodeJsonAST(a)

}
object McpServer {

  /**
    * Static server configuration: the protocol versions it speaks, what it advertises on
    * `server/discover`, and the caching hints stamped on `discover`/`list` results.
    */
  final case class Config(
      supportedVersions: List[API.ProtocolVersion],
      capabilities: API.response.ServerCapabilities,
      serverInfo: Option[API.Implementation],
      instructions: Option[String],
      discoverTtlMs: Long,
      listTtlMs: Long,
      cacheScope: API.response.CacheScope,
  )

}
