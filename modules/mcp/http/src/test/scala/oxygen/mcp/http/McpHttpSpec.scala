package oxygen.mcp.http

import oxygen.core.collection.Growable
import oxygen.http.model.internal.ReceivedRequest
import oxygen.http.server.*
import oxygen.json.*
import oxygen.mcp.api.model.*
import oxygen.mcp.api.model.response.*
import oxygen.mcp.domain.*
import oxygen.mcp.domain.model.*
import oxygen.predef.test.*
import zio.*
import zio.http.*

object McpHttpSpec extends OxygenSpecDefault {

  private trait Api { def echo(msg: String): String }
  private object LiveApi extends Api { override def echo(msg: String): String = msg }

  private def tool(name: String, requiresAuth: Boolean): McpTool[Api] =
    McpTool[Api](
      tool = Tool(name, None, Some("echo"), ToolInputSchema.wrap(Json.obj("type" -> Json.string("object"))), None, None, None),
      requiresAuth = requiresAuth,
      handle = api => _ => ZIO.succeed(McpToolResult.text(api.echo(name))),
    )

  private val server: McpServer =
    McpServer(
      tools = AppliedMcpTools(Growable.many(List(tool("open", requiresAuth = false), tool("secure", requiresAuth = true)).map(_.apply(LiveApi)))),
      config = McpServer.Config(
        supportedVersions = List(ProtocolVersion.V2026_07_28),
        capabilities = ServerCapabilities(None, None, None, None, None, Some(ServerCapabilities.Tools(None)), None),
        serverInfo = Some(Implementation("oxygen-mcp-http-test", None, "0.1.0", None, None, None)),
        instructions = None,
        discoverTtlMs = 0L,
        listTtlMs = 0L,
        cacheScope = CacheScope.Private,
      ),
    )

  /** The single hand-built `POST /mcp` oxygen-http endpoint. */
  private val mcpEndpoint: AppliedEndpoint =
    McpHttp.endpoints(server).toArraySeq.find(e => e.method.contains(Method.POST)).get

  /** Drive a request straight through the endpoint's `handle` (as the endpoint tree would). */
  private def post(body: String): UIO[Response] =
    ZIO.scoped {
      for {
        request <- ReceivedRequest.fromRequest(Request(method = Method.POST, url = URL.root / "mcp", body = Body.fromString(body)))
        input = EndpointInput(request, ServerErrorConfig(exposeInternalErrors = false))
        maybe <- mcpEndpoint.handle(input).getOrElse(ZIO.none)
      } yield maybe.get
    }

  private val meta: String = """{ "io.modelcontextprotocol/protocolVersion": "2026-07-28", "io.modelcontextprotocol/clientCapabilities": {} }"""

  override def testSpec: TestSpec =
    suite("McpHttpSpec")(
      test("POST /mcp tools/list returns 200 with the tools") {
        val body = s"""{ "jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": { "_meta": $meta } }"""
        for {
          resp <- post(body)
          str <- resp.body.asString
        } yield assertTrue(resp.status == Status.Ok, str.contains("\"open\""), str.contains("\"secure\""))
      },
      test("POST /mcp tools/call on an auth-required tool with no verifier is a real 401") {
        val body = s"""{ "jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": { "name": "secure", "arguments": {}, "_meta": $meta } }"""
        for {
          resp <- post(body)
        } yield assertTrue(
          resp.status == Status.Unauthorized,
          resp.headers.rawHeader("WWW-Authenticate").exists(_.contains("Bearer")),
        )
      },
      test("POST /mcp tools/call on an open tool returns 200 and the result") {
        val body = s"""{ "jsonrpc": "2.0", "id": 3, "method": "tools/call", "params": { "name": "open", "arguments": {}, "_meta": $meta } }"""
        for {
          resp <- post(body)
          str <- resp.body.asString
        } yield assertTrue(resp.status == Status.Ok, str.contains("open"), str.contains("\"result\""))
      },
    )

}
