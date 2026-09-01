package oxygen.mcp.domain

import oxygen.core.collection.Growable
import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import oxygen.predef.test.*
import oxygen.schema.JsonSchema
import zio.*

object McpServerSpec extends OxygenSpecDefault {

  // A tiny API + one manually-defined tool, applied to its impl and hosted as `AppliedMcpTools`.
  private trait Weather {
    def report(city: String): String
  }
  private object LiveWeather extends Weather {
    override def report(city: String): String = s"weather: $city"
  }

  private val reportTool: McpTool[Weather] =
    McpTool[Weather](
      tool = API.response.Tool(
        name = "get_weather",
        title = None,
        description = Some("Get the weather for a city."),
        inputSchema = API.response.ToolInputSchema.wrap(Json.obj("type" -> Json.string("object"))),
        outputSchema = None,
        annotations = None,
        icons = None,
      ),
      requiresAuth = false,
      handle = api =>
        input => {
          val city = input.arguments.value.collectFirst { case ("city", Json.Str(s)) => s }.getOrElse("?")
          ZIO.succeed(McpToolResult.text(api.report(city)))
        },
    )

  // A tool whose handler fails its (typed) `McpError` channel — surfaces as a `Left` protocol error.
  private val boomTool: McpTool[Weather] =
    McpTool[Weather](
      tool = API.response.Tool(
        name = "boom",
        title = None,
        description = None,
        inputSchema = API.response.ToolInputSchema.wrap(Json.obj("type" -> Json.string("object"))),
        outputSchema = None,
        annotations = None,
        icons = None,
      ),
      requiresAuth = false,
      handle = _ => _ => ZIO.fail(McpError.Forbidden(List("admin"))),
    )

  private val server: McpServer =
    McpServer(
      tools = AppliedMcpTools(Growable.many(List(reportTool.apply(LiveWeather), boomTool.apply(LiveWeather)))),
      config = McpServer.Config(
        supportedVersions = List(API.ProtocolVersion.V2026_07_28),
        capabilities = API.response.ServerCapabilities(None, None, None, None, None, Some(API.response.ServerCapabilities.Tools(None)), None),
        serverInfo = Some(API.Implementation("oxygen-mcp-test", None, "0.1.0", None, None, None)),
        instructions = None,
        discoverTtlMs = 0L,
        listTtlMs = 0L,
        cacheScope = API.response.CacheScope.Private,
      ),
    )

  private def metaJson(version: String): String =
    s"""{ "io.modelcontextprotocol/protocolVersion": "$version", "io.modelcontextprotocol/clientCapabilities": {} }"""

  private def body(method: String, params: String): String =
    s"""{ "jsonrpc": "2.0", "id": 1, "method": "$method", "params": $params }"""

  private def decodeResult[A: JsonSchema](payload: Json): Either[String, A] =
    JsonSchema[A].jsonCodec.decoder.decodeJsonString(payload.showCompact).leftMap(_.toString)

  override def testSpec: TestSpec =
    suite("McpServerSpec")(
      test("server/discover advertises the supported version + server info") {
        val req = McpCodec.decodeRequest(body("server/discover", s"""{ "_meta": ${metaJson("2026-07-28")} }"""))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(
          outcome.map(decodeResult[API.response.ServerDiscoverResponse]) == Right(
            Right(
              API.response.ServerDiscoverResponse(
                resultType = API.response.ResultType.Complete,
                supportedVersions = List(API.ProtocolVersion.V2026_07_28),
                capabilities = API.response.ServerCapabilities(None, None, None, None, None, Some(API.response.ServerCapabilities.Tools(None)), None),
                instructions = None,
                serverInfo = Some(API.Implementation("oxygen-mcp-test", None, "0.1.0", None, None, None)),
                ttlMs = 0L,
                cacheScope = API.response.CacheScope.Private,
              ),
            ),
          ),
        )
      },
      test("tools/list returns the registered tool") {
        val req = McpCodec.decodeRequest(body("tools/list", s"""{ "_meta": ${metaJson("2026-07-28")} }"""))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(
          outcome.map(p => decodeResult[API.response.ListToolsResponse](p).map(_.tools.map(_.name))) == Right(Right(List("get_weather", "boom"))),
        )
      },
      test("tools/call decodes, dispatches through the handler, and renders the result") {
        val params = s"""{ "name": "get_weather", "arguments": { "city": "Boston" }, "_meta": ${metaJson("2026-07-28")} }"""
        val req = McpCodec.decodeRequest(body("tools/call", params))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(
          outcome.map(decodeResult[API.response.CallToolResponse]) == Right(
            Right(API.response.CallToolResponse(
              resultType = API.response.ResultType.Complete,
              content = List(API.response.ContentBlock.Text("weather: Boston", None)),
              structuredContent = None,
              isError = Some(false),
            )),
          ),
        )
      },
      test("tools/call whose handler fails its McpError channel surfaces as a Left protocol error") {
        val params = s"""{ "name": "boom", "arguments": {}, "_meta": ${metaJson("2026-07-28")} }"""
        val req = McpCodec.decodeRequest(body("tools/call", params))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(outcome == Left(McpError.Forbidden(List("admin"))))
      },
      test("tools/call for an unknown tool is an InvalidTool protocol error") {
        val params = s"""{ "name": "does_not_exist", "arguments": {}, "_meta": ${metaJson("2026-07-28")} }"""
        val req = McpCodec.decodeRequest(body("tools/call", params))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(outcome == Left(McpError.InvalidTool("does_not_exist")))
      },
      test("an unsupported protocol version is rejected") {
        val req = McpCodec.decodeRequest(body("tools/list", s"""{ "_meta": ${metaJson("1999-01-01")} }"""))
        for {
          outcome <- ZIO.scoped(ZIO.fromEither(req).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
        } yield assertTrue(
          outcome match {
            case Left(_: McpError.UnsupportedProtocolVersion) => true
            case _                                            => false
          },
        )
      },
      test("a JSON-RPC error renders with the request id and error code") {
        val id = API.RequestId.wrap(Json.number(1))
        val rendered = McpCodec.renderResponse(id, Left(McpError.InvalidTool("x")))
        val expected = JsonSchema[API.response.ErrorResponse].jsonCodec.encoder.encodeJsonStringCompact(
          API.response.ErrorResponse(API.JsonRpcVersion.V2, id, API.response.JsonRpcError(RpcErrorCode.InvalidParams.code, "invalid tool 'x'", None)),
        )
        assertTrue(rendered.showCompact == expected)
      },
    )

}
