package oxygen.mcp.api.model

import oxygen.json.*
import oxygen.mcp.api.model.request.*
import oxygen.mcp.api.model.response.*
import oxygen.predef.test.*
import oxygen.schema.JsonSchema

object ServerRequestSpec extends OxygenSpecDefault {

  // The exact `tools/call` example from the OFF-36 MCP planning doc (2026-07-28, with _meta).
  private val toolsCallJson: String =
    """{
      |  "jsonrpc": "2.0",
      |  "id": 1,
      |  "method": "tools/call",
      |  "params": {
      |    "name": "get_weather",
      |    "arguments": { "city": "Boston" },
      |    "_meta": {
      |      "io.modelcontextprotocol/protocolVersion": "2026-07-28",
      |      "io.modelcontextprotocol/clientCapabilities": {},
      |      "io.modelcontextprotocol/clientInfo": { "name": "claude-code", "version": "2.0.0" }
      |    }
      |  }
      |}""".stripMargin

  override def testSpec: TestSpec =
    suite("ServerRequestSpec")(
      test("decodes the doc's tools/call example verbatim, and round-trips") {
        val decoded: Either[String, ServerRequest] = JsonSchema[ServerRequest].jsonCodec.decoder.decodeJsonString(toolsCallJson).leftMap(_.toString)
        val reEncoded: Either[String, ServerRequest] =
          decoded.flatMap { req =>
            JsonSchema[ServerRequest].jsonCodec.decoder.decodeJsonString(JsonSchema[ServerRequest].jsonCodec.encoder.encodeJsonStringCompact(req)).leftMap(_.toString)
          }
        assertTrue(
          decoded == Right(
            ServerRequest.ToolsCall(
              jsonrpc = JsonRpcVersion.V2,
              id = RequestId.wrap(Json.number(1)),
              params = ServerRequest.ToolsCall.Params(
                name = "get_weather",
                arguments = Some(ToolArguments.wrap(Json.obj("city" -> Json.string("Boston")))),
                inputResponses = None,
                requestState = None,
                meta = RequestMeta(
                  protocolVersion = ProtocolVersion.V2026_07_28,
                  clientCapabilities = ClientCapabilities(None, None, None, None, None),
                  clientInfo = Some(Implementation("claude-code", None, "2.0.0", None, None, None)),
                  logLevel = None,
                  progressToken = None,
                ),
              ),
            ),
          ),
          reEncoded == decoded,
        )
      },
      test("value enums encode as bare strings and round-trip") {
        def enc[A: JsonSchema](a: A): String = JsonSchema[A].jsonCodec.encoder.encodeJsonStringCompact(a)
        def dec[A: JsonSchema](s: String): Either[String, A] = JsonSchema[A].jsonCodec.decoder.decodeJsonString(s).leftMap(_.toString)
        assertTrue(
          enc(CacheScope.Public) == "\"public\"",
          enc(Role.Assistant) == "\"assistant\"",
          enc(InputResponse.Action.Accept) == "\"accept\"",
          enc(LoggingLevel.Warning) == "\"warning\"",
          enc(Icon.Theme.Dark) == "\"dark\"",
          dec[CacheScope]("\"private\"") == Right(CacheScope.Private),
        )
      },
    )

}
