package oxygen.example.webServer.mcp

import oxygen.json.*
import oxygen.mcp.api.model.*
import oxygen.mcp.api.model.response.*
import oxygen.mcp.domain.*
import oxygen.mcp.domain.model.*
import oxygen.predef.test.*
import oxygen.schema.JsonSchema
import zio.*

/**
  * End-to-end check of the example `NoteApi` as MCP tools, using the new decoupled stack: the same
  * trait's tools are derived via `DeriveMcp` (`McpTools.empty.add[NoteApi]`) and materialized by
  * providing the `Ref`-backed in-memory impl to `.toLayer`; a `create` -> `get` round-trip goes
  * through that impl (no DB), dispatched by `McpServer`.
  *
  * `oxygen-mcp-core` is a test-only dependency of the example, so rather than `NoteApi derives DeriveMcp`
  * on the (main-source) trait, the `DeriveMcp[NoteApi]` instance is summoned here in test scope.
  */
object NoteMcpSpec extends OxygenSpecDefault {

  // A derived tool's typed error `E` is forced into a protocol `McpError` (see `McpResponseSchema.Failure`).
  // A consumer overrides the generic default to map its domain error onto the right protocol error — here a
  // `NoteError.NotFound` -> `McpError.InvalidParams` naming the offending `id`.
  private given McpResponseSchema.Failure[NoteError] = { case NoteError.NotFound(id) =>
    McpError.InvalidParams("id", s"note not found: $id")
  }

  private given DeriveMcp[NoteApi] = DeriveMcp.derived[NoteApi]

  private val meta: String = """{ "io.modelcontextprotocol/protocolVersion": "2026-07-28", "io.modelcontextprotocol/clientCapabilities": {} }"""

  private val config: McpServer.Config =
    McpServer.Config(
      supportedVersions = List(ProtocolVersion.V2026_07_28),
      capabilities = ServerCapabilities(None, None, None, None, None, Some(ServerCapabilities.Tools(None)), None),
      serverInfo = Some(Implementation("oxygen-example", None, "test", None, None, None)),
      instructions = None,
      discoverTtlMs = 0L,
      listTtlMs = 0L,
      cacheScope = CacheScope.Private,
    )

  private def newServer: ZIO[Any, Nothing, McpServer] =
    ZIO
      .scoped {
        McpTools.empty.add[NoteApi].toLayer.build.map(env => McpServer(env.get[AppliedMcpTools], config))
      }
      .provideLayer(NoteApiImpl.layer)

  private def dispatch(server: McpServer, method: String, params: String): UIO[Either[McpError, Json]] = {
    val body = s"""{ "jsonrpc": "2.0", "id": 1, "method": "$method", "params": $params }"""
    ZIO.scoped(ZIO.fromEither(McpCodec.decodeRequest(body)).orDieWith(e => new RuntimeException(e.toString)).flatMap(server.dispatch(_, None)))
  }

  private def listTools(server: McpServer): UIO[List[Tool]] =
    dispatch(server, "tools/list", s"""{ "_meta": $meta }""").map {
      case Right(payload) => JsonSchema[ListToolsResponse].jsonCodec.decoder.decodeJsonString(payload.showCompact).map(_.tools).getOrElse(Nil)
      case Left(err)      => throw new AssertionError(s"tools/list protocol error: $err")
    }

  private def callResult(server: McpServer, name: String, args: String): UIO[CallToolResponse] =
    dispatch(server, "tools/call", s"""{ "name": "$name", "arguments": $args, "_meta": $meta }""").map {
      case Right(payload) => JsonSchema[CallToolResponse].jsonCodec.decoder.decodeJsonString(payload.showCompact).getOrElse(throw new AssertionError("could not decode CallToolResponse"))
      case Left(err)      => throw new AssertionError(s"tools/call protocol error: $err")
    }

  private def strField(json: Json, name: String): Option[String] =
    json match {
      case Json.Obj(fields) => fields.collectFirst { case (`name`, Json.Str(s)) => s }
      case _                => None
    }

  override def testSpec: TestSpec =
    suite("NoteMcpSpec")(
      test("NoteApi derives its three abstract methods as MCP tools") {
        for {
          srv <- newServer
          tools <- listTools(srv)
        } yield assertTrue(tools.map(_.name).toSet == Set("create", "get", "list"))
      },
      test("create then get round-trips through the Ref-backed in-memory store") {
        for {
          srv <- newServer
          created <- callResult(srv, "create", """{ "req": { "title": "Hello", "body": "World" } }""")
          note = created.structuredContent.getOrElse(Json.Null)
          id = strField(note, "id").getOrElse("")
          got <- callResult(srv, "get", s"""{ "id": ${Json.string(id).showCompact} }""")
          gotNote = got.structuredContent.getOrElse(Json.Null)
        } yield assertTrue(
          created.isError == Some(false),
          strField(note, "title").contains("Hello"),
          id.startsWith("note-"),
          strField(gotNote, "id").contains(id),
          strField(gotNote, "body").contains("World"),
        )
      },
      test("get on a missing id is forced into a protocol McpError via the custom Failure") {
        for {
          srv <- newServer
          got <- dispatch(srv, "tools/call", s"""{ "name": "get", "arguments": { "id": "nope" }, "_meta": $meta }""")
        } yield assertTrue(
          got match {
            case Left(McpError.InvalidParams("id", detail)) => detail.contains("nope")
            case _                                          => false
          },
        )
      },
      test("a model field's @doc flows into the derived tool's input schema") {
        for {
          srv <- newServer
          tools <- listTools(srv)
          create = tools.find(_.name == "create").getOrElse(throw new AssertionError("no create tool"))
        } yield assertTrue(create.inputSchema.toString.contains("Short, human-readable title."))
      },
    )

}
