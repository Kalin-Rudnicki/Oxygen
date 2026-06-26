package oxygen.example.webServer.mcp

import oxygen.http.server.{DeriveEndpoints, ServerErrorConfig}
import oxygen.http.server.mcp.*
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.predef.test.*
import zio.*

/**
  * End-to-end check of the example `NoteApi`: it derives as MCP tools and a `create` → `get` round-trip
  * goes through the real `Ref`-backed in-memory impl (no DB). Exercises `DeriveMcpTools` on the example
  * API + `McpServer` dispatch over the `NoAuth` gate.
  */
object NoteMcpSpec extends OxygenSpecDefault {

  private def request(method: String, params: Json): Json =
    Json.obj("jsonrpc" -> Json.string("2.0"), "id" -> Json.number(1), "method" -> Json.string(method), "params" -> params)

  private def call(name: String, args: Json): Json =
    request("tools/call", Json.obj("name" -> Json.string(name), "arguments" -> args))

  extension (self: McpServer)
    private def sendRequest(method: String)(auth: Option[String], params: (String, Json)*): UIO[McpEndpointResult] =
      ZIO.scoped { self.mcpExecute(Json.number(1), request(method, Json.obj(params*)), auth, ServerErrorConfig(true)).merge }
    private def sendCallTool(name: String)(auth: Option[String], args: (String, Json)*): UIO[McpEndpointResult] =
      ZIO.scoped { self.mcpExecute(Json.number(1), call(name, Json.obj(args*)), auth, ServerErrorConfig(true)).merge }

  private def resultOf(r: McpEndpointResult): Json = {
    r match
      case r: McpEndpointResult.Basic => r.json.fld("result")
      case other                      => throw new AssertionError(s"expected Respond, got $other")
  }

  extension (j: Json) {
    private def fld(name: String): Json = j match
      case Json.Obj(fs) => fs.collectFirst { case (`name`, v) => v }.getOrElse(Json.Null)
      case _            => Json.Null
    private def asString: Option[String] = j match { case Json.Str(s) => Some(s); case _ => None }
    private def arrElems: ArraySeq[Json] = j match { case Json.Arr(a) => a; case _ => ArraySeq.empty }
    private def objVals: ArraySeq[Json] = j match { case Json.Obj(fs) => fs.map(_._2); case _ => ArraySeq.empty }
  }

  private def newServer: ZIO[Any, Nothing, McpServer] =
    ZIO.scoped {
      NoteApiImpl.layer.build.map { env =>
        val impl = env.get[NoteApi]
        val tools = summon[DeriveEndpoints[NoteApi]].endpoints.map(_.apply(impl)).toArraySeq.flatMap(_.mcp)
        new McpServer(tools, McpAuthService.NoAuth, McpServer.ServerInfo("oxygen-example", "test"))
      }
    }

  override def testSpec: TestSpec =
    suite("NoteMcpSpec")(
      test("NoteApi exposes its @mcp.tool routes as three tools") {
        for {
          srv <- newServer
          res <- srv.sendRequest("tools/list")(None).map(resultOf)
          names = res.fld("tools").arrElems.flatMap(_.fld("name").asString).toSet
        } yield assertTrue(names == Set("NoteApi_create", "NoteApi_get", "NoteApi_list"))
      },
      test("create then get round-trips through the Ref-backed in-memory store") {
        for {
          srv <- newServer
          created <- srv.sendCallTool("NoteApi_create")(None, "req" -> Json.obj("title" -> Json.string("Hello"), "body" -> Json.string("World"))).map(resultOf)
          createdNote = created.fld("structuredContent")
          id = createdNote.fld("id").asString.getOrElse("")
          got <- srv.sendCallTool("NoteApi_get")(None, "id" -> Json.string(id)).map(resultOf)
          gotNote = got.fld("structuredContent")
        } yield assertTrue(
          created.fld("isError") == Json.Bool(false),
          createdNote.fld("title").asString.contains("Hello"),
          id.startsWith("note-"),
          gotNote.fld("id").asString.contains(id),
          gotNote.fld("body").asString.contains("World"),
        )
      },
      test("get on a missing id is a tool execution error") {
        for {
          srv <- newServer
          res <- srv.sendCallTool("NoteApi_get")(None, "id" -> Json.string("nope")).map(resultOf)
        } yield assertTrue(res.fld("isError") == Json.Bool(true))
      },
      test("docs flow into the tool at all three levels (route / flat param / nested field)") {
        for {
          srv <- newServer
          res <- srv.sendRequest("tools/list")(None).map(resultOf)
          create = res.fld("tools").arrElems.find(_.fld("name").asString.contains("NoteApi_create")).getOrElse(Json.Null)
          inputSchema = create.fld("inputSchema")
          createNoteDef = inputSchema.fld("$defs").objVals.headOption.getOrElse(Json.Null)
        } yield assertTrue(
          // route -> tool description
          create.fld("description").asString.contains("Create a note and return it (with its generated id)."),
          // flat param -> the input property's description
          inputSchema.fld("properties").fld("req").fld("description").asString.contains("The note to create."),
          // nested field -> the $defs property's description
          createNoteDef.fld("properties").fld("title").fld("description").asString.contains("Short, human-readable title."),
        )
      },
    )

}
