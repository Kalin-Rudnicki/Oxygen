package oxygen.http

import java.util.UUID
import oxygen.http.core.*
import oxygen.http.server.{DeriveEndpoints, ServerErrorConfig}
import oxygen.http.server.mcp.*
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.predef.test.*
import oxygen.schema.JsonSchema
import scala.annotation.experimental
import zio.*

@experimental
object McpServerSpec extends OxygenSpecDefault {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Test API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class EchoResult(value: String) derives JsonSchema

  @experimental
  trait McpTestApi {

    @mcp.tool
    @route.get("/echo/%")
    def echo(
        @httpDoc("the word to echo back") @param.path word: String,
        @param.query times: Option[Int],
    ): IO[ApiError, EchoResult]

    @mcp.tool("createUser")
    @route.post("/users")
    def create(
        @param.body body: CreateUser,
    ): IO[ApiError, User]

    // Returns a top-level JSON array — structuredContent must still be an object (wrapped).
    @mcp.tool
    @route.get("/echoes")
    def listEchoes(): IO[ApiError, List[EchoResult]]

    // NOT @mcp.tool — must not appear as a tool.
    @route.get("/internal")
    def internal(): IO[ApiError, User]

  }

  private val fixedId: UUID = UUID.fromString("00000000-0000-0000-0000-000000000001")

  private val impl: McpTestApi =
    new McpTestApi {
      override def echo(word: String, times: Option[Int]): IO[ApiError, EchoResult] =
        ZIO.succeed(EchoResult(word * times.getOrElse(1)))
      override def create(body: CreateUser): IO[ApiError, User] =
        ZIO.succeed(User(fixedId, body.first, body.last, body.age))
      override def listEchoes(): IO[ApiError, List[EchoResult]] =
        ZIO.succeed(List(EchoResult("a"), EchoResult("b")))
      override def internal(): IO[ApiError, User] =
        ZIO.succeed(User(fixedId, "in", "ternal", 0))
    }

  private val tools: ArraySeq[AppliedMcpEndpoint] =
    DeriveEndpoints.derived[McpTestApi].endpoints.map(_(impl)).toArraySeq.flatMap(_.mcp)

  private val server: McpServer =
    new McpServer(tools, McpAuthService.NoAuth, McpServer.ServerInfo("test-mcp", "0.1.0"))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def request(method: String, params: Json): Json =
    Json.obj("jsonrpc" -> Json.string("2.0"), "id" -> Json.number(1), "method" -> Json.string(method), "params" -> params)

  private def respondJson(result: McpEndpointResult): Json =
    result match
      case result: McpEndpointResult.Basic => result.json
      case other                           => throw new AssertionError(s"expected Respond, got $other")

  extension (j: Json) {
    private def fld(name: String): Json =
      j match
        case Json.Obj(fs) => fs.collectFirst { case (k, v) if k == name => v }.getOrElse(Json.Null)
        case _            => Json.Null
    private def asString: Option[String] = j match { case Json.Str(s) => Some(s); case _ => None }
    private def asBool: Option[Boolean] = j match { case Json.Bool(b) => Some(b); case _ => None }
    private def arrElems: ArraySeq[Json] = j match { case Json.Arr(a) => a; case _ => ArraySeq.empty }
    private def objKeys: Set[String] = j match { case Json.Obj(fs) => fs.iterator.map(_._1).toSet; case _ => Set.empty }
  }

  private def result(method: String, params: Json, bearer: Option[String] = None): ZIO[Any, Nothing, Json] =
    ZIO.scoped(server.mcpExecute(Json.number(1), request(method, params), bearer, ServerErrorConfig(true))).merge.map(respondJson(_).fld("result"))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Spec
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override def testSpec: TestSpec =
    suite("McpServerSpec")(
      test("initialize negotiates protocol + advertises tools capability") {
        result("initialize", Json.obj()).map { res =>
          assertTrue(
            res.fld("protocolVersion").asString.contains(McpServer.protocolVersion),
            res.fld("capabilities").objKeys.contains("tools"),
            res.fld("serverInfo").fld("name").asString.contains("test-mcp"),
          )
        }
      },
      test("tools/list lists only @mcp.tool endpoints, with input schemas") {
        result("tools/list", Json.obj()).map { res =>
          val toolList = res.fld("tools").arrElems
          val names = toolList.flatMap(_.fld("name").asString).toSet
          val echoTool = toolList.find(_.fld("name").asString.contains("McpTestApi_echo")).get
          val echoProps = echoTool.fld("inputSchema").fld("properties").objKeys
          val echoRequired = echoTool.fld("inputSchema").fld("required").arrElems.flatMap(_.asString).toSet
          assertTrue(
            // `echo` + `createUser` + `listEchoes` exposed; `internal` (no @mcp.tool) is not
            names == Set("McpTestApi_echo", "createUser", "McpTestApi_listEchoes"),
            echoProps == Set("word", "times"),
            echoRequired.contains("word"), // String path param is required
            !echoRequired.contains("times"), // Option query param is not
            echoTool.fld("inputSchema").fld("properties").fld("word").fld("type").asString.contains("string"),
            // @httpDoc on a flat param surfaces as that property's description
            echoTool.fld("inputSchema").fld("properties").fld("word").fld("description").asString.contains("the word to echo back"),
          )
        }
      },
      test("tools/call decodes JSON args, runs the handler, encodes the result") {
        result("tools/call", Json.obj("name" -> Json.string("McpTestApi_echo"), "arguments" -> Json.obj("word" -> Json.string("hi"), "times" -> Json.number(3)))).map { res =>
          assertTrue(
            res.fld("isError").asBool.contains(false),
            res.fld("structuredContent").fld("value").asString.contains("hihihi"),
          )
        }
      },
      test("tools/call decodes a JSON body param into a product type") {
        val args = Json.obj("body" -> Json.obj("first" -> Json.string("Ada"), "last" -> Json.string("Lovelace"), "age" -> Json.number(36)))
        result("tools/call", Json.obj("name" -> Json.string("createUser"), "arguments" -> args)).map { res =>
          assertTrue(
            res.fld("isError").asBool.contains(false),
            res.fld("structuredContent").fld("first").asString.contains("Ada"),
            res.fld("structuredContent").fld("age") == Json.number(36),
          )
        }
      },
      test("tools/call with a missing required arg returns a tool execution error (not a protocol error)") {
        result("tools/call", Json.obj("name" -> Json.string("McpTestApi_echo"), "arguments" -> Json.obj())).map { res =>
          assertTrue(
            res.fld("isError").asBool.contains(true),
            // structuredContent mirrors success output; it is omitted on error
            res.fld("structuredContent") == Json.Null,
          )
        }
      },
      test("tools/call on an unknown tool is a JSON-RPC error") {
        ZIO.scoped(server.mcpExecute(Json.number(1), request("tools/call", Json.obj("name" -> Json.string("nope"))), None, ServerErrorConfig(true)).merge).map { res =>
          val json = respondJson(res)
          assertTrue(
            json.fld("error").fld("code") == Json.number(RpcErrorCode.InvalidParams.code),
            json.fld("result") == Json.Null,
          )
        }
      },
    )

}
