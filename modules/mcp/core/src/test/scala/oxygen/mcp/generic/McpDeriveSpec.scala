package oxygen.mcp.generic

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.*
import oxygen.mcp.domain.model.*
import oxygen.predef.test.*
import oxygen.schema.{JsonSchema, PlainTextSchema}
import zio.*

object McpDeriveSpec extends OxygenSpecDefault {

  // A trait whose abstract methods become MCP tools by derivation.
  private trait Calculator derives DeriveMcp {
    @mcpDoc("adds two integers") def add(a: Int, b: Int): URIO[Scope, Int]
    def greet(@mcpDoc("who to greet") name: String): URIO[Scope, String]
  }
  private object LiveCalc extends Calculator {
    override def add(a: Int, b: Int): URIO[Scope, Int] = ZIO.succeed(a + b)
    override def greet(name: String): URIO[Scope, String] = ZIO.succeed(s"hi $name")
  }
  private val calcLayer: ULayer[Calculator] = ZLayer.succeed[Calculator](LiveCalc)

  // A trait whose tool has a typed error `E` — with the default `Failure`, `E` is forced into a generic
  // `McpError.InternalError` (which surfaces at the transport as a protocol error).
  private trait Faily derives DeriveMcp {
    def risky(n: Int): ZIO[Scope, String, Int]
  }
  private object LiveFaily extends Faily {
    override def risky(n: Int): ZIO[Scope, String, Int] = if n < 0 then ZIO.fail("negative!") else ZIO.succeed(n * 2)
  }
  private val failyLayer: ULayer[Faily] = ZLayer.succeed[Faily](LiveFaily)

  // A trait whose typed error has a *custom* `Failure` mapping it to a specific `McpError` (here an auth
  // error -> `McpError.Unauthorized`), demonstrating a consumer overriding the generic default.
  private final case class Denied(reason: String)
  private object Denied {
    given McpResponseSchema.Failure[Denied] = d => McpError.Unauthorized(d.reason)
  }
  private trait Guarded derives DeriveMcp {
    def secret(n: Int): ZIO[Scope, Denied, Int]
  }
  private object LiveGuarded extends Guarded {
    override def secret(n: Int): ZIO[Scope, Denied, Int] = if n < 0 then ZIO.fail(Denied("no negatives")) else ZIO.succeed(n)
  }
  private val guardedLayer: ULayer[Guarded] = ZLayer.succeed[Guarded](LiveGuarded)

  // A trait whose tools take the authenticated caller (injected, not decoded from args).
  private trait Secured derives DeriveMcp {
    def whoami(principal: McpPrincipal): URIO[Scope, String]
    def maybeWho(who: Option[McpPrincipal]): URIO[Scope, String]
  }
  private object LiveSecured extends Secured {
    override def whoami(principal: McpPrincipal): URIO[Scope, String] = ZIO.succeed(principal.subject)
    override def maybeWho(who: Option[McpPrincipal]): URIO[Scope, String] = ZIO.succeed(who.fold("anon")(_.subject))
  }
  private val securedLayer: ULayer[Secured] = ZLayer.succeed[Secured](LiveSecured)

  // A consumer's OWN typed caller: any type with a `McpPrincipalDecoder` is injected + auth-requiring,
  // exactly like `McpPrincipal` — tools take the domain type directly instead of the generic principal.
  private final case class Caller(name: String, canRead: Boolean)
  private object Caller {
    given McpPrincipalDecoder[Caller] =
      McpPrincipalDecoder.fromReason(p => if p.subject.isEmpty then Left("empty subject") else Right(Caller(p.subject, p.scopes.contains("read"))))
  }
  private trait Typed derives DeriveMcp {
    def hello(caller: Caller): URIO[Scope, String]
    def maybeHello(caller: Option[Caller], punct: String): URIO[Scope, String]
  }
  private object LiveTyped extends Typed {
    override def hello(caller: Caller): URIO[Scope, String] = ZIO.succeed(s"hello ${caller.name} read=${caller.canRead}")
    override def maybeHello(caller: Option[Caller], punct: String): URIO[Scope, String] = ZIO.succeed(caller.fold("hello anon")(c => s"hello ${c.name}") + punct)
  }
  private val typedLayer: ULayer[Typed] = ZLayer.succeed[Typed](LiveTyped)

  // A caller type decoded straight from the raw bearer via its PlainTextSchema (`fromPlainText`).
  private final case class RawTok(value: String)
  private object RawTok {
    given PlainTextSchema[RawTok] =
      PlainTextSchema.string.transformOrFail[RawTok](s => if s.startsWith("tok-") then Right(RawTok(s)) else Left(s"not a tok: $s"), _.value)
    given McpPrincipalDecoder[RawTok] = McpPrincipalDecoder.fromPlainText[RawTok]
  }
  private trait Plain derives DeriveMcp {
    def raw(t: RawTok): URIO[Scope, String]
  }
  private object LivePlain extends Plain {
    override def raw(t: RawTok): URIO[Scope, String] = ZIO.succeed(t.value)
  }
  private val plainServer: UIO[McpServer] = serverFor(McpTools.empty.add[Plain], ZLayer.succeed[Plain](LivePlain))

  private val cfg: McpServer.Config =
    McpServer.Config(
      supportedVersions = List(API.ProtocolVersion.V2026_07_28),
      capabilities = API.response.ServerCapabilities(None, None, None, None, None, Some(API.response.ServerCapabilities.Tools(None)), None),
      serverInfo = Some(API.Implementation("oxygen-mcp-derive-test", None, "0.1.0", None, None, None)),
      instructions = None,
      discoverTtlMs = 0L,
      listTtlMs = 0L,
      cacheScope = API.response.CacheScope.Private,
    )

  // Provide the API impl to the derived tools' layer, yielding the api-erased applied tools / a server.
  private def appliedTools[Api](tools: McpTools[Api], impl: ULayer[Api]): UIO[AppliedMcpTools] =
    ZIO.scoped(tools.toLayer.build.map(_.get[AppliedMcpTools])).provideLayer(impl)

  private def serverFor[Api](tools: McpTools[Api], impl: ULayer[Api]): UIO[McpServer] =
    appliedTools(tools, impl).map(McpServer(_, cfg))

  private val calcAppliedTools: UIO[AppliedMcpTools] = appliedTools(McpTools.empty.add[Calculator], calcLayer)
  private val securedAppliedTools: UIO[AppliedMcpTools] = appliedTools(McpTools.empty.add[Secured], securedLayer)
  private val calcServer: UIO[McpServer] = serverFor(McpTools.empty.add[Calculator], calcLayer)
  private val securedServer: UIO[McpServer] = serverFor(McpTools.empty.add[Secured], securedLayer)
  private val typedAppliedTools: UIO[AppliedMcpTools] = appliedTools(McpTools.empty.add[Typed], typedLayer)
  private val typedServer: UIO[McpServer] = serverFor(McpTools.empty.add[Typed], typedLayer)
  private val failyServer: UIO[McpServer] = serverFor(McpTools.empty.add[Faily], failyLayer)
  private val guardedServer: UIO[McpServer] = serverFor(McpTools.empty.add[Guarded], guardedLayer)

  private val meta: String = """{ "io.modelcontextprotocol/protocolVersion": "2026-07-28", "io.modelcontextprotocol/clientCapabilities": {} }"""

  private def callAs(srv: McpServer, name: String, args: String, principal: Option[McpPrincipal]): ZIO[Any, Nothing, Either[McpError, Json]] = {
    val body = s"""{ "jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": { "name": "$name", "arguments": $args, "_meta": $meta } }"""
    ZIO.scoped(ZIO.fromEither(McpCodec.decodeRequest(body)).orDieWith(e => new RuntimeException(e.toString)).flatMap(srv.dispatch(_, principal)))
  }

  private val alice: McpPrincipal = McpPrincipal("alice", Set("read"), "tok", Json.obj())

  private def decodeCall(payload: Json): Either[String, API.response.CallToolResponse] =
    JsonSchema[API.response.CallToolResponse].jsonCodec.decoder.decodeJsonString(payload.showCompact).leftMap(_.toString)

  // The text content of a (successful-dispatch) tool result, or throws on a protocol error.
  private def callText(srv: McpServer, name: String, args: String, principal: Option[McpPrincipal]): UIO[List[String]] =
    callAs(srv, name, args, principal).map {
      case Right(payload) => decodeCall(payload).map(_.content.collect { case API.response.ContentBlock.Text(t, _) => t }).getOrElse(Nil)
      case Left(err)      => throw new AssertionError(s"protocol error: $err")
    }

  override def testSpec: TestSpec =
    suite("McpDeriveSpec")(
      test("derives one tool per abstract method, with input schemas") {
        for {
          tools <- calcAppliedTools
        } yield {
          val byName = tools.arraySeq.map(t => t.tool.name -> t.tool).toMap
          assertTrue(
            tools.arraySeq.map(_.tool.name).toSet == Set("add", "greet"),
            // add's input schema declares a + b as required integers
            byName("add").inputSchema.toString.contains("\"a\""),
            byName("add").inputSchema.toString.contains("\"b\""),
            byName("add").inputSchema.toString.contains("integer"),
            byName("greet").inputSchema.toString.contains("\"name\""),
          )
        }
      },
      test("@mcpDoc flows into the tool description and a param's description") {
        for {
          tools <- calcAppliedTools
        } yield {
          val byName = tools.arraySeq.map(t => t.tool.name -> t.tool).toMap
          assertTrue(
            // @mcpDoc on the method -> the tool description
            byName("add").description.contains("adds two integers"),
            byName("greet").description.isEmpty,
            // @mcpDoc on a param -> that property's description in the input schema
            byName("greet").inputSchema.toString.contains("who to greet"),
          )
        }
      },
      test("a derived tool decodes args, invokes the method, and encodes the result") {
        for {
          srv <- calcServer
          add <- callAs(srv, "add", """{ "a": 2, "b": 3 }""", None)
          greet <- callAs(srv, "greet", """{ "name": "bob" }""", None)
        } yield assertTrue(
          add.map(decodeCall) == Right(Right(API.response.CallToolResponse(
            resultType = API.response.ResultType.Complete,
            content = List(API.response.ContentBlock.Text("5", None)),
            structuredContent = None,
            isError = Some(false),
          ))),
          greet.map(decodeCall) == Right(Right(API.response.CallToolResponse(
            resultType = API.response.ResultType.Complete,
            content = List(API.response.ContentBlock.Text("hi bob", None)),
            structuredContent = None,
            isError = Some(false),
          ))),
        )
      },
      test("a bad argument comes back as an InvalidParams protocol error") {
        for {
          srv <- calcServer
          bad <- callAs(srv, "add", """{ "a": "not a number", "b": 3 }""", None)
        } yield assertTrue(
          bad match {
            case Left(McpError.InvalidParams("a", _)) => true
            case _                                    => false
          },
        )
      },
      test("a McpPrincipal param makes the tool require auth and is injected from the caller") {
        for {
          tools <- securedAppliedTools
          srv <- securedServer
          who <- callText(srv, "whoami", "{}", Some(alice))
        } yield {
          val byName = tools.arraySeq.map(t => t.tool.name -> t).toMap
          assertTrue(
            byName("whoami").requiresAuth,
            !byName("maybeWho").requiresAuth,
            // the principal is injected, not an input-schema property
            !byName("whoami").tool.inputSchema.toString.contains("principal"),
            who == List("alice"),
          )
        }
      },
      test("an auth-required derived tool called with no principal is an Unauthorized protocol error") {
        for {
          srv <- securedServer
          out <- callAs(srv, "whoami", "{}", None)
        } yield assertTrue(
          out match {
            case Left(_: McpError.Unauthorized) => true
            case _                              => false
          },
        )
      },
      test("a param of any type with a McpPrincipalDecoder is an injected, auth-requiring caller") {
        for {
          tools <- typedAppliedTools
          srv <- typedServer
          hi <- callText(srv, "hello", "{}", Some(alice))
          anon <- callAs(srv, "hello", "{}", None)
          bad <- callAs(srv, "hello", "{}", Some(alice.copy(subject = "")))
          maybeAnon <- callText(srv, "maybeHello", "{ \"punct\": \"!\" }", None)
          maybeAlice <- callText(srv, "maybeHello", "{ \"punct\": \"?\" }", Some(alice))
        } yield {
          val byName = tools.arraySeq.map(t => t.tool.name -> t).toMap
          assertTrue(
            byName("hello").requiresAuth,
            !byName("maybeHello").requiresAuth,
            // the typed caller is injected, not an input-schema property; the real arg still is
            !byName("hello").tool.inputSchema.toString.contains("caller"),
            !byName("maybeHello").tool.inputSchema.toString.contains("caller"),
            byName("maybeHello").tool.inputSchema.toString.contains("punct"),
            hi == List("hello alice read=true"),
            anon == Left(McpError.Unauthorized("authentication required")),
            bad == Left(McpError.Unauthorized("empty subject")),
            maybeAnon == List("hello anon!"),
            maybeAlice == List("hello alice?"),
          )
        }
      },
      test("fromPlainText decodes the raw bearer through the type's PlainTextSchema") {
        for {
          srv <- plainServer
          ok <- callText(srv, "raw", "{}", Some(alice.copy(token = "tok-123")))
          bad <- callAs(srv, "raw", "{}", Some(alice.copy(token = "nope")))
        } yield assertTrue(ok == List("tok-123"), bad == Left(McpError.Unauthorized("not a tok: nope")))
      },
      test("an Option[McpPrincipal] param is injected and does not require auth") {
        for {
          srv <- securedServer
          anon <- callText(srv, "maybeWho", "{}", None)
          named <- callText(srv, "maybeWho", "{}", Some(alice))
        } yield assertTrue(anon == List("anon"), named == List("alice"))
      },
      test("a ZIO[Scope, E, A] tool's default Failure forces E into an InternalError protocol error") {
        for {
          srv <- failyServer
          ok <- callAs(srv, "risky", """{ "n": 3 }""", None)
          bad <- callAs(srv, "risky", """{ "n": -1 }""", None)
        } yield assertTrue(
          // success: E is not raised, A is encoded normally
          ok.map(decodeCall).map(_.map(r => (r.isError, r.content))) ==
            Right(Right((Some(false), List(API.response.ContentBlock.Text("6", None))))),
          // typed failure: the default Failure maps E -> McpError.InternalError carrying its compact json
          bad == Left(McpError.InternalError(Some("\"negative!\""))),
        )
      },
      test("a custom Failure forces the tool's E into a specific McpError (surfaces as a protocol error)") {
        for {
          srv <- guardedServer
          ok <- callAs(srv, "secret", """{ "n": 3 }""", None)
          bad <- callAs(srv, "secret", """{ "n": -1 }""", None)
        } yield assertTrue(
          // success: A is encoded normally
          ok.map(decodeCall).map(_.map(r => (r.isError, r.content))) ==
            Right(Right((Some(false), List(API.response.ContentBlock.Text("3", None))))),
          // typed failure: the custom Failure[Denied] -> McpError.Unauthorized, out `Left` from dispatch
          bad == Left(McpError.Unauthorized("no negatives")),
        )
      },
    )

}
