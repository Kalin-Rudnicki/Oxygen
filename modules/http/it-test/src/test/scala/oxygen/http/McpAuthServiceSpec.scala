package oxygen.http

import java.time.Instant
import oxygen.crypto.model.*
import oxygen.crypto.service.*
import oxygen.http.schema.McpEndpointSchema
import oxygen.http.server.ServerErrorConfig
import oxygen.http.server.mcp.*
import oxygen.json.*
import oxygen.predef.test.*
import zio.*

/**
  * End-to-end test of the MCP OAuth resource-server gate: issue a real JWT carrying [[RegisteredClaims]]
  * (signed with oxygen-crypto's HS256), then validate it through [[McpAuthService.Live]] — signature +
  * audience/issuer/expiry. No external dependencies.
  */
object McpAuthServiceSpec extends OxygenSpecDefault {

  private val audience: String = "https://mcp.example.com/mcp"
  private val issuer: String = "https://auth.example.com"

  private val key1: HashKey.Hmac = HashKey.Hmac(CryptoKey.HS256.fromPlain("oxygen-mcp-test-secret-1"))
  private val key2: HashKey.Hmac = HashKey.Hmac(CryptoKey.HS256.fromPlain("oxygen-mcp-test-secret-2"))

  private val config: McpAuthConfig = McpAuthConfig(audience, issuer.some, List(issuer), Set("read"))

  private val authService: McpAuthService =
    McpAuthService.Live(config, BearerTokenService.Validator.Live(SignatureService.Validator.Live(key1)))

  private def issuerWith(key: HashKey.Hmac): BearerTokenService.Issuer =
    BearerTokenService.Issuer.Live(SignatureService.Signer.Live(key), SignatureService.Validator.Live(key))

  private def mint(key: HashKey.Hmac, claims: RegisteredClaims): UIO[String] =
    issuerWith(key).issueToken(summon[JsonCodec[RegisteredClaims]].encoder.encodeJsonStringCompact(claims)).map(_.bearer)

  private val validClaims: RegisteredClaims =
    RegisteredClaims(
      issuer = issuer.some,
      subject = "user-123".some,
      audience = Audience.one(audience),
      expiresAt = Instant.parse("2999-01-01T00:00:00Z").some,
      scope = Scopes.of("read", "write").some,
    )

  private def isErr(r: Either[McpAuthError, RegisteredClaims])(pf: PartialFunction[McpAuthError, Boolean]): Boolean =
    r.swap.toOption.exists(e => pf.applyOrElse(e, _ => false))

  // ===== Server-wide scope gate (the 403 path) — auth requires "admin" =====

  private val scopedAuth: McpAuthService =
    McpAuthService.Live(config.copy(requiredScopes = Set("admin")), BearerTokenService.Validator.Live(SignatureService.Validator.Live(key1)))

  private val secureTool: AppliedMcpEndpoint =
    AppliedMcpEndpoint(
      schema = McpEndpointSchema(
        toolName = "secure",
        description = None,
        inputSchema = Json.obj("type" -> Json.string("object")),
        authParamName = "auth".some,
      ),
      handle = _ => ZIO.succeed(McpToolResult(McpResponseContent.JsonText(Json.obj("ok" -> Json.Bool(true))) :: Nil, false)),
    )

  private val scopedServer: McpServer =
    new McpServer(ArraySeq(secureTool), scopedAuth, McpServer.ServerInfo("test-mcp", "0.1.0"))

  private def callSecure(token: String): UIO[McpEndpointResult] =
    ZIO.scoped {
      scopedServer.mcpExecute(
        Json.number(1),
        Json.obj(
          "jsonrpc" -> Json.string("2.0"),
          "id" -> Json.number(1),
          "method" -> Json.string("tools/call"),
          "params" -> Json.obj("name" -> Json.string("secure"), "arguments" -> Json.obj()),
        ),
        token.some,
        ServerErrorConfig(true),
      ).merge
    }

  override def testSpec: TestSpec =
    suite("McpAuthServiceSpec")(
      test("a valid, audience-bound token authenticates and yields its claims") {
        for {
          token <- mint(key1, validClaims)
          r <- authService.authenticate(token.some).either
        } yield assertTrue(
          r.toOption.flatMap(_.subject).contains("user-123"),
          r.toOption.flatMap(_.scope).exists(_.values == Set("read", "write")),
        )
      },
      test("a missing token is rejected") {
        authService.authenticate(None).either.map(r => assertTrue(r == Left(McpAuthError.MissingToken)))
      },
      test("an expired token is rejected") {
        for {
          token <- mint(key1, validClaims.copy(expiresAt = Instant.EPOCH.some))
          r <- authService.authenticate(token.some).either
        } yield assertTrue(isErr(r) { case _: McpAuthError.Expired => true })
      },
      test("a token for a different audience is rejected") {
        for {
          token <- mint(key1, validClaims.copy(audience = Audience.one("https://other.example.com")))
          r <- authService.authenticate(token.some).either
        } yield assertTrue(isErr(r) { case _: McpAuthError.WrongAudience => true })
      },
      test("a token from a different issuer is rejected") {
        for {
          token <- mint(key1, validClaims.copy(issuer = "https://evil.example.com".some))
          r <- authService.authenticate(token.some).either
        } yield assertTrue(isErr(r) { case _: McpAuthError.WrongIssuer => true })
      },
      test("a token signed with the wrong key is rejected") {
        for {
          token <- mint(key2, validClaims)
          r <- authService.authenticate(token.some).either
        } yield assertTrue(isErr(r) { case _: McpAuthError.InvalidSignature => true })
      },
      test("an authed tool is forbidden when the (valid) token lacks the server's required scope") {
        for {
          token <- mint(key1, validClaims) // scopes: read, write — not admin
          r <- callSecure(token)
        } yield assertTrue(r match { case _: McpEndpointResult.MissingScopes => true; case _ => false })
      },
      test("an authed tool proceeds when the token holds the required scope") {
        for {
          token <- mint(key1, validClaims.copy(scope = Scopes.of("admin").some))
          r <- callSecure(token)
        } yield assertTrue(r match { case _: McpEndpointResult.RespondSuccess => true; case _ => false })
      },
    )

}
