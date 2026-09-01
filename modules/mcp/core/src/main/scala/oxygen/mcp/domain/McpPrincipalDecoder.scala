package oxygen.mcp.domain

import oxygen.mcp.domain.model.*
import oxygen.schema.PlainTextSchema

/**
  * How a derived tool obtains its injected "who is calling" parameter of type `A` from the transport-level
  * [[McpPrincipal]] the HTTP layer resolved.
  *
  * [[oxygen.mcp.generic.McpDerive]] treats any tool parameter whose type has a given `McpPrincipalDecoder`
  * as an *injected, authentication-requiring* parameter: it is supplied from [[McpToolInput.principal]]
  * rather than decoded from the `tools/call` arguments, is excluded from the tool's `inputSchema`, and marks
  * the tool `requiresAuth` (a missing principal is a `401`). An `Option[A]` parameter is injected too but
  * does not require auth (`None` when the call is unauthenticated).
  *
  * `mcp-core` ships only the identity instance for [[McpPrincipal]] itself. A consumer with its own typed
  * token gives an instance for that type — typically [[McpPrincipalDecoder.fromPlainText]], re-decoding the
  * raw [[McpPrincipal.token]] through the type's `PlainTextSchema` — so its tools can take the domain type
  * directly instead of the generic principal.
  *
  * Covariant: a decoder for a subtype serves a param of the supertype (e.g. a `TypedJWT`'s bare-`Token`
  * variant decoder satisfies a param typed as the JWT itself).
  */
trait McpPrincipalDecoder[+A] {

  /** Turn the resolved principal into `A`; a `Left` is the protocol error the call fails with. */
  def decode(principal: McpPrincipal): Either[McpError, A]

}
object McpPrincipalDecoder {

  def apply[A](using d: McpPrincipalDecoder[A]): McpPrincipalDecoder[A] = d

  /** Build an instance from a plain function. */
  def make[A](f: McpPrincipal => Either[McpError, A]): McpPrincipalDecoder[A] =
    p => f(p)

  /** Build an instance whose failure reason is a plain message, surfaced as an [[McpError.Unauthorized]]. */
  def fromReason[A](f: McpPrincipal => Either[String, A]): McpPrincipalDecoder[A] =
    p => f(p).left.map(McpError.Unauthorized(_))

  /**
    * Decode the raw [[McpPrincipal.token]] with the given `PlainTextSchema[A]`. The token is the BARE bearer
    * (the `Bearer ` scheme is stripped by the HTTP layer), so for a `TypedJWT` pass its bare-token variant's
    * schema (`X.Token.schema` / `X.TokenOrBearer.schema`), not the default `Authorization`-header one, which
    * only accepts the `Bearer …` form. A decode failure is an [[McpError.Unauthorized]].
    */
  def fromPlainText[A](using schema: PlainTextSchema[A]): McpPrincipalDecoder[A] =
    fromReason(p => schema.decode(p.token))

  /** The generic principal itself. */
  given principal: McpPrincipalDecoder[McpPrincipal] = Right(_)

}
