package oxygen.http.server.mcp

import java.time.Instant
import oxygen.crypto.model.*
import oxygen.crypto.service.{BearerTokenService, HashKey}
import oxygen.json.*
import oxygen.predef.core.*
import zio.*

/**
  * The MCP server's OAuth 2.1 *Resource Server* gate. It does not issue tokens, run any OAuth flow,
  * or handle refresh — its entire job is to validate the audience-bound JWT bearer presented on a
  * `tools/call` and to advertise where to get one (Protected Resource Metadata, RFC 9728).
  *
  *   - [[Live]] validates the JWT signature (via [[BearerTokenService.Validator]] — HS256 or RS256,
  *     from oxygen-crypto, no external deps) plus the registered claims that constitute the gate:
  *     `exp`/`nbf`, `aud` = this resource, `iss` = the configured authorization server. These are the
  *     401-class failures. Scope authorization (the 403 path) is resource-server-wide — the dispatcher
  *     checks the token holds [[requiredScopes]] for every authed tool, not per-tool.
  *   - [[NoAuth]] is for servers with no authenticated tools; if an authed tool is exposed while
  *     `NoAuth` is wired, the middleware fails to start.
  */
trait McpAuthService {

  /** `true` for a real (`Live`) gate; `false` for `NoAuth`. */
  def isConfigured: Boolean

  /** Validate the presented bearer (signature + audience-bound registered claims). */
  def authenticate(bearer: Option[String]): IO[McpAuthError, RegisteredClaims]

  /** Scopes every authed tool's token must hold (empty = any valid token). Enforced by the dispatcher. */
  def requiredScopes: Set[String]

  /** The RFC 9728 Protected Resource Metadata document, or `None` when auth is not configured. */
  def protectedResourceMetadata: Option[Json]

}
object McpAuthService {

  final case class Live(
      config: McpAuthConfig,
      bearerTokenValidator: BearerTokenService.Validator,
  ) extends McpAuthService {

    override def isConfigured: Boolean = true

    override def requiredScopes: Set[String] = config.requiredScopes

    override def authenticate(bearer: Option[String]): IO[McpAuthError, RegisteredClaims] =
      for {
        raw <- ZIO.fromOption(bearer).orElseFail(McpAuthError.MissingToken)
        jwt <- ZIO
          .fromEither(JWT.decodeBearerOrToken[RegisteredClaims](raw)(using summon[JsonCodec[RegisteredClaims]].decoder))
          .mapError(McpAuthError.Malformed(_))
        _ <- bearerTokenValidator.validateToken(jwt.token).mapError(e => McpAuthError.InvalidSignature(e.toString))
        now <- Clock.instant
        claims = jwt.payload
        _ <- validateTiming(claims, now)
        _ <- validateAudience(claims)
        _ <- validateIssuer(claims)
      } yield claims

    private def validateTiming(claims: RegisteredClaims, now: Instant): IO[McpAuthError, Unit] =
      claims.validAfter.filter(now.isBefore) match
        case Some(nbf) => ZIO.fail(McpAuthError.NotYetValid(now, nbf))
        case None      =>
          claims.expiresAt.filterNot(now.isBefore) match
            case Some(exp) => ZIO.fail(McpAuthError.Expired(now, exp))
            case None      => ZIO.unit

    private def validateAudience(claims: RegisteredClaims): IO[McpAuthError, Unit] =
      ZIO.fail(McpAuthError.WrongAudience(config.audience, claims.audience)).unless(claims.audience.contains(config.audience)).unit

    private def validateIssuer(claims: RegisteredClaims): IO[McpAuthError, Unit] =
      config.expectedIssuer match
        case None      => ZIO.unit
        case Some(iss) => ZIO.fail(McpAuthError.WrongIssuer(iss, claims.issuer)).unless(claims.issuer.contains(iss)).unit

    override def protectedResourceMetadata: Option[Json] = {
      val fields: List[(String, Json)] =
        List(
          "resource" -> Json.string(config.audience),
          "authorization_servers" -> Json.Arr(config.authorizationServers.map[Json](Json.string).toArraySeq),
        ) :::
          // advertise required scopes too, so a client always knows what to request
          ({
            val advertised = config.scopesSupported ++ config.requiredScopes;
            if advertised.nonEmpty then List("scopes_supported" -> Json.Arr(advertised.toList.sorted.map[Json](Json.string).toArraySeq)) else Nil
          }) :::
          List("bearer_methods_supported" -> Json.arr(Json.string("header")))
      Json.Obj(fields.toArraySeq).some
    }

  }

  case object NoAuth extends McpAuthService {
    override def isConfigured: Boolean = false
    override def requiredScopes: Set[String] = Set.empty
    override def authenticate(bearer: Option[String]): IO[McpAuthError, RegisteredClaims] =
      ZIO.die(new IllegalStateException("McpAuthService.NoAuth.authenticate was called — an authed MCP tool was exposed without auth configured"))
    override def protectedResourceMetadata: Option[Json] = None
  }

  def live: URLayer[McpAuthConfig & BearerTokenService.Validator, McpAuthService] =
    ZLayer.fromFunction(Live.apply)

  /** Convenience layer: supply this server's validation key + config directly. */
  def keyLive: URLayer[McpAuthConfig & HashKey.CanValidate, McpAuthService] =
    BearerTokenService.Validator.keyLayer >>> live

  val noAuth: ULayer[McpAuthService] = ZLayer.succeed(NoAuth)

}

/**
  * Resource-server config. `audience` is this server's canonical resource URI (the RFC 8707 `aud`
  * tokens must carry); `expectedIssuer` is the configured authorization server (`None` skips the `iss`
  * check); `authorizationServers` + `scopesSupported` populate the Protected Resource Metadata.
  *
  * `requiredScopes` are enforced server-wide: every authed tool's token must hold them (empty = any
  * valid token suffices). They are also advertised in the Protected Resource Metadata.
  */
final case class McpAuthConfig(
    audience: String,
    expectedIssuer: Option[String],
    authorizationServers: List[String],
    scopesSupported: Set[String] = Set.empty,
    requiredScopes: Set[String] = Set.empty,
)

sealed trait McpAuthError {
  def message: String
}
object McpAuthError {
  case object MissingToken extends McpAuthError { override def message: String = "missing bearer token" }
  final case class Malformed(detail: String) extends McpAuthError { override def message: String = s"malformed bearer token: $detail" }
  final case class InvalidSignature(detail: String) extends McpAuthError { override def message: String = s"invalid token signature: $detail" }
  final case class Expired(now: Instant, expiresAt: Instant) extends McpAuthError { override def message: String = s"token expired at $expiresAt (now: $now)" }
  final case class NotYetValid(now: Instant, validAfter: Instant) extends McpAuthError { override def message: String = s"token not valid until $validAfter (now: $now)" }
  final case class WrongAudience(expected: String, got: Audience) extends McpAuthError {
    override def message: String = s"token audience [${got.values.mkString(", ")}] does not include this resource server ($expected)"
  }
  final case class WrongIssuer(expected: String, got: Option[String]) extends McpAuthError {
    override def message: String = s"token issuer (${got.getOrElse("<none>")}) does not match the expected authorization server ($expected)"
  }
}
