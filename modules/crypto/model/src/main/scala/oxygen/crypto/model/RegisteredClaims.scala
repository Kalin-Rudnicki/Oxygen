package oxygen.crypto.model

import java.time.Instant
import oxygen.core.model.Email
import oxygen.json.*
import oxygen.predef.core.*

/**
  * The standard RFC 7519 registered claims (plus the OAuth `scope` claim and the widely-emitted OIDC
  * claims), modeled as third parties actually emit them — the loose form, deliberately distinct from
  * [[JWT.StandardPayload]] (which is oxygen's opinionated *issuance* shape: `jti` a UUID, no `iss`/`aud`).
  *
  * This is the *consumption / validation* shape used to validate third-party (e.g. OAuth) access
  * tokens — every claim optional, `jti` a plain `String` (not a UUID), and `aud` decoding from either
  * a single string or an array of strings (see [[Audience]]).
  *
  * Typed fields cover three tiers, all optional:
  *   - RFC 7519 registered claims: `iss` `sub` `aud` `exp` `nbf` `iat` `jti`.
  *   - The OAuth `scope` claim.
  *   - The OIDC claims that are standardized and emitted broadly across IdPs (Keycloak, Okta, Auth0,
  *     Entra, ...): the OIDC Core §5.1 profile claims `email` `email_verified` `name` `given_name`
  *     `family_name` `preferred_username`, and the §2 authentication claims `acr` `auth_time` `azp`
  *     `sid`. `email` uses [[Email]] (so a malformed address fails the decode).
  *
  * `exp`/`nbf`/`iat`/`auth_time` decode as RFC 7519 §2 NumericDate (a JSON number of seconds since the
  * epoch), which is how real issuers emit them — see [[numericDateDecoder]].
  *
  * Any other claim on the wire (e.g. Keycloak's `typ`/`allowed-origins`, or custom claims) is preserved
  * verbatim in [[otherClaims]] rather than dropped — so consumers can reach issuer- or deployment-
  * specific claims without a new payload type per issuer.
  */
final case class RegisteredClaims(
    // RFC 7519 registered claims
    issuer: Option[String] = None,
    subject: Option[String] = None,
    audience: Audience = Audience.empty,
    expiresAt: Option[Instant] = None,
    validAfter: Option[Instant] = None,
    issuedAt: Option[Instant] = None,
    tokenId: Option[String] = None,
    // OAuth
    scope: Option[Scopes] = None,
    // OIDC Core §5.1 profile claims
    email: Option[Email] = None,
    emailVerified: Option[Boolean] = None,
    name: Option[String] = None,
    givenName: Option[String] = None,
    familyName: Option[String] = None,
    preferredUsername: Option[String] = None,
    // OIDC Core §2 authentication claims
    acr: Option[String] = None,
    authTime: Option[Instant] = None,
    authorizedParty: Option[String] = None,
    sessionId: Option[String] = None,
    // everything else (Keycloak `typ`/`allowed-origins`, custom claims, ...)
    otherClaims: Map[String, Json] = Map.empty,
)
object RegisteredClaims {

  // File-scoped NumericDate instances. `derives JsonCodec` summons JsonDecoder/JsonEncoder per field
  // (not JsonCodec), so both must be provided directly; these lexical givens outrank oxygen's default
  // (ISO-string) Instant instances for the `Registered` mirror's derivation only.
  private given numericDateDecoder: JsonDecoder[Instant] =
    JsonCodec.json.decoder.mapOrFail {
      case Json.Number(n) => Right(Instant.ofEpochSecond(n.toLong))
      case other          => Left(s"NumericDate (exp/nbf/iat) must be a JSON number of seconds since the epoch: ${other.showCompact}")
    }
  private given numericDateEncoder: JsonEncoder[Instant] =
    JsonCodec.json.encoder.contramap(i => Json.Number(BigDecimal(i.getEpochSecond)))

  /**
    * Mirror of just the typed claims, used so the field-level derivation (Audience / Scopes / Email /
    * NumericDate) is reused rather than reimplemented. Non-strict, so the catch-all keys present on the
    * wire are ignored here and captured separately into [[RegisteredClaims.otherClaims]].
    */
  private final case class Registered(
      @jsonField("iss") issuer: Option[String] = None,
      @jsonField("sub") subject: Option[String] = None,
      @jsonField("aud") audience: Audience = Audience.empty,
      @jsonField("exp") expiresAt: Option[Instant] = None,
      @jsonField("nbf") validAfter: Option[Instant] = None,
      @jsonField("iat") issuedAt: Option[Instant] = None,
      @jsonField("jti") tokenId: Option[String] = None,
      @jsonField("scope") scope: Option[Scopes] = None,
      @jsonField("email") email: Option[Email] = None,
      @jsonField("email_verified") emailVerified: Option[Boolean] = None,
      @jsonField("name") name: Option[String] = None,
      @jsonField("given_name") givenName: Option[String] = None,
      @jsonField("family_name") familyName: Option[String] = None,
      @jsonField("preferred_username") preferredUsername: Option[String] = None,
      @jsonField("acr") acr: Option[String] = None,
      @jsonField("auth_time") authTime: Option[Instant] = None,
      @jsonField("azp") authorizedParty: Option[String] = None,
      @jsonField("sid") sessionId: Option[String] = None,
  ) derives JsonCodec

  /** The on-the-wire keys owned by a typed field; every other key lands in `otherClaims`. */
  private val registeredKeys: Set[String] =
    Set(
      "iss",
      "sub",
      "aud",
      "exp",
      "nbf",
      "iat",
      "jti",
      "scope",
      "email",
      "email_verified",
      "name",
      "given_name",
      "family_name",
      "preferred_username",
      "acr",
      "auth_time",
      "azp",
      "sid",
    )

  /**
    * Map a raw JSON object into the typed claims plus an `otherClaims` catch-all of the rest. Shared by
    * both the [[JsonCodec]] (resource-server validation) and the `JsonSchema` defined in `oxygen-schema`
    * (the `@mcp.auth` param decode), so the two decode paths can never drift.
    */
  def fromJsonAST(json: Json): Either[String, RegisteredClaims] =
    json match {
      case obj: Json.Obj =>
        summon[JsonCodec[Registered]].decoder.decodeJsonAST(obj).leftMap(_.getMessage).map { r =>
          val other = obj.value.iterator.collect { case (k, v) if !registeredKeys(k) => k -> v }.toMap
          RegisteredClaims(
            r.issuer,
            r.subject,
            r.audience,
            r.expiresAt,
            r.validAfter,
            r.issuedAt,
            r.tokenId,
            r.scope,
            r.email,
            r.emailVerified,
            r.name,
            r.givenName,
            r.familyName,
            r.preferredUsername,
            r.acr,
            r.authTime,
            r.authorizedParty,
            r.sessionId,
            other,
          )
        }
      case other =>
        s"JWT claims must be a JSON object, got ${other.tpe}".asLeft
    }

  /** Inverse of [[fromJsonAST]]: typed fields (None omitted) merged with the `otherClaims`. */
  def toJsonAST(claims: RegisteredClaims): Json = {
    val registered =
      summon[JsonCodec[Registered]].encoder.encodeJsonAST(
        Registered(
          claims.issuer,
          claims.subject,
          claims.audience,
          claims.expiresAt,
          claims.validAfter,
          claims.issuedAt,
          claims.tokenId,
          claims.scope,
          claims.email,
          claims.emailVerified,
          claims.name,
          claims.givenName,
          claims.familyName,
          claims.preferredUsername,
          claims.acr,
          claims.authTime,
          claims.authorizedParty,
          claims.sessionId,
        ),
      )
    val registeredFields = registered match {
      case obj: Json.Obj => obj.value
      case _             => ArraySeq.empty[(String, Json)]
    }
    Json.Obj(registeredFields ++ claims.otherClaims.iterator.map { case (k, v) => k -> v })
  }

  given JsonCodec[RegisteredClaims] = JsonCodec.json.transformOrFail(fromJsonAST, toJsonAST)

}

/** The JWT `aud` claim: either a single string or an array of strings (RFC 7519 §4.1.3). */
final case class Audience(values: List[String]) {
  def contains(audience: String): Boolean = values.contains(audience)
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty
}
object Audience {

  val empty: Audience = Audience(Nil)
  def one(audience: String): Audience = Audience(audience :: Nil)

  given JsonCodec[Audience] =
    JsonCodec.json.transformOrFail(
      {
        case Json.Str(s)     => Audience(s :: Nil).asRight
        case Json.Arr(elems) =>
          elems.toList.foldRight[Either[String, List[String]]](Nil.asRight) {
            case (Json.Str(s), Right(acc)) => (s :: acc).asRight
            case (other, Right(_))         => s"`aud` array element is not a string: ${other.showCompact}".asLeft
            case (_, left)                 => left
          }.map(Audience(_))
        case Json.Null => Audience.empty.asRight
        case other     => s"`aud` is not a string or array of strings: ${other.showCompact}".asLeft
      },
      {
        case Audience(s :: Nil) => Json.Str(s)
        case Audience(many)     => Json.Arr(many.map[Json](Json.Str(_)).toArraySeq)
      },
    )

}
