package oxygen.crypto.model

import oxygen.json.*
import oxygen.predef.core.*

/**
  * The profile claims returned by an OpenID Connect **UserInfo endpoint** (OIDC Core §5.3) — or, when
  * the access token is itself a rich JWT, the equivalent claims read directly off the token.
  *
  * A resource server doing **JIT (just-in-time) user auto-provisioning** uses this to find-or-create a
  * local user the first time a `sub` is seen: `sub` is the stable key, the rest is profile data. The
  * OIDC *standard* claims are modeled as typed fields; non-standard provider claims (notably `groups`
  * / `roles`, which are NOT part of core OIDC) stay reachable through [[raw]] / [[stringArrayClaim]].
  *
  * Security: the `sub` here MUST equal the `sub` of the validated access token before provisioning.
  */
final case class OidcUserInfo(
    sub: String,
    name: Option[String],
    givenName: Option[String],
    familyName: Option[String],
    preferredUsername: Option[String],
    email: Option[String],
    emailVerified: Option[Boolean],
    picture: Option[String],
    raw: Json,
) {

  /** Any claim by name — including non-standard ones (e.g. `groups`, `roles`, `org_id`). */
  def claim(name: String): Option[Json] =
    raw match
      case Json.Obj(fields) => fields.collectFirst { case (k, v) if k == name => v }
      case _                => None

  /** A non-standard string-or-array claim flattened to a list (the usual shape for `groups`/`roles`). */
  def stringArrayClaim(name: String): List[String] =
    claim(name) match
      case Some(Json.Arr(elems)) => elems.toList.collect { case Json.Str(s) => s }
      case Some(Json.Str(s))     => List(s)
      case _                     => Nil

}
object OidcUserInfo {

  def fromJson(json: Json): Either[String, OidcUserInfo] = {
    def field(name: String): Option[Json] =
      json match
        case Json.Obj(fields) => fields.collectFirst { case (k, v) if k == name => v }
        case _                => None
    def str(name: String): Option[String] = field(name).collect { case Json.Str(s) => s }
    def bool(name: String): Option[Boolean] = field(name).collect { case Json.Bool(b) => b }

    str("sub") match
      case None      => "userinfo response is missing the required `sub` claim".asLeft
      case Some(sub) =>
        OidcUserInfo(
          sub = sub,
          name = str("name"),
          givenName = str("given_name"),
          familyName = str("family_name"),
          preferredUsername = str("preferred_username"),
          email = str("email"),
          emailVerified = bool("email_verified"),
          picture = str("picture"),
          raw = json,
        ).asRight
  }

  given JsonCodec[OidcUserInfo] = JsonCodec.json.transformOrFail(fromJson, _.raw)

}
