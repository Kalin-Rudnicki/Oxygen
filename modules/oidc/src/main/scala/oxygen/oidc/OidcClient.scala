package oxygen.oidc

import java.time.Instant
import org.scalajs.dom
import oxygen.core.PlatformCompat
import oxygen.crypto.model.{Base64, OidcDiscovery, OidcTokenResponse, Pkce}
import oxygen.http.client.RequestMiddleware
import oxygen.json.JsonCodec
import scala.concurrent.Future
import scala.scalajs.js
import zio.*

/**
  * A browser (public, PKCE) OpenID Connect relying party for the Authorization Code flow.
  *
  *   - `login` redirects to the IdP (PKCE/state/nonce + the pre-login location persisted in
  *     sessionStorage across the redirect).
  *   - `handleRedirectCallback` verifies `state`, exchanges the `code`, and stores tokens.
  *   - `consumeReturnTo` reads (once) where the user was before `login`, so the app can send them back.
  *   - `accessToken` returns a valid token, refreshing first if the current one is near expiry.
  *   - `bearerMiddleware` attaches `Authorization: Bearer` to outgoing oxygen-http requests.
  *
  * Tokens are held in a `Ref` and mirrored into `sessionStorage`, so a full-page refresh restores the
  * session without a redirect round-trip (the token is per-tab, cleared when the tab closes, and still
  * short-lived — revocation stays IdP-owned via TTL, not app state). This is a deliberate XSS-window
  * tradeoff for usable refresh; it is not a cookie. IdP-facing calls (discovery, token endpoint) use
  * the browser `fetch` API; the IdP must allow this origin via CORS.
  */
final class OidcClient(
    config: OidcClientConfig,
    discoveryRef: Ref[Option[OidcDiscovery]],
    tokensRef: Ref[Option[OidcTokens]],
    refreshLock: Semaphore,
) {

  /** Redirect the browser to the IdP `/authorize`. `prompt = Some("none")` attempts a silent re-auth. */
  def login(prompt: Option[String] = None): Task[Unit] =
    for {
      disc <- discovery
      pkce = Pkce.generate
      state = OidcClient.randomToken()
      nonce = OidcClient.randomToken()
      _ <- ZIO.attempt {
        val ss = dom.window.sessionStorage
        ss.setItem(OidcClient.stateKey, state)
        ss.setItem(OidcClient.nonceKey, nonce)
        ss.setItem(OidcClient.verifierKey, pkce.codeVerifier)
        // Remember where the user was so `handleRedirectCallback` can send them back (not to some
        // fixed landing page). Skip the callback path itself to avoid a return-to-callback loop.
        val here = dom.window.location.pathname + dom.window.location.search
        if !here.contains("/oidc/callback") then ss.setItem(OidcClient.returnToKey, here)
      }
      url = AuthorizeUrl.build(disc, config, pkce, state, nonce, prompt)
      _ <- ZIO.attempt(dom.window.location.href = url)
    } yield ()

  /** Handle the `?code&state` callback: verify `state`, exchange the `code`, store the tokens. */
  def handleRedirectCallback: Task[OidcTokens] =
    for {
      params <- ZIO.attempt(new dom.URLSearchParams(dom.window.location.search))
      errorParam = Option(params.get("error"))
      _ <- ZIO.fail(OidcError(s"IdP returned error: ${errorParam.getOrElse("")}")).whenDiscard(errorParam.isDefined)
      code <- ZIO.fromOption(Option(params.get("code"))).orElseFail(OidcError("callback missing `code`"))
      returnedState <- ZIO.fromOption(Option(params.get("state"))).orElseFail(OidcError("callback missing `state`"))
      ss <- ZIO.attempt(dom.window.sessionStorage)
      savedState <- ZIO.fromOption(Option(ss.getItem(OidcClient.stateKey))).orElseFail(OidcError("no saved state (login not initiated here?)"))
      _ <- ZIO.fail(OidcError("state mismatch (possible CSRF)")).unlessDiscard(returnedState == savedState)
      verifier <- ZIO.fromOption(Option(ss.getItem(OidcClient.verifierKey))).orElseFail(OidcError("no saved PKCE verifier"))
      disc <- discovery
      now <- Clock.instant
      tokens <- exchange(disc, OidcClient.codeBody(config, code, verifier)).map(OidcClient.toTokens(_, now))
      _ <- setTokens(Some(tokens))
      _ <- ZIO.attempt {
        ss.removeItem(OidcClient.stateKey)
        ss.removeItem(OidcClient.nonceKey)
        ss.removeItem(OidcClient.verifierKey)
      }
    } yield tokens

  /** A valid access token, refreshing first if the current one is missing or near expiry. */
  def accessToken: Task[String] =
    for {
      now <- Clock.instant
      cur <- tokensRef.get
      tok <- cur match
        case Some(t) if !OidcClient.isExpired(t, now) => ZIO.succeed(t)
        case _                                        => refresh
    } yield tok.accessToken

  /** Refresh only if the current token is missing/near-expiry; single-flight across concurrent callers in this tab. */
  def refresh: Task[OidcTokens] =
    refreshLock.withPermit {
      for {
        now <- Clock.instant
        cur <- tokensRef.get
        tok <- cur match
          case Some(t) if !OidcClient.isExpired(t, now) => ZIO.succeed(t) // someone refreshed while we waited
          case _                                        => exchangeRefresh(now)
      } yield tok
    }

  /**
    * Force a refresh-token exchange even if the access token is not yet locally expired. This is a
    * liveness probe against the IdP: it succeeds (and rotates the token) only if the session/refresh
    * token is still valid, and fails if the IdP was reset, the session revoked, or the refresh token
    * expired. Callers use the failure to detect a restored-but-dead token and drop it.
    */
  def forceRefresh: Task[OidcTokens] =
    refreshLock.withPermit(Clock.instant.flatMap(exchangeRefresh))

  /** The refresh-token exchange itself (assumes the lock is held); rotates the stored tokens on success. */
  private def exchangeRefresh(now: Instant): Task[OidcTokens] =
    tokensRef.get.flatMap {
      case Some(t) =>
        t.refreshToken match
          case Some(rt) =>
            for {
              disc <- discovery
              fresh <- exchange(disc, OidcClient.refreshBody(config, rt)).map(OidcClient.toTokens(_, now))
              _ <- setTokens(Some(fresh))
            } yield fresh
          case None => ZIO.fail(OidcError("no refresh token; re-login required"))
      case None => ZIO.fail(OidcError("not authenticated; call login()"))
    }

  /** Whatever access token is currently held, without triggering a refresh (for the middleware). */
  def currentAccessToken: UIO[Option[String]] = tokensRef.get.map(_.map(_.accessToken))

  /** The id_token's raw JWS, if present (decode with oxygen-crypto's JWT to read claims). */
  def idToken: UIO[Option[String]] = tokensRef.get.map(_.flatMap(_.idToken))

  /** Clear the locally-held tokens ONLY. The IdP session (SSO cookie) is untouched — see `logoutRedirect`. */
  def logout: UIO[Unit] = setTokens(None)

  /**
    * RP-initiated logout (OIDC): clear the local tokens AND end the session at the IdP, so the user is
    * genuinely signed out. Local-only `logout` is not enough — the IdP's SSO session survives, so the
    * next `login()` is silently re-authenticated (looks like "sign out immediately signs back in").
    *
    * Redirects the browser to the IdP `end_session_endpoint` with `id_token_hint` (skips the IdP's
    * logout-confirm prompt) and, if configured, `post_logout_redirect_uri` (must be allow-listed on the
    * client). If the IdP advertises no end-session endpoint, this degrades to a local-only logout.
    */
  def logoutRedirect: Task[Unit] =
    for {
      disc <- discovery
      idt <- idToken
      _ <- logout // clear local tokens+storage before leaving the page
      _ <- disc.endSessionEndpoint match
        case None           => ZIO.unit
        case Some(endpoint) =>
          val params: List[(String, String)] =
            ("client_id" -> config.clientId) ::
              idt.map("id_token_hint" -> _).toList :::
              config.postLogoutRedirectUri.map("post_logout_redirect_uri" -> _).toList
          val query = params.map((k, v) => s"${OidcClient.enc(k)}=${OidcClient.enc(v)}").mkString("&")
          ZIO.attempt(dom.window.location.href = s"$endpoint?$query")
    } yield ()

  /** Where the user was before `login()` redirected (read once, then cleared). `None` if unknown. */
  def consumeReturnTo: UIO[Option[String]] =
    ZIO.attempt {
      val ss = dom.window.sessionStorage
      val v = Option(ss.getItem(OidcClient.returnToKey))
      ss.removeItem(OidcClient.returnToKey)
      v
    }.orElseSucceed(None)

  /** Update the in-memory tokens and mirror them into sessionStorage (best-effort) so a refresh restores them. */
  private def setTokens(tokens: Option[OidcTokens]): UIO[Unit] =
    tokensRef.set(tokens) *> OidcClient.persist(tokens)

  /** Attaches `Authorization: Bearer <token>` to outgoing oxygen-http requests, if authenticated. */
  def bearerMiddleware: RequestMiddleware =
    new RequestMiddleware {
      override def apply(request: zio.http.Request): URIO[Scope, zio.http.Request] =
        currentAccessToken.map {
          case Some(t) => request.addHeader("Authorization", s"Bearer $t")
          case None    => request
        }
    }

  /** OIDC-discover (and cache) the IdP endpoints. */
  def discovery: Task[OidcDiscovery] =
    discoveryRef.get.flatMap {
      case Some(d) => ZIO.succeed(d)
      case None    => fetchJson[OidcDiscovery](config.discoveryUrl, None).tap(d => discoveryRef.set(Some(d)))
    }

  private def exchange(disc: OidcDiscovery, formBody: String): Task[OidcTokenResponse] =
    fetchJson[OidcTokenResponse](disc.tokenEndpoint, Some(formBody))

  private def fetchJson[A: JsonCodec](url: String, formBody: Option[String]): Task[A] =
    for {
      body <- OidcClient.fetchText(url, formBody)
      a <- ZIO.fromEither(JsonCodec[A].decoder.decodeJsonString(body)).mapError(e => OidcError(s"bad response from $url: ${e.getMessage}"))
    } yield a

}

object OidcClient {

  def make(config: OidcClientConfig): UIO[OidcClient] =
    for {
      d <- Ref.make(Option.empty[OidcDiscovery])
      initial <- loadPersisted // restore a prior session across a full-page refresh (no redirect)
      t <- Ref.make(initial)
      sem <- Semaphore.make(1)
    } yield new OidcClient(config, d, t, sem)

  val layer: URLayer[OidcClientConfig, OidcClient] =
    ZLayer.fromZIO(ZIO.serviceWithZIO[OidcClientConfig](make))

  private val stateKey: String = "oxygen.oidc.state"
  private val nonceKey: String = "oxygen.oidc.nonce"
  private val verifierKey: String = "oxygen.oidc.verifier"
  private val tokensKey: String = "oxygen.oidc.tokens"
  private val returnToKey: String = "oxygen.oidc.returnTo"

  /** sessionStorage-serializable form of `OidcTokens` (`Instant` -> epoch-millis for a portable codec). */
  private final case class Persisted(
      accessToken: String,
      refreshToken: Option[String],
      idToken: Option[String],
      expiresAtEpochMs: Option[Long],
  ) derives JsonCodec {
    def toTokens: OidcTokens = OidcTokens(accessToken, refreshToken, idToken, expiresAtEpochMs.map(Instant.ofEpochMilli))
  }
  private object Persisted {
    def from(t: OidcTokens): Persisted = Persisted(t.accessToken, t.refreshToken, t.idToken, t.expiresAt.map(_.toEpochMilli))
  }

  /** Mirror the tokens into sessionStorage (best-effort; storage failures must not break auth). */
  private def persist(tokens: Option[OidcTokens]): UIO[Unit] =
    ZIO.attempt {
      val ss = dom.window.sessionStorage
      tokens match
        case Some(t) => ss.setItem(tokensKey, JsonCodec[Persisted].encoder.encodeJsonStringCompact(Persisted.from(t)))
        case None    => ss.removeItem(tokensKey)
    }.ignore

  /** Load tokens previously mirrored into sessionStorage; `None` if absent or unparseable. */
  private def loadPersisted: UIO[Option[OidcTokens]] =
    ZIO.attempt {
      Option(dom.window.sessionStorage.getItem(tokensKey))
        .flatMap(s => JsonCodec[Persisted].decoder.decodeJsonString(s).toOption)
        .map(_.toTokens)
    }.orElseSucceed(None)

  private val expirySkewSeconds: Long = 30L

  private def isExpired(t: OidcTokens, now: Instant): Boolean =
    t.expiresAt.exists(exp => !now.isBefore(exp.minusSeconds(expirySkewSeconds)))

  private def toTokens(resp: OidcTokenResponse, now: Instant): OidcTokens =
    OidcTokens(resp.accessToken, resp.refreshToken, resp.idToken, resp.expiresIn.map(s => now.plusSeconds(s)))

  private def randomToken(): String = Base64.urlEncoder.encodeToString(PlatformCompat.secureRandomBytes(16))

  private def enc(s: String): String = js.URIUtils.encodeURIComponent(s)

  private def codeBody(config: OidcClientConfig, code: String, verifier: String): String =
    s"grant_type=authorization_code&code=${enc(code)}&redirect_uri=${enc(config.redirectUri)}&client_id=${enc(config.clientId)}&code_verifier=${enc(verifier)}"

  private def refreshBody(config: OidcClientConfig, refreshToken: String): String =
    s"grant_type=refresh_token&refresh_token=${enc(refreshToken)}&client_id=${enc(config.clientId)}"

  /** GET (`formBody = None`) or form-POST via the browser `fetch` API; fails on a non-2xx status. */
  private def fetchText(url: String, formBody: Option[String]): Task[String] = {
    val init: dom.RequestInit =
      formBody match
        case Some(b) =>
          js.Dynamic
            .literal(
              method = "POST",
              headers = js.Dynamic.literal("Content-Type" -> "application/x-www-form-urlencoded"),
              body = b,
            )
            .asInstanceOf[dom.RequestInit]
        case None =>
          js.Dynamic.literal(method = "GET").asInstanceOf[dom.RequestInit]

    ZIO
      .fromFuture { ec =>
        given scala.concurrent.ExecutionContext = ec
        import scala.scalajs.js.Thenable.Implicits.*
        val respF: Future[dom.Response] = dom.window.fetch(url, init)
        respF.flatMap { resp =>
          val textF: Future[String] = resp.text()
          textF.map(body => (resp.status, body))
        }
      }
      .mapError(e => OidcError(s"network error calling $url: ${e.getMessage}"))
      .flatMap { (status, body) =>
        if status >= 200 && status < 300 then ZIO.succeed(body)
        else ZIO.fail(OidcError(s"$url -> HTTP $status: $body"))
      }
  }

}
