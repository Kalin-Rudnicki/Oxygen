package oxygen.crypto.service

import java.math.BigInteger
import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.security.KeyFactory
import java.security.spec.RSAPublicKeySpec
import java.time.{Duration as JDuration, Instant}
import java.util.Base64
import oxygen.crypto.model.*
import oxygen.json.JsonCodec
import zio.*

/**
  * A [[BearerTokenService.Validator]] that verifies RS256 (RSA) and ES256/ES384/ES512 (EC) JWTs
  * against an OpenID Connect provider's JWKS (Keycloak, Okta, Auth0, Entra, ...) rather than a single
  * statically-configured key. It OIDC-discovers the `jwks_uri` from the issuer's metadata, caches keys
  * by `kid`, and refetches (throttled) when a token presents an unknown `kid` — covering key rotation.
  *
  * This is the stateless resource-server validation half of OIDC: it checks the token's signature
  * against the issuer's published keys and never calls the IdP per request beyond the throttled,
  * cached JWKS fetch. Expiry/audience/issuer claim checks layer on top of this (the signature gate).
  */
final case class JwksValidator(
    issuer: String,
    minRefreshInterval: Duration,
    keysByKid: Ref[Map[String, HashKey.CanValidate]],
    lastRefresh: Ref[Option[Instant]],
) extends BearerTokenService.Validator {

  override def validateToken(token: BearerToken): IO[SignatureError, Unit] =
    for {
      kid <- ZIO.fromOption(token.header.kid).orElseFail(SignatureError.GenericValidationFailure(new RuntimeException("token has no `kid` header")))
      key <- keyForKid(kid)
      // The JWK's key type fixes the algorithm (RSA -> RS256, EC curve -> ES256/384/512); reject a token
      // whose header advertises a different one rather than silently verifying under the wrong scheme.
      _ <- ZIO.fail(SignatureError.InvalidAlgorithm(key.alg, token.header.alg)).unlessDiscard(token.header.alg == key.alg)
      _ <- SignatureService.Validator
        .Live(key)
        .validateBase64(token.`headerBase64.payloadBase64`, Signature.Base64(token.signatureBase64))
    } yield ()

  /** Cached lookup; on an unknown `kid`, refetch the JWKS once (throttled) and look again. */
  private def keyForKid(kid: String): IO[SignatureError, HashKey.CanValidate] =
    keysByKid.get.map(_.get(kid)).flatMap {
      case Some(key) => ZIO.succeed(key)
      case None      =>
        refreshThrottled *>
          keysByKid.get.map(_.get(kid)).someOrFail(SignatureError.GenericValidationFailure(new RuntimeException(s"no JWKS key for kid=$kid")))
    }

  private def refreshThrottled: UIO[Unit] =
    for {
      now <- Clock.instant
      last <- lastRefresh.get
      stale = last.forall(t => JDuration.between(t, now).getSeconds >= minRefreshInterval.toSeconds)
      _ <- ZIO.when(stale) {
        fetchKeys.foldZIO(
          e => ZIO.logWarning(s"JWKS refresh failed: ${e.getMessage}"),
          keys => keysByKid.set(keys) *> lastRefresh.set(Some(now)),
        )
      }
    } yield ()

  /** OIDC-discover the jwks_uri from the issuer, fetch it, and build a kid -> validator-key map. */
  private def fetchKeys: Task[Map[String, HashKey.CanValidate]] =
    for {
      discoveryBody <- httpGet(s"${issuer.stripSuffix("/")}/.well-known/openid-configuration")
      discovery <- fromJson[JwksValidator.Discovery](discoveryBody)
      jwksBody <- httpGet(discovery.jwks_uri)
      jwks <- fromJson[JwksValidator.Jwks](jwksBody)
    } yield jwks.keys.iterator.flatMap(JwksValidator.hashKeyForJwk).toMap

  private def httpGet(url: String): Task[String] =
    ZIO.attemptBlocking {
      val req = HttpRequest.newBuilder(URI.create(url)).timeout(JDuration.ofSeconds(10)).GET().build()
      val resp = JwksValidator.client.send(req, HttpResponse.BodyHandlers.ofString())
      if resp.statusCode() != 200 then throw new RuntimeException(s"GET $url -> ${resp.statusCode()}")
      resp.body()
    }

  private def fromJson[A: JsonCodec](body: String): Task[A] =
    ZIO.fromEither(JsonCodec[A].decoder.decodeJsonString(body)).mapError(e => new RuntimeException(s"bad JSON: ${e.getMessage}"))

}
object JwksValidator {

  private val client: HttpClient =
    HttpClient.newBuilder().connectTimeout(JDuration.ofSeconds(5)).build()

  private final case class Discovery(jwks_uri: String) derives JsonCodec
  // A JWK is `kty`-tagged: RSA carries `n`/`e`, EC carries `crv`/`x`/`y`. All optional so a mixed set decodes.
  private final case class Jwk(
      kid: String,
      kty: String,
      n: Option[String] = None,
      e: Option[String] = None,
      crv: Option[String] = None,
      x: Option[String] = None,
      y: Option[String] = None,
  ) derives JsonCodec
  private final case class Jwks(keys: List[Jwk]) derives JsonCodec

  /** Turn a single JWK into a `kid -> validator-key`, skipping unsupported/malformed entries. */
  private def hashKeyForJwk(k: Jwk): Option[(String, HashKey.CanValidate)] =
    k.kty match
      case "RSA" => for { n <- k.n; e <- k.e } yield k.kid -> HashKey.RsaPublic(rsaPublicFromJwk(n, e))
      case "EC"  => for { crv <- k.crv; x <- k.x; y <- k.y } yield k.kid -> HashKey.EcPublic(CryptoKey.EC.Public.fromJWK(crv, x, y))
      case _     => None

  /** JWK RSA params (base64url unsigned big-endian `n`/`e`) -> oxygen public key, via X.509 SPKI DER. */
  private def rsaPublicFromJwk(n: String, e: String): CryptoKey.RSA.Public = {
    val modulus = new BigInteger(1, Base64.getUrlDecoder.decode(n))
    val exponent = new BigInteger(1, Base64.getUrlDecoder.decode(e))
    val pub = KeyFactory.getInstance("RSA").generatePublic(new RSAPublicKeySpec(modulus, exponent))
    CryptoKey.RSA.Public.fromDER(KeyFormat.DER(pub.getEncoded))
  }

  /** @param minRefreshInterval throttle: don't refetch the JWKS more than once per this interval. */
  final case class Config(issuer: String, minRefreshInterval: Duration = 10.seconds)

  val layer: URLayer[JwksValidator.Config, BearerTokenService.Validator] =
    ZLayer.fromZIO {
      for {
        cfg <- ZIO.service[JwksValidator.Config]
        keys <- Ref.make(Map.empty[String, HashKey.CanValidate])
        last <- Ref.make(Option.empty[Instant])
      } yield JwksValidator(cfg.issuer, cfg.minRefreshInterval, keys, last)
    }

}
