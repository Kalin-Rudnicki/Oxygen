package oxygen.http.server.mcp

import oxygen.crypto.model.OidcUserInfo
import oxygen.json.Json
import zio.*
import zio.http.{Client, Header, Request, URL}

/**
  * A small ZIO service that fetches OpenID Connect **UserInfo** for a validated bearer token — the
  * profile claims (`sub`/`email`/`name`/…, plus provider claims like `groups`) a resource server uses
  * to JIT auto-provision users when the access token itself is thin.
  *
  * `GET {userinfo_endpoint}` with `Authorization: Bearer <token>` → JSON → [[OidcUserInfo]]. The
  * endpoint URL is configured directly (discovering it from the AS metadata document's
  * `userinfo_endpoint` field is a future add). The caller MUST have already validated the token and
  * MUST check that the returned `sub` matches the token's `sub` before trusting the result.
  */
trait UserInfoService {
  def fetch(bearer: String): IO[UserInfoError, OidcUserInfo]
}
object UserInfoService {

  final case class Config(userInfoEndpoint: URL)

  final case class Live(client: Client, config: Config) extends UserInfoService {

    override def fetch(bearer: String): IO[UserInfoError, OidcUserInfo] = {
      val token: String = bearer.stripPrefix("Bearer ").trim
      val request: Request = Request.get(config.userInfoEndpoint).addHeader(Header.Authorization.Bearer(token))
      ZIO.scoped {
        for {
          response <- client.request(request).mapError(UserInfoError.RequestFailed(_))
          _ <- ZIO.fail(UserInfoError.NonSuccess(response.status.code)).unless(response.status.code / 100 == 2)
          body <- response.body.asString.mapError(UserInfoError.RequestFailed(_))
          json <- ZIO.fromEither(Json.parse(body)).mapError(e => UserInfoError.Malformed(e.toString))
          info <- ZIO.fromEither(OidcUserInfo.fromJson(json)).mapError(UserInfoError.Malformed(_))
        } yield info
      }
    }

  }
  object Live {
    val layer: URLayer[Client & Config, UserInfoService] = ZLayer.fromFunction(Live.apply)
  }

}

enum UserInfoError {
  case RequestFailed(cause: Throwable)
  case NonSuccess(status: Int)
  case Malformed(detail: String)

  def message: String = this match
    case RequestFailed(cause) => s"userinfo request failed: ${cause.getMessage}"
    case NonSuccess(status)   => s"userinfo endpoint returned non-2xx status: $status"
    case Malformed(detail)    => s"malformed userinfo response: $detail"
}
