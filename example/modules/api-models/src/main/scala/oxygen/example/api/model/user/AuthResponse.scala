package oxygen.example.api.model.user

import oxygen.http.core.*
import oxygen.http.core.partial.*
import oxygen.http.core.partial.ResponseCodecNoStatus

final case class AuthResponse(
    user: User,
    authorization: UserToken,
)
object AuthResponse {

  given responseCodec: ResponseCodecNoStatus[AuthResponse] =

    (
      ResponseCodecNoStatus.fromBody[User] ++
        ResponseCodecNoStatus.header.plain.required[UserToken]("Authorization")
    ).transform(AuthResponse(_, _), res => (res.user, res.authorization))

}
