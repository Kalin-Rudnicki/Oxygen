package oxygen.example.api.model.user

import oxygen.crypto.model.JWT
import oxygen.schema.*

final case class UserToken(jwt: JWT.Std[User]) {
  val user: User = jwt.payload.payload
}
object UserToken {
  given schema: PlainTextSchema[UserToken] = PlainTextSchema.standardJWT[User].transform(UserToken(_), _.jwt)
}
