package oxygen.crypto.model

import oxygen.json.*
import oxygen.predef.core.*

final case class JWTHeader(alg: JWTHeader.Alg, typ: JWTHeader.Type) derives JsonCodec
object JWTHeader {

  enum Alg derives StrictEnum {
    case HS256, RS256, none

    final def toJWTHeader: JWTHeader = JWTHeader(this, Type.JWT)

  }

  enum Type derives StrictEnum { case JWT }

}
