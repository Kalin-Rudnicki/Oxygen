package oxygen.crypto.model

import oxygen.json.*
import oxygen.predef.core.*

final case class JWTHeader(alg: JWTHeader.Alg, typ: JWTHeader.Type) derives JsonCodec
object JWTHeader {

  enum Alg extends Enum[Alg] {
    case HS256, RS256, none

    final def toJWTHeader: JWTHeader = JWTHeader(this, Type.JWT)

  }
  object Alg extends Enum.Companion[Alg]

  enum Type extends Enum[Type] { case JWT }
  object Type extends Enum.Companion[Type]

}
