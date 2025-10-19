package oxygen.crypto.model

import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

final case class EncryptedValue(value: String) {

  def toIVCipher: Try[EncryptedValue.IVCipher] = EncryptedValue.IVCipher.parseString(value)

}
object EncryptedValue {

  val delim: String = "::"
  val b64String: Regex = "([A-Za-z0-9+/\\-_]+=*)".r
  val encodedReg: Regex = s"^$b64String$delim$b64String$$".r

  final case class IV(bytes: Array[Byte]) {
    def base64: String = Base64.urlEncoder.encodeToString(bytes)
  }

  final case class Cypher(bytes: Array[Byte]) {
    def base64: String = Base64.urlEncoder.encodeToString(bytes)
  }

  final case class IVCipher(iv: IV, cypher: Cypher) {

    def toEncryptedValue: EncryptedValue =
      EncryptedValue(s"${iv.base64}$delim${cypher.base64}")

  }
  object IVCipher {

    def parseString(value: String): Try[IVCipher] =
      for {
        (base64IV, base64Cypher) <- value match
          case encodedReg(iv, cypher) => Success((iv, cypher))
          case _                      => Failure(EncryptionError.MalformedEncryptedValue(value))
        iv <- Try { IV(Base64.urlDecoder.decode(base64IV)) }
        cypher <- Try { Cypher(Base64.urlDecoder.decode(base64Cypher)) }
      } yield EncryptedValue.IVCipher(iv, cypher)

  }

}
