package oxygen.crypto.model

import oxygen.core.typeclass.StringCodec
import oxygen.crypto.model.AsymmetricEncryptedValue.WrappedKey
import oxygen.crypto.model.SymmetricEncryptedValue.{Cipher, IV}
import oxygen.json.JsonCodec
import scala.util.matching.Regex

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      EncryptedValue
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait EncryptedValue {
  def format: EncryptedValue.Formatted
  def iv: IV
  def cipher: Cipher
}
object EncryptedValue {

  def unsafeFromString(value: String): EncryptedValue = value match
    case SymmetricEncryptedValue.regex(iv, cipher)       => SymmetricEncryptedValue(IV.base64(iv), Cipher.base64(cipher))
    case AsymmetricEncryptedValue.regex(key, iv, cipher) => AsymmetricEncryptedValue(WrappedKey.base64(key), IV.base64(iv), Cipher.base64(cipher))
    case _                                               => throw new RuntimeException("Malformed EncryptedValue")

  given stringCodec: StringCodec[EncryptedValue] = StringCodec.string.transformCatchFail(EncryptedValue.unsafeFromString, _.format.untyped)
  given jsonCodec: JsonCodec[EncryptedValue] = JsonCodec.string.transformAttempt(EncryptedValue.unsafeFromString, _.format.untyped)

  sealed trait Formatted {
    val untyped: String
    def unsafeParse: EncryptedValue = EncryptedValue.unsafeFromString(untyped)
  }
  object Formatted {

    final case class Unknown(untyped: String) extends EncryptedValue.Formatted

    given stringCodec: StringCodec[EncryptedValue.Formatted] = StringCodec.string.transform(Formatted.Unknown(_), _.untyped)
    given jsonCodec: JsonCodec[EncryptedValue.Formatted] = JsonCodec.string.transform(Formatted.Unknown(_), _.untyped)

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      SymmetricEncryptedValue
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class SymmetricEncryptedValue(
    iv: SymmetricEncryptedValue.IV,
    cipher: SymmetricEncryptedValue.Cipher,
) extends EncryptedValue {

  override def format: SymmetricEncryptedValue.Formatted =
    SymmetricEncryptedValue.Formatted(s"${iv.bytes.urlBase64}::${cipher.bytes.urlBase64}")

  def asymmetric(wrappedKey: AsymmetricEncryptedValue.WrappedKey): AsymmetricEncryptedValue =
    AsymmetricEncryptedValue(wrappedKey, this)

}
object SymmetricEncryptedValue {

  val regex: Regex = "^([^:]*)::([^:]*)$".r

  def unsafeFromString(value: String): SymmetricEncryptedValue = value match
    case SymmetricEncryptedValue.regex(iv, cipher) => SymmetricEncryptedValue(IV.base64(iv), Cipher.base64(cipher))
    case _                                         => throw new RuntimeException("Malformed SymmetricEncryptedValue")

  given stringCodec: StringCodec[SymmetricEncryptedValue] = StringCodec.string.transformCatchFail(SymmetricEncryptedValue.unsafeFromString, _.format.untyped)
  given jsonCodec: JsonCodec[SymmetricEncryptedValue] = JsonCodec.string.transformAttempt(SymmetricEncryptedValue.unsafeFromString, _.format.untyped)

  final case class Formatted(untyped: String) extends EncryptedValue.Formatted {
    override def unsafeParse: SymmetricEncryptedValue = SymmetricEncryptedValue.unsafeFromString(untyped)
  }
  object Formatted {
    given stringCodec: StringCodec[SymmetricEncryptedValue.Formatted] = StringCodec.string.transform(SymmetricEncryptedValue.Formatted(_), _.untyped)
    given jsonCodec: JsonCodec[SymmetricEncryptedValue.Formatted] = JsonCodec.string.transform(SymmetricEncryptedValue.Formatted(_), _.untyped)
  }

  opaque type IV = Bytes.Raw
  object IV {

    private lazy val rand = new java.security.SecureRandom()

    def apply(value: Array[Byte]): IV = Bytes.Raw(value)
    def base64(value: String): IV = Bytes.UrlBase64(value).raw

    extension (value: IV)
      def bytes: Bytes.Raw = value

    def generate(): IV = {
      val bytes = new Array[Byte](12)
      rand.nextBytes(bytes)
      Bytes.Raw(bytes)
    }

  }

  opaque type Cipher = Bytes.Raw
  object Cipher {

    def apply(value: Array[Byte]): Cipher = Bytes.Raw(value)
    def base64(value: String): Cipher = Bytes.UrlBase64(value).raw
    def stringBytes(value: String): Cipher = Bytes.Raw.stringBytes(value)

    extension (value: Cipher)
      def bytes: Bytes.Raw = value

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      AsymmetricEncryptedValue
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class AsymmetricEncryptedValue(
    key: AsymmetricEncryptedValue.WrappedKey,
    symmetric: SymmetricEncryptedValue,
) extends EncryptedValue {

  def iv: IV = symmetric.iv
  def cipher: Cipher = symmetric.cipher

  override def format: AsymmetricEncryptedValue.Formatted =
    AsymmetricEncryptedValue.Formatted(s"${key.bytes.urlBase64}::${iv.bytes.urlBase64}::${cipher.bytes.urlBase64}")

}
object AsymmetricEncryptedValue {

  val regex: Regex = "^([^:]*)::([^:]*)::([^:]*)$".r

  def apply(key: WrappedKey, iv: IV, cipher: Cipher): AsymmetricEncryptedValue =
    AsymmetricEncryptedValue(key, SymmetricEncryptedValue(iv, cipher))

  def unsafeFromString(value: String): AsymmetricEncryptedValue = value match
    case AsymmetricEncryptedValue.regex(key, iv, cipher) => AsymmetricEncryptedValue(WrappedKey.base64(key), IV.base64(iv), Cipher.base64(cipher))
    case _                                               => throw new RuntimeException("Malformed AsymmetricEncryptedValue")

  given stringCodec: StringCodec[AsymmetricEncryptedValue] = StringCodec.string.transformCatchFail(AsymmetricEncryptedValue.unsafeFromString, _.format.untyped)
  given jsonCodec: JsonCodec[AsymmetricEncryptedValue] = JsonCodec.string.transformAttempt(AsymmetricEncryptedValue.unsafeFromString, _.format.untyped)

  final case class Formatted(untyped: String) extends EncryptedValue.Formatted {
    override def unsafeParse: AsymmetricEncryptedValue = AsymmetricEncryptedValue.unsafeFromString(untyped)
  }
  object Formatted {
    given stringCodec: StringCodec[AsymmetricEncryptedValue.Formatted] = StringCodec.string.transform(AsymmetricEncryptedValue.Formatted(_), _.untyped)
    given jsonCodec: JsonCodec[AsymmetricEncryptedValue.Formatted] = JsonCodec.string.transform(AsymmetricEncryptedValue.Formatted(_), _.untyped)
  }

  opaque type WrappedKey = Bytes.Raw
  object WrappedKey {

    def apply(value: Array[Byte]): WrappedKey = Bytes.Raw(value)
    def base64(value: String): WrappedKey = Bytes.UrlBase64(value).raw

    extension (value: WrappedKey)
      def bytes: Bytes.Raw = value

  }

}
