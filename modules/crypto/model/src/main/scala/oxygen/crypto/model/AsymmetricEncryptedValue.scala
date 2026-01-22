package oxygen.crypto.model

import oxygen.core.typeclass.StringCodec
import oxygen.crypto.model.SymmetricEncryptedValue.{Cipher, IV}

final case class AsymmetricEncryptedValue(
    key: AsymmetricEncryptedValue.WrappedKey,
    symmetric: SymmetricEncryptedValue,
) {

  def iv: IV = symmetric.iv
  def cipher: Cipher = symmetric.cipher

  def format: AsymmetricEncryptedValue.Formatted =
    AsymmetricEncryptedValue.Formatted(s"${key.bytes.urlBase64}::${iv.bytes.urlBase64}::${cipher.bytes.urlBase64}")

}
object AsymmetricEncryptedValue {

  def apply(key: WrappedKey, iv: IV, cipher: Cipher): AsymmetricEncryptedValue =
    AsymmetricEncryptedValue(key, SymmetricEncryptedValue(iv, cipher))

  def unsafeFromString(value: String): AsymmetricEncryptedValue =
    value.split("::").toList match
      case key :: iv :: cipher :: Nil => AsymmetricEncryptedValue(WrappedKey.base64(key), IV.base64(iv), Cipher.base64(cipher))
      case _                          => throw new RuntimeException("Malformed AsymmetricEncryptedValue")

  given StringCodec[AsymmetricEncryptedValue] =
    StringCodec.string.transformCatchFail(AsymmetricEncryptedValue.unsafeFromString, _.format)

  opaque type Formatted = String
  object Formatted {

    def apply(value: String): Formatted = value

    extension (value: Formatted)
      def untyped: String = value
      def unsafeParse: AsymmetricEncryptedValue = AsymmetricEncryptedValue.unsafeFromString(value)

  }

  opaque type WrappedKey = Bytes.Raw
  object WrappedKey {

    def apply(value: Array[Byte]): WrappedKey = Bytes.Raw(value)
    def base64(value: String): WrappedKey = Bytes.UrlBase64(value).raw

    extension (value: WrappedKey)
      def bytes: Bytes.Raw = value

  }

}
