package oxygen.crypto.model

import oxygen.core.typeclass.StringCodec

final case class SymmetricEncryptedValue(
    iv: SymmetricEncryptedValue.IV,
    cipher: SymmetricEncryptedValue.Cipher,
) {

  def format: SymmetricEncryptedValue.Formatted =
    SymmetricEncryptedValue.Formatted(s"${iv.bytes.urlBase64}::${cipher.bytes.urlBase64}")

  def asymmetric(wrappedKey: AsymmetricEncryptedValue.WrappedKey): AsymmetricEncryptedValue =
    AsymmetricEncryptedValue(wrappedKey, this)

}
object SymmetricEncryptedValue {

  def unsafeFromString(value: String): SymmetricEncryptedValue =
    value.split("::").toList match
      case iv :: cipher :: Nil => SymmetricEncryptedValue(IV.base64(iv), Cipher.base64(cipher))
      case _                   => throw new RuntimeException("Malformed AsymmetricEncryptedValue")

  given StringCodec[SymmetricEncryptedValue] =
    StringCodec.string.transformCatchFail(SymmetricEncryptedValue.unsafeFromString, _.format)

  opaque type Formatted = String
  object Formatted {

    def apply(value: String): Formatted = value

    extension (value: Formatted)
      def untyped: String = value
      def unsafeParse: SymmetricEncryptedValue = SymmetricEncryptedValue.unsafeFromString(value)

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

    extension (value: Cipher)
      def bytes: Bytes.Raw = value

  }

}
