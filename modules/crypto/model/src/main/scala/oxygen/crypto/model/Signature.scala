package oxygen.crypto.model

object Signature {

  opaque type Raw = Bytes.Raw
  object Raw {

    def apply(value: Array[Byte]): Raw = Bytes.Raw(value)

    extension (value: Raw)
      def bytes: Bytes.Raw = value
      def toBase64: Base64 = value.urlBase64

  }

  opaque type Base64 = Bytes.UrlBase64
  object Base64 {

    def apply(value: String): Base64 = Bytes.UrlBase64(value)

    extension (value: Base64)
      def bytes: Bytes.UrlBase64 = value
      def toRaw: Raw = value.raw

  }

}
