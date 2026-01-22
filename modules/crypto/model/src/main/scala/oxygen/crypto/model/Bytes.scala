package oxygen.crypto.model

import oxygen.crypto.model.Base64 as B64

object Bytes {

  opaque type Raw <: Array[Byte] = Array[Byte]
  object Raw {

    def apply(value: Array[Byte]): Bytes.Raw = value

    extension (value: Bytes.Raw)
      def unwrap: Array[Byte] = value
      def standardBase64: Bytes.StandardBase64 = B64.stdEncoder.encodeToString(value)
      def urlBase64: Bytes.UrlBase64 = B64.urlEncoder.withoutPadding().encodeToString(value)

  }

  opaque type StandardBase64 <: String = String
  object StandardBase64 {

    def apply(value: String): Bytes.StandardBase64 = value

    extension (value: Bytes.StandardBase64)
      def unwrap: String = value
      def raw: Bytes.Raw = B64.stdDecoder.decode(value)

  }

  opaque type UrlBase64 <: String = String
  object UrlBase64 {

    def apply(value: String): Bytes.UrlBase64 = value

    extension (value: Bytes.UrlBase64)
      def unwrap: String = value
      def raw: Bytes.Raw = B64.urlDecoder.decode(value)

  }

}
