package oxygen.crypto.model

import oxygen.core.typeclass.StringCodec
import oxygen.crypto.model.Base64 as B64
import oxygen.json.JsonCodec

object KeyFormat {

  opaque type DER <: Bytes.Raw = Bytes.Raw
  object DER {

    def apply(value: Array[Byte]): DER = Bytes.Raw(value)

    given stringCodec: StringCodec[KeyFormat.DER] = KeyFormat.Base64.stringCodec.transformCatchFail(_.toDER, _.toBase64)
    given jsonCodec: JsonCodec[KeyFormat.DER] = JsonCodec.jsonStringUsingStringCodecDetailed

    extension (value: DER)
      def bytes: Array[Byte] = value
      def toBase64: Base64 = value.standardBase64

  }

  opaque type Base64 <: Bytes.StandardBase64 = Bytes.StandardBase64
  object Base64 {

    def apply(value: String): Base64 = Bytes.StandardBase64(value)
    def fromPlain(value: String): Base64 = Bytes.StandardBase64(B64.stdEncoder.encodeToString(value.getBytes))

    given stringCodec: StringCodec[KeyFormat.Base64] = StringCodec.string.transformCatchFail(KeyFormat.Base64(_), v => v)
    given jsonCodec: JsonCodec[KeyFormat.Base64] = JsonCodec.jsonStringUsingStringCodecDetailed

    extension (value: Base64)
      def toDER: DER = value.raw
      def toPKCS8: PKCS8PEM = s"-----BEGIN PRIVATE KEY-----\n$value\n-----END PRIVATE KEY-----"
      def toX509: X509PEM = s"-----BEGIN PUBLIC KEY-----\n$value\n-----END PUBLIC KEY-----"

    private[KeyFormat] def cleanKey(value: PKCS8PEM | X509PEM): Base64 =
      Bytes.StandardBase64(value.replaceAll("-----[^\\-\n]*(?:BEGIN|END)[^\\-\n]*-----|\\s+", ""))

  }

  opaque type PKCS8PEM <: String = String
  object PKCS8PEM {

    def apply(value: String): PKCS8PEM = value

    given stringCodec: StringCodec[KeyFormat.PKCS8PEM] = StringCodec.string
    given jsonCodec: JsonCodec[KeyFormat.PKCS8PEM] = JsonCodec.jsonStringUsingStringCodecDetailed

    extension (value: PKCS8PEM)
      def toBase64: Base64 = Base64.cleanKey(value)

  }

  opaque type X509PEM <: String = String
  object X509PEM {

    def apply(value: String): X509PEM = value

    given stringCodec: StringCodec[KeyFormat.X509PEM] = StringCodec.string
    given jsonCodec: JsonCodec[KeyFormat.X509PEM] = JsonCodec.jsonStringUsingStringCodecDetailed

    extension (value: X509PEM)
      def toBase64: Base64 = Base64.cleanKey(value)

  }

}
