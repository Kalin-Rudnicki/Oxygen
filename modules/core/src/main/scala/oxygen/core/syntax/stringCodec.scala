package oxygen.core.syntax

import oxygen.core.typeclass.{StringDecoder, StringEncoder}

object stringCodec {

  extension (self: String) {

    def fromStringError[A: StringDecoder]: Either[StringDecoder.Error, A] = StringDecoder[A].decodeError(self)
    def fromString[A: StringDecoder]: Either[String, A] = StringDecoder[A].decode(self)
    def fromStringSimple[A: StringDecoder]: Either[String, A] = StringDecoder[A].decodeSimple(self)

  }

  extension [A: StringEncoder](self: A) {

    def encodeToString: String = StringEncoder[A].encode(self)

  }

}
