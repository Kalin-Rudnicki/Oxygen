package oxygen.json.syntax

import oxygen.predef.core.*
import zio.json.*

object instances {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      String___ -> Json___
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // =====| Companion |=====

  extension (self: JsonCodec.type) {
    def usingStringCodec[A: StringCodec]: JsonCodec[A] = StringCodec[A].toJsonCodec
  }

  extension (self: JsonDecoder.type) {
    def usingStringDecoder[A: StringDecoder]: JsonDecoder[A] = StringDecoder[A].toJsonDecoder
  }

  extension (self: JsonEncoder.type) {
    def usingStringEncoder[A: StringEncoder]: JsonEncoder[A] = StringEncoder[A].toJsonEncoder
  }

  extension (self: JsonFieldDecoder.type) {
    def fromStringDecoder[A: StringDecoder]: JsonFieldDecoder[A] = StringDecoder[A].toJsonFieldDecoder
  }

  extension (self: JsonFieldEncoder.type) {
    def fromStringEncoder[A: StringEncoder]: JsonFieldEncoder[A] = StringEncoder[A].toJsonFieldEncoder
  }

  // =====| Instance |=====

  extension [A](self: StringCodec[A]) {

    def toJsonCodec: JsonCodec[A] =
      JsonCodec.string.transformOrFail(
        self.decoder.decode,
        self.encoder.encode,
      )

  }

  extension [A](self: StringDecoder[A]) {

    def toJsonDecoder: JsonDecoder[A] =
      JsonDecoder.string.mapOrFail(self.decode)

    def toJsonFieldDecoder: JsonFieldDecoder[A] =
      JsonFieldDecoder.string.mapOrFail(self.decode)

  }
  extension [A](self: StringEncoder[A]) {

    def toJsonEncoder: JsonEncoder[A] =
      JsonEncoder.string.contramap(self.encode)

    def toJsonFieldEncoder: JsonFieldEncoder[A] =
      JsonFieldEncoder.string.contramap(self.encode)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Json___ -> String___
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // =====| Companion |=====

  extension (self: StringCodec.type) {

    def usingJsonCodec[A: JsonCodec: TypeTag]: StringCodec[A] = JsonCodec[A].toStringCodec

    def jsonString: StringCodec[String] = StringCodec.usingJsonCodec

  }

  extension (self: StringDecoder.type) {
    def usingJsonDecoder[A: JsonDecoder: TypeTag]: StringDecoder[A] = JsonDecoder[A].toStringDecoder
  }

  extension (self: StringEncoder.type) {
    def usingJsonEncoder[A: JsonEncoder]: StringEncoder[A] = JsonEncoder[A].toStringEncoder
  }

  // =====| Instance |=====

  extension [A](self: JsonCodec[A]) {

    def toStringCodec(implicit typeTag: TypeTag[A]): StringCodec[A] =
      StringCodec.string.transformOrFail(
        self.decoder.decodeJson,
        self.encoder.encodeJson(_).toString,
      )

  }

  extension [A](self: JsonDecoder[A]) {

    def toStringDecoder(implicit typeTag: TypeTag[A]): StringDecoder[A] =
      StringDecoder.string.mapOrFail(self.decodeJson)

  }

  extension [A](self: JsonEncoder[A]) {

    def toStringEncoder: StringEncoder[A] =
      StringEncoder.string.contramap(self.encodeJson(_).toString)

  }

}
