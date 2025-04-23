package oxygen.json.syntax

import oxygen.core.TypeTag
import oxygen.core.syntax.either.*
import oxygen.core.typeclass.{StringCodec, StringDecoder, StringEncoder}
import oxygen.json.{JsonCodec, JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder}

object interop {

  // =====| Json => String |=====

  extension [A: TypeTag](self: JsonCodec[A])
    def toStringCodec: StringCodec[A] =
      StringCodec(self.encoder.toStringEncoder, self.decoder.toStringDecoder)

  extension [A: TypeTag](self: JsonDecoder[A])
    def toStringDecoder: StringDecoder[A] =
      StringDecoder.string.mapOrFail(self.decodeJsonString(_).leftMap(_.toString))

  extension [A](self: JsonEncoder[A])
    def toStringEncoder: StringEncoder[A] =
      self.encodeJsonStringCompact(_)

  extension (self: StringCodec.type)
    def fromJsonCodec[A: {JsonCodec, TypeTag}]: StringCodec[A] =
      JsonCodec[A].toStringCodec

  extension (self: JsonDecoder.type)
    def fromJsonDecoder[A: {JsonDecoder, TypeTag}]: StringDecoder[A] =
      JsonDecoder[A].toStringDecoder

  extension (self: JsonEncoder.type)
    def fromJsonEncoder[A: JsonEncoder]: StringEncoder[A] =
      JsonEncoder[A].toStringEncoder

    // =====| String => Json |=====

  extension [A](self: StringCodec[A])
    def toJsonCodec: JsonCodec[A] =
      JsonCodec(self.encoder.toJsonEncoder, self.decoder.toJsonDecoder)

  extension [A](self: StringEncoder[A]) {

    def toJsonEncoder: JsonEncoder[A] =
      JsonEncoder.string.contramap(self.encode)

    def toJsonFieldEncoder: JsonFieldEncoder[A] =
      JsonFieldEncoder.string.contramap(self.encode)

  }

  extension [A](self: StringDecoder[A]) {

    def toJsonDecoder: JsonDecoder[A] =
      JsonDecoder.string.mapOrFail(self.decode)

    def toJsonFieldDecoder: JsonFieldDecoder[A] =
      JsonFieldDecoder.string.mapOrFail(self.decode)

  }

  extension (self: JsonCodec.type)
    def usingStringCodec[A: StringCodec]: JsonCodec[A] =
      StringCodec[A].toJsonCodec

  extension (self: JsonDecoder.type)
    def usingStringDecoder[A: StringDecoder]: JsonDecoder[A] =
      StringDecoder[A].toJsonDecoder

  extension (self: JsonEncoder.type)
    def usingStringEncoder[A: StringEncoder]: JsonEncoder[A] =
      StringEncoder[A].toJsonEncoder

  extension (self: JsonFieldDecoder.type)
    def fromStringDecoder[A: StringDecoder]: JsonFieldDecoder[A] =
      StringDecoder[A].toJsonFieldDecoder

  extension (self: JsonFieldEncoder.type)
    def fromStringEncoder[A: StringEncoder]: JsonFieldEncoder[A] =
      StringEncoder[A].toJsonFieldEncoder

}
