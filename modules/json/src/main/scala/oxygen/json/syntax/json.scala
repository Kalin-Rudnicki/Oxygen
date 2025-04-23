package oxygen.json.syntax

import oxygen.json.*

object json {

  // Encode

  extension [A](self: A)
    def toJsonAST(using encoder: JsonEncoder[A]): Json =
      encoder.encodeJsonAST(self)

  extension [A](self: A)
    def toJsonStringCompact(using encoder: JsonEncoder[A]): String =
      encoder.encodeJsonStringCompact(self)

  extension [A](self: A)
    def toJsonStringPretty(using encoder: JsonEncoder[A]): String =
      encoder.encodeJsonStringPretty(self)

  // Decode

  extension (self: Json)
    def fromJsonAST[A](using decoder: JsonDecoder[A]): Either[JsonError, A] =
      decoder.decodeJsonAST(self)

  extension (self: String)
    def fromJsonString[A](using decoder: JsonDecoder[A]): Either[JsonError, A] =
      decoder.decodeJsonString(self)

}
