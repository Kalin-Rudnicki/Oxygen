package oxygen.json

import oxygen.json.syntax.instances.*
import oxygen.predef.core.*
import zio.json.*

object autoInstances {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      String___ -> Json___
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit def stringCodecToJsonCodec[A: StringCodec]: JsonCodec[A] = StringCodec[A].toJsonCodec

  implicit def stringDecoderToJsonDecoder[A: StringDecoder]: JsonDecoder[A] = StringDecoder[A].toJsonDecoder

  implicit def stringEncoderToJsonEncoder[A: StringEncoder]: JsonEncoder[A] = StringEncoder[A].toJsonEncoder

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Json___ -> String___
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  implicit def jsonCodecToStringCodec[A: {JsonCodec, TypeTag}]: StringCodec[A] = JsonCodec[A].toStringCodec

  implicit def jsonDecoderToStringDecoder[A: {JsonDecoder, TypeTag}]: StringDecoder[A] = JsonDecoder[A].toStringDecoder

  implicit def jsonEncoderToStringEncoder[A: JsonEncoder]: StringEncoder[A] = JsonEncoder[A].toStringEncoder

}
