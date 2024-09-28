package oxygen.json

import zio.json.*

final case class InJsonString[A](value: A)
object InJsonString {

  implicit def jsonCodec[A: JsonCodec]: JsonCodec[InJsonString[A]] =
    JsonCodec.string.transformOrFail(
      _.fromJson[A].map(InJsonString(_)),
      _.value.toJson,
    )

  implicit def jsonDecoder[A: JsonDecoder]: JsonDecoder[InJsonString[A]] =
    JsonDecoder.string.mapOrFail(_.fromJson[A].map(InJsonString(_)))

  implicit def jsonEncoder[A: JsonEncoder]: JsonEncoder[InJsonString[A]] =
    JsonEncoder.string.contramap(_.value.toJson)

}
