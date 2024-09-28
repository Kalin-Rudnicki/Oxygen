package oxygen.json

import oxygen.json.syntax.instances.*
import oxygen.predef.core.*
import zio.json.*

object instances {

  implicit def nonEmptyListJsonCodec[A: JsonCodec]: JsonCodec[NonEmptyList[A]] =
    JsonCodec
      .list[A]
      .transformOrFail(
        NonEmptyList.fromList(_).toRight("NonEmptyList was empty"),
        _.toList,
      )

  implicit val typeRefSingleJsonCodec: JsonCodec[TypeTag.TypeRef.Single] = JsonCodec.derived
  implicit val typeRefJsonCodec: JsonCodec[TypeTag.TypeRef] = JsonCodec.derived

  implicit val typeTagJsonCodec: JsonCodec[TypeTag[?]] = {
    given JsonCodec[Class[?]] = JsonCodec.usingStringCodec

    final case class Encoded(
        tag: TypeTag.TypeRef,
        closestClass: Class[?],
    )

    JsonCodec
      .derived[Encoded]
      .transform(
        enc => TypeTag(enc.tag, enc.closestClass),
        dec => Encoded(dec.tag, dec.closestClass),
      )
  }

  implicit val throwableJsonCodec: JsonCodec[Throwable] =
    JsonCodec[EncodedThrowable].transform(_.toThrowable, EncodedThrowable.fromThrowable)

  implicit def fieldDecoderFromStringDecoder[A: StringDecoder]: JsonFieldDecoder[A] = StringDecoder[A].toJsonFieldDecoder

  implicit def fieldEncoderFromStringEncoder[A: StringEncoder]: JsonFieldEncoder[A] = StringEncoder[A].toJsonFieldEncoder

}
