package oxygen.json

final case class JsonCodec[A](
    encoder: JsonEncoder[A],
    decoder: JsonDecoder[A],
) {

  def transform[B](ab: A => B, ba: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(ba), decoder.map(ab))

  def transformOrFail[B](ab: A => Either[String, B], ba: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(ba), decoder.mapOrFail(ab))

}
object JsonCodec {

  inline def apply[A](using ev: JsonCodec[A]): JsonCodec[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given fromEncoderAndDecoder: [A] => (enc: JsonEncoder[A], dec: JsonDecoder[A]) => JsonCodec[A] = JsonCodec(enc, dec)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def derived[A]: JsonCodec[A] = JsonCodec(JsonEncoder.derived[A], JsonDecoder.derived[A])

}
