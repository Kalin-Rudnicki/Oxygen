package oxygen.json

import oxygen.core.typeclass.*

extension (self: StringCodec.type) {

  def usingJsonCodecCompact[A: JsonCodec as codec]: StringCodec[A] = codec.toStringCodecCompact
  def usingJsonCodecPretty[A: JsonCodec as codec]: StringCodec[A] = codec.toStringCodecPretty
  def usingJsonCodec[A: JsonCodec as codec]: StringCodec[A] = codec.toStringCodec

  def fromJsonCodecCompact[A](codec: JsonCodec[A]): StringCodec[A] = codec.toStringCodecCompact
  def fromJsonCodecPretty[A](codec: JsonCodec[A]): StringCodec[A] = codec.toStringCodecPretty
  def fromJsonCodec[A](codec: JsonCodec[A]): StringCodec[A] = codec.toStringCodec

  inline def fromDerivedJsonCodecCompact[A]: StringCodec[A] = JsonCodec.derived[A].toStringCodecCompact
  inline def fromDerivedJsonCodecPretty[A]: StringCodec[A] = JsonCodec.derived[A].toStringCodecPretty
  inline def fromDerivedJsonCodec[A]: StringCodec[A] = JsonCodec.derived[A].toStringCodec

}

extension (self: StringDecoder.type) {

  def usingJsonDecoder[A: JsonDecoder as dec]: StringDecoder[A] = dec.toStringDecoder

  def fromJsonDecoder[A](dec: JsonDecoder[A]): StringDecoder[A] = dec.toStringDecoder

  inline def fromDerivedJsonDecoder[A]: StringDecoder[A] = JsonDecoder.derived[A].toStringDecoder

}

extension (self: StringEncoder.type) {

  def usingJsonEncoderCompact[A: JsonEncoder as codec]: StringEncoder[A] = codec.toStringEncoderCompact
  def usingJsonEncoderPretty[A: JsonEncoder as codec]: StringEncoder[A] = codec.toStringEncoderPretty
  def usingJsonEncoder[A: JsonEncoder as codec]: StringEncoder[A] = codec.toStringEncoder

  def fromJsonEncoderCompact[A](codec: JsonEncoder[A]): StringEncoder[A] = codec.toStringEncoderCompact
  def fromJsonEncoderPretty[A](codec: JsonEncoder[A]): StringEncoder[A] = codec.toStringEncoderPretty
  def fromJsonEncoder[A](codec: JsonEncoder[A]): StringEncoder[A] = codec.toStringEncoder

  inline def fromDerivedJsonEncoderCompact[A]: StringEncoder[A] = JsonEncoder.derived[A].toStringEncoderCompact
  inline def fromDerivedJsonEncoderPretty[A]: StringEncoder[A] = JsonEncoder.derived[A].toStringEncoderPretty
  inline def fromDerivedJsonEncoder[A]: StringEncoder[A] = JsonEncoder.derived[A].toStringEncoder

}
