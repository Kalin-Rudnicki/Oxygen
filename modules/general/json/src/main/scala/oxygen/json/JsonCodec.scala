package oxygen.json

import java.time.*
import java.util.TimeZone
import java.util.UUID
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*
import scala.reflect.ClassTag

final case class JsonCodec[A](
    encoder: JsonEncoder[A],
    decoder: JsonDecoder[A],
) {

  def transform[B](ab: A => B, ba: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(ba), decoder.map(ab))

  def transformOrFail[B](ab: A => Either[String, B], ba: B => A): JsonCodec[B] =
    JsonCodec(encoder.contramap(ba), decoder.mapOrFail(ab))

  inline def autoTransform[B]: JsonCodec[B] = {
    val (ab, ba) = ProductGeneric.deriveTransform[A, B]
    transform(ab, ba)
  }

  def toStringCodecCompact: StringCodec[A] = StringCodec(encoder.toStringEncoderCompact, decoder.toStringDecoder)
  def toStringCodecPretty: StringCodec[A] = StringCodec(encoder.toStringEncoderPretty, decoder.toStringDecoder)
  def toStringCodec: StringCodec[A] = toStringCodecCompact

}
object JsonCodec extends JsonCodecLowPriority.LowPriority1 {

  inline def apply[A](using ev: JsonCodec[A]): JsonCodec[A] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given json: JsonCodec[Json] = fromEncoderAndDecoder
  given jsonString: JsonCodec[Json.Str] = fromEncoderAndDecoder
  given jsonBoolean: JsonCodec[Json.Bool] = fromEncoderAndDecoder
  given jsonNumber: JsonCodec[Json.Number] = fromEncoderAndDecoder
  given jsonArray: JsonCodec[Json.Arr] = fromEncoderAndDecoder
  given jsonObject: JsonCodec[Json.Obj] = fromEncoderAndDecoder
  given jsonNull: JsonCodec[Json.Null.type] = fromEncoderAndDecoder

  given string: JsonCodec[String] = fromEncoderAndDecoder
  given boolean: JsonCodec[Boolean] = fromEncoderAndDecoder
  given uuid: JsonCodec[UUID] = fromEncoderAndDecoder

  given bigDecimal: JsonCodec[BigDecimal] = fromEncoderAndDecoder
  given double: JsonCodec[Double] = fromEncoderAndDecoder
  given float: JsonCodec[Float] = fromEncoderAndDecoder
  given bigInt: JsonCodec[BigInt] = fromEncoderAndDecoder
  given long: JsonCodec[Long] = fromEncoderAndDecoder
  given int: JsonCodec[Int] = fromEncoderAndDecoder
  given short: JsonCodec[Short] = fromEncoderAndDecoder
  given byte: JsonCodec[Byte] = fromEncoderAndDecoder

  given option: [A: JsonCodec] => JsonCodec[Option[A]] = fromEncoderAndDecoder
  given nullable: [A: JsonCodec] => JsonCodec[Nullable[A]] = fromEncoderAndDecoder
  given specified: [A: JsonCodec] => JsonCodec[Specified[A]] = fromEncoderAndDecoder

  given arraySeq: [A] => (encoder: JsonCodec[A]) => JsonCodec[ArraySeq[A]] = fromEncoderAndDecoder
  given seq: [S[_]: SeqOps, A: {ClassTag, JsonCodec}] => JsonCodec[S[A]] = fromEncoderAndDecoder

  given map: [K: {JsonFieldEncoder, JsonFieldDecoder}, V: JsonCodec] => JsonCodec[Map[K, V]] = fromEncoderAndDecoder

  given tuple: [A <: Tuple: {JsonEncoder.TupleEncoder, JsonDecoder.TupleDecoder}] => JsonCodec[A] = fromEncoderAndDecoder

  given strictEnum: [A: StrictEnum] => JsonCodec[A] = fromEncoderAndDecoder
  given enumWithOther: [A: EnumWithOther] => JsonCodec[A] = fromEncoderAndDecoder

  given localTime: JsonCodec[LocalTime] = fromEncoderAndDecoder
  given localDate: JsonCodec[LocalDate] = fromEncoderAndDecoder
  given localDateTime: JsonCodec[LocalDateTime] = fromEncoderAndDecoder
  given zonedDateTime: JsonCodec[ZonedDateTime] = fromEncoderAndDecoder
  given offsetDateTime: JsonCodec[OffsetDateTime] = fromEncoderAndDecoder
  given offsetTime: JsonCodec[OffsetTime] = fromEncoderAndDecoder
  given instant: JsonCodec[Instant] = fromEncoderAndDecoder
  given duration: JsonCodec[Duration] = fromEncoderAndDecoder
  given period: JsonCodec[Period] = fromEncoderAndDecoder
  given zoneId: JsonCodec[ZoneId] = fromEncoderAndDecoder
  given zoneOffset: JsonCodec[ZoneOffset] = fromEncoderAndDecoder
  given timeZone: JsonCodec[TimeZone] = fromEncoderAndDecoder
  given monthDay: JsonCodec[MonthDay] = fromEncoderAndDecoder
  given year: JsonCodec[Year] = fromEncoderAndDecoder
  given yearMonth: JsonCodec[YearMonth] = fromEncoderAndDecoder
  given month: JsonCodec[Month] = fromEncoderAndDecoder

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def `enum`[A: StrictEnum]: JsonCodec[A] = strictEnum[A]

  def jsonStringUsingStringCodecSimple[A: StringCodec as codec]: JsonCodec[A] =
    JsonCodec(JsonEncoder.jsonStringFromStringEncoder(codec.encoder), JsonDecoder.jsonStringFromStringDecoderSimple(codec.decoder))
  def jsonStringUsingStringCodecDetailed[A: StringCodec as codec]: JsonCodec[A] =
    JsonCodec(JsonEncoder.jsonStringFromStringEncoder(codec.encoder), JsonDecoder.jsonStringFromStringDecoderDetailed(codec.decoder))
  def jsonStringUsingStringCodec[A: StringCodec as codec]: JsonCodec[A] =
    jsonStringUsingStringCodecDetailed[A]

  def jsonStringFromStringCodecSimple[A](codec: StringCodec[A]): JsonCodec[A] =
    JsonCodec(JsonEncoder.jsonStringFromStringEncoder(codec.encoder), JsonDecoder.jsonStringFromStringDecoderSimple(codec.decoder))
  def jsonStringFromStringCodecDetailed[A](codec: StringCodec[A]): JsonCodec[A] =
    JsonCodec(JsonEncoder.jsonStringFromStringEncoder(codec.encoder), JsonDecoder.jsonStringFromStringDecoderDetailed(codec.decoder))
  def jsonStringFromStringCodec[A](codec: StringCodec[A]): JsonCodec[A] =
    jsonStringFromStringCodecDetailed[A](codec)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def derived[A]: JsonCodec[A] = JsonCodec(JsonEncoder.derived[A], JsonDecoder.derived[A])

  private def deriveWrappedImpl[A: Type](using Quotes): Expr[JsonCodec[A]] = {
    type B
    val wrapping: ProductGeneric.SingleFieldCaseClassGeneric[A, B] = ProductGeneric.SingleFieldCaseClassGeneric.ofTypeField[A, B]
    given Type[B] = wrapping.field.tpe

    '{ ${ wrapping.field.summonTypeClass[JsonCodec] }.transform[A](${ wrapping.singleField.wrapExpr }, ${ wrapping.singleField.unwrapExpr }) }
  }

  /**
    * Expects [[A]] to be a case class with a single field.
    * Will then derive an instance
    *
    * Example:
    * ```scala
    * final case class Wrapped(value: String)
    * object Wrapped {
    *   given JsonCodec[Wrapped] = JsonCodec.deriveWrapped
    * }
    * ```
    */
  inline def deriveWrapped[A]: JsonCodec[A] = ${ deriveWrappedImpl[A] }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Low Priority Instances
//////////////////////////////////////////////////////////////////////////////////////////////////////

object JsonCodecLowPriority {

  trait LowPriority1 {

    given fromEncoderAndDecoder: [A] => (enc: JsonEncoder[A], dec: JsonDecoder[A]) => JsonCodec[A] = JsonCodec(enc, dec)

  }

}
