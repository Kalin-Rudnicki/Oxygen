package oxygen.schema

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.{SourcePosition, TypeTag}
import oxygen.crypto.model.{BearerToken, JWT, Password}
import oxygen.meta.k0.*
import oxygen.predef.core.*
import scala.quoted.*

sealed trait PlainTextSchema[A] extends SchemaLike[A] {

  override final type S[a] = PlainTextSchema[a]

  override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A)(using pos: SourcePosition): PlainTextSchema[B] =
    PlainTextSchema.Transform(this, newTypeTag, ab, ba, pos)
  override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A)(using pos: SourcePosition): PlainTextSchema[B] =
    PlainTextSchema.TransformOrFail(this, newTypeTag, ab, ba, pos)

  final def @@(wrapper: PlainTextSchema.Encoding): PlainTextSchema[A] = PlainTextSchema.EncodedText(this, wrapper)

  final def withFormats(format0: String, formatN: String*): PlainTextSchema[A] = this.withFormats(NonEmptyList(format0, formatN.toList))
  final def withFormats(formats: NonEmptyList[String]): PlainTextSchema[A] = this match
    case PlainTextSchema.WithFormats(underlying, existingFormats) => PlainTextSchema.WithFormats(underlying, (existingFormats ++ formats).distinct)
    case _                                                        => PlainTextSchema.WithFormats(this, formats)

}
object PlainTextSchema extends PlainTextSchemaLowPriority.LowPriority1 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given string: PlainTextSchema[String] = StringSchema
  given text: PlainTextSchema[Text] = StringSchema.transform(Text.fromString, _.toString)
  given uuid: PlainTextSchema[UUID] = PlainTextSchema.fromStringCodecWithFormat("UUID", "RFC 4122")
  given boolean: PlainTextSchema[Boolean] = PlainTextSchema.fromStringCodecWithFormat("boolean")

  given byte: PlainTextSchema[Byte] = PlainTextSchema.fromStringCodecWithFormat("Int8")
  given short: PlainTextSchema[Short] = PlainTextSchema.fromStringCodecWithFormat("Int16")
  given int: PlainTextSchema[Int] = PlainTextSchema.fromStringCodecWithFormat("Int32")
  given long: PlainTextSchema[Long] = PlainTextSchema.fromStringCodecWithFormat("Int64")
  given bigInt: PlainTextSchema[BigInt] = PlainTextSchema.fromStringCodecWithFormat("BigInt")

  given float: PlainTextSchema[Float] = PlainTextSchema.fromStringCodecWithFormat("Float32")
  given double: PlainTextSchema[Double] = PlainTextSchema.fromStringCodecWithFormat("Float64")
  given bigDecimal: PlainTextSchema[BigDecimal] = PlainTextSchema.fromStringCodecWithFormat("BigDecimal")

  given period: PlainTextSchema[Period] = standardJavaTime.period
  given instant: PlainTextSchema[Instant] = standardJavaTime.instant
  given offsetDateTime: PlainTextSchema[OffsetDateTime] = standardJavaTime.offsetDateTime
  given zonedDateTime: PlainTextSchema[ZonedDateTime] = standardJavaTime.zonedDateTime
  given zoneId: PlainTextSchema[ZoneId] = standardJavaTime.zoneId
  given zoneOffset: PlainTextSchema[ZoneOffset] = standardJavaTime.zoneOffset
  given timeZone: PlainTextSchema[TimeZone] = standardJavaTime.timeZone

  given duration: PlainTextSchema[Duration] = standardJavaTime.duration
  given localDate: PlainTextSchema[LocalDate] = standardJavaTime.localDate
  given localTime: PlainTextSchema[LocalTime] = standardJavaTime.localTime
  given localDateTime: PlainTextSchema[LocalDateTime] = standardJavaTime.localDateTime

  given bearerToken: PlainTextSchema[BearerToken] =
    PlainTextSchema.BearerTokenSchema[BearerToken](TypeTag.derived, PlainTextSchema.string, _.asRight, identity)

  given standardJWT: [A: JsonSchema] => PlainTextSchema[JWT.Std[A]] = {
    given JsonSchema[JWT.StandardPayload[A]] = instances.standardJWTPayloadSchema[A]
    PlainTextSchema.jwt[JWT.StandardPayload[A]]
  }

  given password: PlainTextSchema[Password.PlainText] =
    PlainTextSchema.fromStringCodec

  def fromStringCodec[A: StringCodec as codec](using pos: SourcePosition): PlainTextSchema[A] =
    PlainTextSchema.FromStringCodec(codec, pos)
  def fromStringCodecWithFormat[A: StringCodec as codec](format0: String, formatN: String*)(using pos: SourcePosition): PlainTextSchema[A] =
    PlainTextSchema.FromStringCodec(codec, pos).withFormats(format0, formatN*)

  def jsonString[A: JsonSchema as schema]: PlainTextSchema[A] = PlainTextSchema.JsonEncoded(schema)

  object standardJavaTime {

    given period: PlainTextSchema[Period] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Period)")(using StringCodec.standardJavaTime.period)
    given instant: PlainTextSchema[Instant] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Instant)")(using StringCodec.standardJavaTime.instant)
    given offsetDateTime: PlainTextSchema[OffsetDateTime] =
      PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Date Time with Offset)")(using StringCodec.standardJavaTime.offsetDateTime)
    given zonedDateTime: PlainTextSchema[ZonedDateTime] =
      PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Date Time with Zone)")(using StringCodec.standardJavaTime.zonedDateTime)
    given zoneId: PlainTextSchema[ZoneId] = PlainTextSchema.fromStringCodecWithFormat("IANA Time Zone Database")(using StringCodec.standardJavaTime.zoneId)
    given zoneOffset: PlainTextSchema[ZoneOffset] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Zone Offset)")(using StringCodec.standardJavaTime.zoneOffset)
    given timeZone: PlainTextSchema[TimeZone] = PlainTextSchema.fromStringCodecWithFormat("IANA Time Zone Database")(using StringCodec.standardJavaTime.timeZone)

    given duration: PlainTextSchema[Duration] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Duration)")(using StringCodec.standardJavaTime.duration)
    given localDate: PlainTextSchema[LocalDate] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Date)")(using StringCodec.standardJavaTime.localDate)
    given localTime: PlainTextSchema[LocalTime] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Time)")(using StringCodec.standardJavaTime.localTime)
    given localDateTime: PlainTextSchema[LocalDateTime] = PlainTextSchema.fromStringCodecWithFormat("ISO 8601 (Date Time)")(using StringCodec.standardJavaTime.localDateTime)

  }

  object oxygenTime {

    given duration: PlainTextSchema[Duration] = PlainTextSchema.fromStringCodec(using StringCodec.oxygenTime.duration)
    given localDate: PlainTextSchema[LocalDate] = PlainTextSchema.fromStringCodecWithFormat("Oxygen (Date)", "ISO 8601 (Date)")(using StringCodec.oxygenTime.localDate)
    given localTime: PlainTextSchema[LocalTime] = PlainTextSchema.fromStringCodec(using StringCodec.oxygenTime.localTime)
    given localDateTime: PlainTextSchema[LocalDateTime] = PlainTextSchema.fromStringCodec(using StringCodec.oxygenTime.localDateTime)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[schema] case object StringSchema extends PlainTextSchema[String] {
    val pos: SourcePosition = SourcePosition.derived
    override val typeTag: TypeTag[String] = TypeTag[String]
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("PlainText")
    override def decode(string: String): Either[String, String] = string.asRight
    override def encode(value: String): String = value
  }

  private[schema] trait EnumSchema[A] extends PlainTextSchema[A] {
    val encodedValues: Seq[String]
    override protected final def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("PlainText.Enum", "values" -> encodedValues.mkString("[", ",", "]"), "caseSensitive" -> caseSensitive.toString, "exhaustive" -> exhaustive.toString)
    val caseSensitive: Boolean
    val exhaustive: Boolean
  }

  private[schema] final case class StrictEnumSchema[A](strictEnum: StrictEnum[A]) extends EnumSchema[A] {

    override val typeTag: TypeTag[A] = strictEnum.typeTag
    override val encodedValues: Seq[String] = strictEnum.encodedValues
    override val caseSensitive: Boolean = false
    override val exhaustive: Boolean = true

    override def decode(string: String): Either[String, A] = strictEnum.decodeEitherWithHint(string)

    override def encode(value: A): String = strictEnum.encode(value)

  }

  private[schema] final case class EnumWithOtherSchema[A](strictEnum: EnumWithOther[A]) extends EnumSchema[A] {

    override val typeTag: TypeTag[A] = strictEnum.typeTag
    override val encodedValues: Seq[String] = strictEnum.encodedValues
    override val caseSensitive: Boolean = false
    override val exhaustive: Boolean = false

    override def decode(string: String): Either[String, A] = strictEnum.decode(string).asRight

    override def encode(value: A): String = strictEnum.encode(value)

  }

  private[schema] final case class BearerTokenSchema[A](
      typeTag: TypeTag[A],
      payloadSchema: PlainTextSchema[?],
      fromToken: BearerToken => Either[String, A],
      toToken: A => BearerToken,
  ) extends PlainTextSchema[A] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("BearerToken", "payload" -> builder.referenceOf(payloadSchema))
    override def decode(string: String): Either[String, A] =
      BearerToken.bearerStringCodec.decoder.decodeDetailed(string).flatMap(fromToken(_).leftMap("Unable to decode bearer token payload: " + _))
    override def encode(value: A): String =
      BearerToken.bearerStringCodec.encoder.encode(toToken(value))
  }

  private[schema] final case class JsonEncoded[A](underlying: JsonSchema[A]) extends PlainTextSchema[A] {
    override val typeTag: TypeTag[A] = underlying.typeTag

    // this exactly matches the reference string of the corresponding JsonSchema so that there is no penalty or duplication from switching back and forth
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = builder.referenceOf(underlying)

    override def decode(string: String): Either[String, A] = underlying.decode(string)
    override def encode(value: A): String = underlying.encode(value)
  }

  private[schema] final case class FromStringCodec[A](codec: StringCodec[A], pos: SourcePosition) extends PlainTextSchema[A] {

    override val typeTag: TypeTag[A] = codec.decoder.typeInfo

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("FromStringCodec", "pos" -> pos.toString)

    override def decode(string: String): Either[String, A] = codec.decoder.decodeDetailed(string)
    override def encode(value: A): String = codec.encoder.encode(value)

  }

  private[schema] final case class WithFormats[A](underlying: PlainTextSchema[A], formats: NonEmptyList[String]) extends PlainTextSchema[A] {

    override val typeTag: TypeTag[A] = underlying.typeTag

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = formats match
      case NonEmptyList(head, Nil) => withHeader("WithFormat", "format" -> head, "underlying" -> builder.referenceOf(underlying))
      case formats                 => withHeader("WithFormat", "formats" -> formats.mkString("[", ",", "]"), "underlying" -> builder.referenceOf(underlying))

    private val formatsSuffix: String =
      formats match
        case NonEmptyList(head, Nil) => s"\n  Accepted Format: $head"
        case formats                 => "\n  Accepted Formats:" + formats.map { f => s"\n    - $f" }.mkString

    override def decode(string: String): Either[String, A] = underlying.decode(string).leftMap { _ + formatsSuffix }
    override def encode(value: A): String = underlying.encode(value)

  }

  private[schema] final case class EncodedText[A](underlying: PlainTextSchema[A], encoding: PlainTextSchema.Encoding) extends PlainTextSchema[A] {

    override val typeTag: TypeTag[A] = underlying.typeTag

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = s"Encoded<${encoding.name}>(${builder.referenceOf(underlying)})"

    override def decode(string: String): Either[String, A] =
      encoding.codec.decoder.decodeDetailed(string).flatMap(underlying.decode)

    override def encode(value: A): String =
      encoding.codec.encoder.encode(underlying.encode(value))

  }

  private[schema] final case class Transform[A, B](
      underlying: PlainTextSchema[A],
      typeTag: TypeTag[B],
      ab: A => B,
      ba: B => A,
      pos: SourcePosition,
  ) extends PlainTextSchema[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("PlainText.Transform", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
    override def decode(string: String): Either[String, B] = underlying.decode(string).map(ab)
    override def encode(value: B): String = underlying.encode(ba(value))
  }

  private[schema] final case class TransformOrFail[A, B](
      underlying: PlainTextSchema[A],
      typeTag: TypeTag[B],
      ab: A => Either[String, B],
      ba: B => A,
      pos: SourcePosition,
  ) extends PlainTextSchema[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("PlainText.Transform", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
    override def decode(string: String): Either[String, B] = underlying.decode(string).flatMap(ab)
    override def encode(value: B): String = underlying.encode(ba(value))
  }

  final case class Encoding(codec: StringCodec[String], name: String) {
    override def toString: String = s"PlainTextSchema.Encoding($name)"
  }
  object Encoding {

    val base64WithPadding: Encoding = Encoding(StringCodec.base64, "Base64 (With Padding)")
    val base64UrlWithPadding: Encoding = Encoding(StringCodec.base64Url, "Base64 Url (With Padding)")
    val base64MimeWithPadding: Encoding = Encoding(StringCodec.base64Mime, "Base64 Mime (With Padding)")

    val base64WithoutPadding: Encoding = Encoding(StringCodec.base64NoPadding, "Base64 (Without Padding)")
    val base64UrlWithoutPadding: Encoding = Encoding(StringCodec.base64UrlNoPadding, "Base64 Url (Without Padding)")
    val base64MimeWithoutPadding: Encoding = Encoding(StringCodec.base64MimeNoPadding, "Base64 Mime (Without Padding)")

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def deriveWrappedImpl[A: Type](using Quotes): Expr[PlainTextSchema[A]] = {
    type B
    val wrapping: ProductGeneric.SingleFieldCaseClassGeneric[A, B] = ProductGeneric.SingleFieldCaseClassGeneric.ofTypeField[A, B]
    given Type[B] = wrapping.field.tpe

    '{ ${ wrapping.field.summonTypeClass[PlainTextSchema] }.transform[A](${ wrapping.singleField.wrapExpr }, ${ wrapping.singleField.unwrapExpr }) }
  }

  /**
    * Expects [[A]] to be a case class with a single field.
    * Will then derive an instance
    *
    * Example:
    * ```scala
    * final case class Wrapped(value: String)
    * object Wrapped {
    *   given PlainTextSchema[Wrapped] = PlainTextSchema.deriveWrapped
    * }
    * ```
    */
  inline def deriveWrapped[A]: PlainTextSchema[A] = ${ deriveWrappedImpl[A] }

}

object PlainTextSchemaLowPriority {

  trait LowPriority1 {

    given jwt: [A: JsonSchema as payloadSchema] => PlainTextSchema[JWT[A]] = {
      given TypeTag[A] = payloadSchema.typeTag
      PlainTextSchema.BearerTokenSchema[JWT[A]](TypeTag.derived, PlainTextSchema.JsonEncoded(payloadSchema), JWT.decode[A](_)(using payloadSchema.jsonDecoder), _.token)
    }

    given strictEnum: [A: StrictEnum as e] => PlainTextSchema[A] = PlainTextSchema.StrictEnumSchema(e)
    given enumWithOther: [A: EnumWithOther as e] => PlainTextSchema[A] = PlainTextSchema.EnumWithOtherSchema(e)

    def `enum`[A: StrictEnum]: PlainTextSchema[A] = strictEnum[A]

  }

}
