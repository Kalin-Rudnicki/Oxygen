package oxygen.schema

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.TypeTag
import oxygen.crypto.model.{BearerToken, JWT, Password}
import oxygen.predef.core.*

sealed trait PlainTextSchema[A] extends SchemaLike[A] {

  override final type S[a] = PlainTextSchema[a]

  override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A): PlainTextSchema[B] = PlainTextSchema.Transform(this, newTypeTag, ab, ba)
  override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A): PlainTextSchema[B] = PlainTextSchema.TransformOrFail(this, newTypeTag, ab, ba)

}
object PlainTextSchema extends PlainTextSchemaLowPriority.LowPriority1 {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given string: PlainTextSchema[String] = StringSchema
  given uuid: PlainTextSchema[UUID] = PlainTextSchema.string.transformAttempt(UUID.fromString, _.toString)
  given boolean: PlainTextSchema[Boolean] = PlainTextSchema.string.transformOption(_.toBooleanOption, _.toString)

  given byte: PlainTextSchema[Byte] = PlainTextSchema.string.transformOption(_.toByteOption, _.toString)
  given short: PlainTextSchema[Short] = PlainTextSchema.string.transformOption(_.toShortOption, _.toString)
  given int: PlainTextSchema[Int] = PlainTextSchema.string.transformOption(_.toIntOption, _.toString)
  given long: PlainTextSchema[Long] = PlainTextSchema.string.transformOption(_.toLongOption, _.toString)
  given bigInt: PlainTextSchema[BigInt] = PlainTextSchema.string.transformAttempt(BigInt(_), _.toString)

  given float: PlainTextSchema[Float] = PlainTextSchema.string.transformOption(_.toFloatOption, _.toString)
  given double: PlainTextSchema[Double] = PlainTextSchema.string.transformOption(_.toDoubleOption, _.toString)
  given bigDecimal: PlainTextSchema[BigDecimal] = PlainTextSchema.string.transformAttempt(BigDecimal(_), _.toString)

  given localDate: PlainTextSchema[LocalDate] = PlainTextSchema.string.transformAttempt(LocalDate.parse, _.toString)
  given localTime: PlainTextSchema[LocalTime] = PlainTextSchema.string.transformAttempt(LocalTime.parse, _.toString)
  given localDateTime: PlainTextSchema[LocalDateTime] = PlainTextSchema.string.transformAttempt(LocalDateTime.parse, _.toString)

  given duration: PlainTextSchema[Duration] = PlainTextSchema.fromStringCodec
  given period: PlainTextSchema[Period] = PlainTextSchema.string.transformAttempt(Period.parse, _.toString)
  given instant: PlainTextSchema[Instant] = PlainTextSchema.string.transformAttempt(Instant.parse, _.toString)
  given offsetDateTime: PlainTextSchema[OffsetDateTime] = PlainTextSchema.string.transformAttempt(OffsetDateTime.parse, _.toString)
  given zonedDateTime: PlainTextSchema[ZonedDateTime] = PlainTextSchema.string.transformAttempt(ZonedDateTime.parse, _.toString)
  given zoneId: PlainTextSchema[ZoneId] = PlainTextSchema.string.transformAttempt(ZoneId.of, _.toString)
  given zoneOffset: PlainTextSchema[ZoneOffset] = PlainTextSchema.string.transformAttempt(ZoneOffset.of, _.toString)
  given timeZone: PlainTextSchema[TimeZone] = PlainTextSchema.string.transformAttempt(TimeZone.getTimeZone, _.getID)

  given bearerToken: PlainTextSchema[BearerToken] = PlainTextSchema.BearerTokenSchema

  given standardJWT: [A: JsonSchema] => (jwtTypeTag: TypeTag[JWT.Std[A]], payloadTypeTag: TypeTag[JWT.StandardPayload[A]]) => PlainTextSchema[JWT.Std[A]] =
    PlainTextSchema.JWTSchema(jwtTypeTag, instances.standardJWTPayloadSchema[A])
  given password: PlainTextSchema[Password.PlainText] = PlainTextSchema.fromStringCodec

  def fromStringCodec[A: {StringCodec as codec, TypeTag}]: PlainTextSchema[A] =
    PlainTextSchema.string.transformOrFail(codec.decoder.decodeSimple, codec.encoder.encode)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object StringSchema extends PlainTextSchema[String] {
    override val typeTag: TypeTag[String] = TypeTag[String]
    override def decode(string: String): Either[String, String] = string.asRight
    override def encode(value: String): String = value
  }

  trait EnumSchema[A] extends PlainTextSchema[A] {
    val encodedValues: Seq[String]
  }

  final case class GenericEnumSchema[A](typeTag: TypeTag[A], values: Seq[A], encodeValue: A => String, caseSensitive: Boolean) extends EnumSchema[A] {

    override val encodedValues: Seq[String] = values.map(encodeValue)
    private val encodedValuesString: String = encodedValues.map(s => s"'$s'").mkString(", ")

    val valueMap: Map[String, A] = values.map { a => (if (caseSensitive) encodeValue(a) else encodeValue(a).toLowerCase, a) }.toMap

    override def decode(string: String): Either[String, A] =
      valueMap.get(if (caseSensitive) string else string.toLowerCase).toRight(s"Invalid '${typeTag.prefixObject}' ($string), expected one of: $encodedValuesString")

    override def encode(value: A): String =
      encodeValue(value)

  }

  final case class StrictEnumSchema[A](strictEnum: StrictEnum[A]) extends EnumSchema[A] {

    override val typeTag: TypeTag[A] = strictEnum.typeTag
    override val encodedValues: Seq[String] = strictEnum.encodedValues

    override def decode(string: String): Either[String, A] = strictEnum.decodeEitherWithHint(string)

    override def encode(value: A): String = strictEnum.encode(value)

  }

  final case class EnumWithOtherSchema[A](strictEnum: EnumWithOther[A]) extends EnumSchema[A] {

    override val typeTag: TypeTag[A] = strictEnum.typeTag
    override val encodedValues: Seq[String] = strictEnum.encodedValues

    override def decode(string: String): Either[String, A] = strictEnum.decode(string).asRight

    override def encode(value: A): String = strictEnum.encode(value)

  }

  case object BearerTokenSchema extends PlainTextSchema[BearerToken] {
    override val typeTag: TypeTag[BearerToken] = TypeTag[BearerToken]
    override def decode(string: String): Either[String, BearerToken] = BearerToken.decodeBearer(string)
    override def encode(value: BearerToken): String = value.bearer
  }

  final case class JWTSchema[A](typeTag: TypeTag[JWT[A]], payloadSchema: JsonSchema[A]) extends PlainTextSchema[JWT[A]] {
    override def decode(string: String): Either[String, JWT[A]] = JWT.decodeBearer[A](string)(using payloadSchema.jsonDecoder)
    override def encode(value: JWT[A]): String = value.token.bearer
  }

  final case class Transform[A, B](underlying: PlainTextSchema[A], typeTag: TypeTag[B], ab: A => B, ba: B => A) extends PlainTextSchema[B] {
    override def decode(string: String): Either[String, B] = underlying.decode(string).map(ab)
    override def encode(value: B): String = underlying.encode(ba(value))
  }

  final case class TransformOrFail[A, B](underlying: PlainTextSchema[A], typeTag: TypeTag[B], ab: A => Either[String, B], ba: B => A) extends PlainTextSchema[B] {
    override def decode(string: String): Either[String, B] = underlying.decode(string).flatMap(ab)
    override def encode(value: B): String = underlying.encode(ba(value))
  }

}

object PlainTextSchemaLowPriority {

  trait LowPriority1 {

    given jwt: [A: JsonSchema as payloadSchema] => (typeTag: TypeTag[JWT[A]]) => PlainTextSchema[JWT[A]] = PlainTextSchema.JWTSchema(typeTag, payloadSchema)

    given strictEnum: [A: StrictEnum as e] => PlainTextSchema[A] = PlainTextSchema.StrictEnumSchema(e)
    given enumWithOther: [A: EnumWithOther as e] => PlainTextSchema[A] = PlainTextSchema.EnumWithOtherSchema(e)

    def `enum`[A: StrictEnum]: PlainTextSchema[A] = strictEnum[A]

  }

}
