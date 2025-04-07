package oxygen.schema

import java.time.*
import java.util.UUID
import oxygen.predef.core.*
import scala.util.Try
import scala.util.matching.Regex

sealed trait JsonSchema[A] extends SchemaLike[JsonSchema, A] {

  val jsonTypes: Set[Json.Type]

  val toJson: A => Json
  val fromJson: (List[JsonError.Path], Json) => Either[JsonError, A]

  val includeInObject: A => Boolean
  val onMissing: Option[A]

  override final val encode: A => String = toJson(_).showSimple

  override final val decode: String => Either[String, A] = Json.parse(_).flatMap(fromJson(Nil, _)).leftMap(_.getMessage)

  override final def transform[B](ab: A => B, ba: B => A): JsonSchema[B] =
    JsonSchema.Transform(s"transform:$name", this, ab, ba)

  override final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): JsonSchema[B] =
    JsonSchema.TransformOrFail(s"transformOrFail:$name", this, ab, ba)

}
object JsonSchema {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Cases
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait RequiredSchema[A] extends JsonSchema[A] {
    override final val includeInObject: A => Boolean = (_: A) => true
    override final val onMissing: Option[A] = None
  }

  final case class StringSchema[A](plainTextSchema: PlainTextSchema[A]) extends JsonSchema.RequiredSchema[A] {

    override val name: String = plainTextSchema.name

    override val jsonTypes: Set[Json.Type] = Set(Json.Type.String)

    override val toJson: A => Json =
      value => Json.string(plainTextSchema.encode(value))

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, A] = {
      case (rPath, Json.Str(value)) => plainTextSchema.decode(value).leftMap(e => JsonError(rPath, JsonError.Cause.DecodingFailed(e)))
      case (rPath, json)            => JsonError(rPath, JsonError.Cause.InvalidType(Json.Type.String, json.tpe)).asLeft
    }

  }

  case object BigIntSchema extends JsonSchema.RequiredSchema[BigInt] {

    override val name: String = "BigInt"

    override val jsonTypes: Set[Json.Type] = Set(Json.Type.Number)

    override val toJson: BigInt => Json = Json.number

    private object stringToBigInt {
      def unapply(string: String): Option[BigInt] = string match
        case regularExpressions.integer(string) => Try { BigInt(string) }.toOption
        case _                                  => None
    }

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, BigInt] = {
      case (_, Json.NumberWithoutDecimal(value)) => value.asRight
      case (_, Json.Str(stringToBigInt(value)))  => value.asRight
      case (rPath, Json.NumberWithDecimal(_))    => JsonError(rPath, JsonError.Cause.DecodingFailed("decimal not allowed")).asLeft
      case (rPath, json)                         => JsonError(rPath, JsonError.Cause.InvalidType(Json.Type.Number, json.tpe)).asLeft
    }

    private[JsonSchema] def narrow[A](name: String, min: A, max: A, ab: BigInt => A, ba: A => BigInt): JsonSchema[A] = {
      val _min = ba(min)
      val _max = ba(max)

      TransformOrFail(
        name,
        this,
        a =>
          if (a > _max || a < _min) "Number out of range".asLeft
          else ab(a).asRight,
        ba,
      )
    }

  }

  case object BigDecimalSchema extends JsonSchema.RequiredSchema[BigDecimal] {

    override val name: String = "BigDecimal"

    override val jsonTypes: Set[Json.Type] = Set(Json.Type.Number)

    override val toJson: BigDecimal => Json = Json.number

    private object stringToBigDecimal {
      def unapply(string: String): Option[BigDecimal] = string match
        case regularExpressions.integer(string) => Try { BigDecimal(string) }.toOption
        case regularExpressions.decimal(string) => Try { BigDecimal(string) }.toOption
        case _                                  => None
    }

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, BigDecimal] = {
      case (_, Json.Number(value))                  => value.asRight
      case (_, Json.Str(stringToBigDecimal(value))) => value.asRight
      case (rPath, json)                            => JsonError(rPath, JsonError.Cause.InvalidType(Json.Type.Number, json.tpe)).asLeft
    }

    private[JsonSchema] def narrow[A](name: String, min: A, max: A, ab: BigDecimal => A, ba: A => BigDecimal): JsonSchema[A] = {
      val _min = ba(min)
      val _max = ba(max)

      TransformOrFail(
        name,
        this,
        a =>
          if (a > _max || a < _min) "Number out of range".asLeft
          else ab(a).asRight,
        ba,
      )
    }

  }

  case object BooleanSchema extends JsonSchema.RequiredSchema[Boolean] {

    override val name: String = "Boolean"

    override val jsonTypes: Set[Json.Type] = Set(Json.Type.Boolean)

    override val toJson: Boolean => Json = Json.boolean

    private object stringToBoolean {
      def unapply(string: String): Option[Boolean] = string.toBooleanOption
    }

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, Boolean] = {
      case (_, Json.Bool(value))                 => value.asRight
      case (_, Json.Str(stringToBoolean(value))) => value.asRight
      case (rPath, json)                         => JsonError(rPath, JsonError.Cause.InvalidType(Json.Type.Boolean, json.tpe)).asLeft
    }

  }

  final case class OptionalSchema[A](inner: JsonSchema[A]) extends JsonSchema[Option[A]] {

    override val name: String = s"Option[${inner.name}]"

    override val jsonTypes: Set[Json.Type] = inner.jsonTypes + Json.Type.Null

    override val toJson: Option[A] => Json = {
      case Some(value) => inner.toJson(value)
      case None        => Json.Null
    }

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, Option[A]] = {
      case (_, Json.Null) => None.asRight
      case (rPath, json)  => inner.fromJson(rPath, json).map(_.some)
    }

    override val includeInObject: Option[A] => Boolean = _.nonEmpty

    override val onMissing: Option[Option[A]] = None.some

  }

  final case class SpecifiedSchema[A](inner: JsonSchema[A]) extends JsonSchema[Specified[A]] {

    override val name: String = s"Specified[${inner.name}]"

    override val jsonTypes: Set[Json.Type] = inner.jsonTypes

    override val toJson: Specified[A] => Json = {
      case Specified.WasSpecified(value) => inner.toJson(value)
      case Specified.WasNotSpecified     => Json.Null
    }

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, Specified[A]] =
      inner.fromJson(_, _).map(Specified.WasSpecified(_))

    override val includeInObject: Specified[A] => Boolean = _.toOption.nonEmpty

    override val onMissing: Option[Specified[A]] = Specified.WasNotSpecified.some

  }

  final case class SeqSchema[A](elem: JsonSchema[A]) extends JsonSchema.RequiredSchema[Seq[A]] {
    override val name: String = s"Seq[${elem.name}]"
    override val jsonTypes: Set[Json.Type] = Set(Json.Type.Array)
    override val toJson: Seq[A] => Json =
      value => Json.arr(value.map(elem.toJson)*)
    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, Seq[A]] = {
      case (rPath, Json.Arr(value)) =>
        value.zipWithIndex.toSeq.traverse { case (json, idx) => elem.fromJson(JsonError.Path.Index(idx) :: rPath, json) }
      case (rPath, json) =>
        JsonError(rPath, JsonError.Cause.InvalidType(Json.Type.Array, json.tpe)).asLeft
    }
  }

  sealed trait ProductSchema[A] extends JsonSchema.RequiredSchema[A] {

    override final val jsonTypes: Set[Json.Type] = Set(Json.Type.Object)

    val fields: IArray[ProductSchema.Field]

  }
  object ProductSchema {

    final case class Field(
        name: String,
        schema: JsonSchema[?],
    )

  }

  final case class Transform[A, B](
      name: String,
      a: JsonSchema[A],
      ab: A => B,
      ba: B => A,
  ) extends JsonSchema[B] {

    override val jsonTypes: Set[Json.Type] = a.jsonTypes

    override val toJson: B => Json =
      value => a.toJson(ba(value))

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, B] =
      a.fromJson(_, _).map(ab)

    override val onMissing: Option[B] = a.onMissing.map(ab)

    override val includeInObject: B => Boolean = value => a.includeInObject(ba(value))

  }

  final case class TransformOrFail[A, B](
      name: String,
      a: JsonSchema[A],
      ab: A => Either[String, B],
      ba: B => A,
  ) extends JsonSchema[B] {

    override val jsonTypes: Set[Json.Type] = a.jsonTypes

    override val toJson: B => Json =
      value => a.toJson(ba(value))

    override val fromJson: (List[JsonError.Path], Json) => Either[JsonError, B] =
      (rPath, json) => a.fromJson(rPath, json).flatMap(ab(_).leftMap(e => JsonError(rPath, JsonError.Cause.DecodingFailed(e))))

    override val onMissing: Option[B] = a.onMissing.flatMap(ab(_).toOption)

    override val includeInObject: B => Boolean = value => a.includeInObject(ba(value))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // =====| From Plain Text |=====

  def fromPlainText[A: {PlainTextSchema as plainTextSchema}]: JsonSchema[A] =
    JsonSchema.StringSchema(plainTextSchema)

  given string: JsonSchema[String] = fromPlainText
  given uuid: JsonSchema[UUID] = fromPlainText

  given instant: JsonSchema[Instant] = fromPlainText
  given duration: JsonSchema[Duration] = fromPlainText
  given period: JsonSchema[Period] = fromPlainText
  given localDate: JsonSchema[LocalDate] = fromPlainText
  given localTime: JsonSchema[LocalTime] = fromPlainText
  given localDateTime: JsonSchema[LocalDateTime] = fromPlainText
  given zonedDateTime: JsonSchema[ZonedDateTime] = fromPlainText
  given offsetDateTime: JsonSchema[OffsetDateTime] = fromPlainText
  given dayOfWeek: JsonSchema[DayOfWeek] = fromPlainText
  given month: JsonSchema[Month] = fromPlainText
  given zoneId: JsonSchema[ZoneId] = fromPlainText
  given zoneOffset: JsonSchema[ZoneOffset] = fromPlainText

  // =====| Numbers |=====

  given byte: JsonSchema[Byte] = BigIntSchema.narrow("Byte", Byte.MinValue, Byte.MaxValue, _.toByte, BigInt(_))
  given short: JsonSchema[Short] = BigIntSchema.narrow("Short", Short.MinValue, Short.MaxValue, _.toShort, BigInt(_))
  given int: JsonSchema[Int] = BigIntSchema.narrow("Int", Int.MinValue, Int.MaxValue, _.toInt, BigInt(_))
  given long: JsonSchema[Long] = BigIntSchema.narrow("Long", Long.MinValue, Long.MaxValue, _.toLong, BigInt(_))
  given bigInt: JsonSchema[BigInt] = BigIntSchema

  given float: JsonSchema[Float] = BigDecimalSchema.narrow("Float", Float.MinValue, Float.MaxValue, _.toFloat, BigDecimal(_))
  given double: JsonSchema[Double] = BigDecimalSchema.narrow("Double", Double.MinValue, Double.MaxValue, _.toDouble, BigDecimal(_))
  given bigDecimal: JsonSchema[BigDecimal] = BigDecimalSchema

  // =====| Other |=====

  given boolean: JsonSchema[Boolean] = BooleanSchema

  given option: [A: {JsonSchema as inner}] => JsonSchema[Option[A]] = JsonSchema.OptionalSchema(inner)
  given specified: [A: {JsonSchema as inner}] => JsonSchema[Specified[A]] = JsonSchema.SpecifiedSchema(inner)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object regularExpressions {
    val integer: Regex = "(-?\\d+)".r
    val decimal: Regex = "(-?\\d+\\.\\d+)".r
  }

}
