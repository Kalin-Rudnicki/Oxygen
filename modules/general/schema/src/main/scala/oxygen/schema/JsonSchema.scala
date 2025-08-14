package oxygen.schema

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.json.*
import oxygen.json.generic.*
import oxygen.meta.{given, *}
import oxygen.meta.K0.*
import oxygen.predef.core.*
import oxygen.quoted.*
import scala.quoted.*
import scala.reflect.ClassTag

sealed trait JsonSchema[A] extends SchemaLike[A] {

  override type S[a] <: JsonSchema[a]

  val jsonEncoder: JsonEncoder[A]
  val jsonDecoder: JsonDecoder[A]
  final def jsonCodec: JsonCodec[A] = JsonCodec(jsonEncoder, jsonDecoder)

  override final def decode(string: String): Either[String, A] = jsonDecoder.decodeJsonString(string).leftMap(_.getMessage)

  override final def encode(value: A): String = jsonEncoder.encodeJsonStringCompact(value)

}
object JsonSchema extends Derivable[JsonSchema.ProductLike], JsonSchemaLowPriority.LowPriority1 {

  def apply[A: JsonSchema as schema]: JsonSchema[A] = schema

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given json: JsonSchema[Json] = ASTSchema(TypeTag[Json], JsonEncoder.json, JsonDecoder.json)
  given jsonString: JsonSchema[Json.Str] = ASTSchema(TypeTag[Json.Str], JsonEncoder.json, JsonDecoder.jsonString)
  given jsonBoolean: JsonSchema[Json.Bool] = ASTSchema(TypeTag[Json.Bool], JsonEncoder.json, JsonDecoder.jsonBoolean)
  given jsonNumber: JsonSchema[Json.Number] = ASTSchema(TypeTag[Json.Number], JsonEncoder.json, JsonDecoder.jsonNumber)
  given jsonArray: JsonSchema[Json.Arr] = ASTSchema(TypeTag[Json.Arr], JsonEncoder.json, JsonDecoder.jsonArray)
  given jsonObject: JsonSchema[Json.Obj] = ASTSchema(TypeTag[Json.Obj], JsonEncoder.json, JsonDecoder.jsonObject)
  given jsonNull: JsonSchema[Json.Null.type] = ASTSchema(TypeTag[Json.Null.type], JsonEncoder.json, JsonDecoder.jsonNull)

  given string: JsonSchema[String] = fromPlainText
  given boolean: JsonSchema[Boolean] = BooleanSchema
  given uuid: JsonSchema[UUID] = fromPlainText

  given byte: JsonSchema[Byte] = IntNumberSchema(TypeTag[Byte], JsonEncoder.byte, JsonDecoder.byte)
  given short: JsonSchema[Short] = IntNumberSchema(TypeTag[Short], JsonEncoder.short, JsonDecoder.short)
  given int: JsonSchema[Int] = IntNumberSchema(TypeTag[Int], JsonEncoder.int, JsonDecoder.int)
  given long: JsonSchema[Long] = IntNumberSchema(TypeTag[Long], JsonEncoder.long, JsonDecoder.long)
  given bigInt: JsonSchema[BigInt] = IntNumberSchema(TypeTag[BigInt], JsonEncoder.bigInt, JsonDecoder.bigInt)

  given float: JsonSchema[Float] = NumberSchema(TypeTag[Float], JsonEncoder.float, JsonDecoder.float)
  given double: JsonSchema[Double] = NumberSchema(TypeTag[Double], JsonEncoder.double, JsonDecoder.double)
  given bigDecimal: JsonSchema[BigDecimal] = NumberSchema(TypeTag[BigDecimal], JsonEncoder.bigDecimal, JsonDecoder.bigDecimal)

  given option: [A: JsonSchema as underlying] => (newTypeTag: TypeTag[Option[A]]) => JsonSchema[Option[A]] =
    OptionalSchema(underlying, newTypeTag, JsonEncoder.option(using underlying.jsonEncoder), JsonDecoder.option(using underlying.jsonDecoder))
  given specified: [A: JsonSchema as underlying] => (newTypeTag: TypeTag[Specified[A]]) => JsonSchema[Specified[A]] =
    OptionalSchema(underlying, newTypeTag, JsonEncoder.specified(using underlying.jsonEncoder), JsonDecoder.specified(using underlying.jsonDecoder))

  given localDate: JsonSchema[LocalDate] = fromPlainText
  given localTime: JsonSchema[LocalTime] = fromPlainText
  given localDateTime: JsonSchema[LocalDateTime] = fromPlainText

  given instant: JsonSchema[Instant] = fromPlainText
  given offsetDateTime: JsonSchema[OffsetDateTime] = fromPlainText
  given zonedDateTime: JsonSchema[ZonedDateTime] = fromPlainText
  given zoneId: JsonSchema[ZoneId] = fromPlainText
  given zoneOffset: JsonSchema[ZoneOffset] = fromPlainText
  given timeZone: JsonSchema[TimeZone] = fromPlainText

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Non-Object ///////////////////////////////////////////////////////////////

  sealed trait NonProductLike[A] extends JsonSchema[A] {

    override final type S[a] = JsonSchema[a]

    override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A): JsonSchema[B] = JsonSchema.Transform(this, newTypeTag, ab, ba)
    override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A): JsonSchema[B] = JsonSchema.TransformOrFail(this, newTypeTag, ab, ba)

  }

  final case class StringSchema[A](underlying: PlainTextSchema[A]) extends NonProductLike[A] {
    override val typeTag: TypeTag[A] = underlying.typeTag
    override val jsonEncoder: JsonEncoder[A] = JsonEncoder.string.contramap(underlying.encode)
    override val jsonDecoder: JsonDecoder[A] = JsonDecoder.string.mapOrFail(underlying.decode)
  }

  case object BooleanSchema extends NonProductLike[Boolean] {
    override val typeTag: TypeTag[Boolean] = TypeTag[Boolean]
    override val jsonEncoder: JsonEncoder[Boolean] = JsonEncoder.boolean
    override val jsonDecoder: JsonDecoder[Boolean] = JsonDecoder.boolean
  }

  final case class ASTSchema[A] private[JsonSchema] (typeTag: TypeTag[A], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A]) extends NonProductLike[A]

  final case class IntNumberSchema[A] private[JsonSchema] (typeTag: TypeTag[A], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A]) extends NonProductLike[A]
  final case class NumberSchema[A] private[JsonSchema] (typeTag: TypeTag[A], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A]) extends NonProductLike[A]

  final case class OptionalSchema[A] private[JsonSchema] (
      underlying: JsonSchema[?],
      typeTag: TypeTag[A],
      jsonEncoder: JsonEncoder[A],
      jsonDecoder: JsonDecoder[A],
      // TODO (KR) : additional information about null/missing
  ) extends JsonSchema.NonProductLike[A]

  final case class ArraySchema[A] private[JsonSchema] (
      underlying: JsonSchema[?],
      typeTag: TypeTag[A],
      jsonEncoder: JsonEncoder[A],
      jsonDecoder: JsonDecoder[A],
  ) extends NonProductLike[A]

  final case class Transform[A, B] private[JsonSchema] (underlying: JsonSchema[A], typeTag: TypeTag[B], ab: A => B, ba: B => A) extends JsonSchema.NonProductLike[B] {
    override val jsonEncoder: JsonEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder[B] = underlying.jsonDecoder.map(ab)
  }

  final case class TransformOrFail[A, B] private[JsonSchema] (underlying: JsonSchema[A], typeTag: TypeTag[B], ab: A => Either[String, B], ba: B => A) extends JsonSchema.NonProductLike[B] {
    override val jsonEncoder: JsonEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder[B] = underlying.jsonDecoder.mapOrFail(ab)
  }

  /////// Object ///////////////////////////////////////////////////////////////

  final case class ProductField[A](
      name: String,
      schema: JsonSchema[A],
  )

  final case class SumCase[A](
      name: String,
      schema: JsonSchema[A],
  )

  sealed trait ProductLike[A] extends JsonSchema[A] {

    override final type S[a] = JsonSchema.ProductLike[a]

    override val jsonEncoder: JsonEncoder.ObjectEncoder[A]
    override val jsonDecoder: JsonDecoder.ObjectDecoder[A]

    override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A): JsonSchema.ProductLike[B] = JsonSchema.TransformProduct(this, newTypeTag, ab, ba)
    override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A): JsonSchema.ProductLike[B] = JsonSchema.TransformOrFailProduct(this, newTypeTag, ab, ba)

  }
  object ProductLike {

    def apply[A: JsonSchema.ProductLike as schema]: JsonSchema.ProductLike[A] = schema

    inline def derived[A]: JsonSchema.ProductLike[A] = JsonSchema.derived[A]

  }

  trait ProductSchema[A] extends ProductLike[A] {
    lazy val fields: ArraySeq[ProductField[?]]
  }

  trait SumSchema[A] extends ProductLike[A] {
    val discriminator: Option[String]
    lazy val children: ArraySeq[SumCase[? <: A]]
  }

  final case class TransformProduct[A, B] private[JsonSchema] (underlying: ProductLike[A], typeTag: TypeTag[B], ab: A => B, ba: B => A) extends JsonSchema.ProductLike[B] {
    override val jsonEncoder: JsonEncoder.ObjectEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder.ObjectDecoder[B] = underlying.jsonDecoder.map(ab)
  }

  final case class TransformOrFailProduct[A, B] private[JsonSchema] (underlying: ProductLike[A], typeTag: TypeTag[B], ab: A => Either[String, B], ba: B => A) extends JsonSchema.ProductLike[B] {
    override val jsonEncoder: JsonEncoder.ObjectEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder.ObjectDecoder[B] = underlying.jsonDecoder.mapOrFail(ab)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[ProductLike], Type[A], ProductGeneric[A], Derivable[ProductLike]): Derivable.ProductDeriver[ProductLike, A] =
    Derivable.ProductDeriver.withDisjointInstances[JsonSchema.ProductLike, JsonSchema, A] { instances =>
      new Derivable.ProductDeriver[JsonSchema.ProductLike, A] {

        private val typeTagExpr: Expr[TypeTag[A]] = Implicits.companion.searchRequiredIgnoreMessage[TypeTag[A]]

        private val jsonEncoderInstances: Expressions[JsonEncoder, A] =
          instances.mapK { [a] => _ ?=> (expr: Expr[JsonSchema[a]]) => '{ $expr.jsonEncoder } }

        private val jsonDecoderInstances: Expressions[JsonDecoder, A] =
          instances.mapK { [a] => _ ?=> (expr: Expr[JsonSchema[a]]) => '{ $expr.jsonDecoder } }

        private val derivedJsonEncoder: Expr[JsonEncoder.ObjectEncoder[A]] = DeriveProductJsonEncoder[A](jsonEncoderInstances).derive
        private val derivedJsonDecoder: Expr[JsonDecoder.ObjectDecoder[A]] = DeriveProductJsonDecoder[A](jsonDecoderInstances).derive

        private val fieldsExpr: Expr[ArraySeq[ProductField[?]]] =
          generic.mapChildren
            .mapExpr[ProductField[?]] { [a] => (_, _) ?=> (field: generic.Field[a]) =>
              '{
                ProductField[a](
                  name = ${ Expr(field.annotations.optionalOfValue[jsonField].fold(field.name)(_.name)) },
                  schema = ${ field.getExpr(instances) },
                )
              }
            }
            .seqToArraySeqExpr

        override def derive: Expr[ProductSchema[A]] =
          '{
            new ProductSchema[A] {
              override val typeTag: TypeTag[A] = $typeTagExpr
              override val jsonEncoder: JsonEncoder.ObjectEncoder[A] = $derivedJsonEncoder
              override val jsonDecoder: JsonDecoder.ObjectDecoder[A] = $derivedJsonDecoder
              override lazy val fields: ArraySeq[ProductField[?]] = $fieldsExpr
            }
          }

      }
    }

  override protected def sumDeriver[A](using Quotes, Type[ProductLike], Type[A], SumGeneric[A], Derivable[ProductLike]): Derivable.SumDeriver[ProductLike, A] =
    Derivable.SumDeriver.withInstances[JsonSchema.ProductLike, A] { instances =>
      new Derivable.SumDeriver[JsonSchema.ProductLike, A] {

        private val typeTagExpr: Expr[TypeTag[A]] = Implicits.companion.searchRequiredIgnoreMessage[TypeTag[A]]

        private val jsonEncoderInstances: Expressions[JsonEncoder.ObjectEncoder, A] =
          instances.mapK { [a] => _ ?=> (expr: Expr[JsonSchema.ProductLike[a]]) => '{ $expr.jsonEncoder } }

        private val jsonDecoderInstances: Expressions[JsonDecoder.ObjectDecoder, A] =
          instances.mapK { [a] => _ ?=> (expr: Expr[JsonSchema.ProductLike[a]]) => '{ $expr.jsonDecoder } }

        private val derivedJsonEncoder: Expr[JsonEncoder.ObjectEncoder[A]] = DeriveSumJsonEncoder[A](jsonEncoderInstances).derive
        private val derivedJsonDecoder: Expr[JsonDecoder.ObjectDecoder[A]] = DeriveSumJsonDecoder[A](jsonDecoderInstances).derive

        private val childrenExpr: Expr[ArraySeq[SumCase[? <: A]]] =
          generic.mapChildren
            .mapExpr[SumCase[? <: A]] { [a <: A] => (_, _) ?=> (kase: generic.Case[a]) =>
              '{
                SumCase[a](
                  name = ${ Expr(kase.annotations.optionalOfValue[jsonType].fold(kase.name)(_.name)) },
                  schema = ${ kase.getExpr(instances) },
                )
              }
            }
            .seqToArraySeqExpr

        override def derive: Expr[SumSchema[A]] =
          '{
            new SumSchema[A] {
              override val typeTag: TypeTag[A] = $typeTagExpr
              override val jsonEncoder: JsonEncoder.ObjectEncoder[A] = $derivedJsonEncoder
              override val jsonDecoder: JsonDecoder.ObjectDecoder[A] = $derivedJsonDecoder
              override val discriminator: Option[String] = ${ Expr(generic.annotations.optionalOfValue[jsonDiscriminator].map(_.name)) }
              override lazy val children: ArraySeq[SumCase[? <: A]] = $childrenExpr
            }
          }

      }
    }

  override inline def derived[A]: ProductLike[A] = ${ derivedImpl[A] }

}

object JsonSchemaLowPriority {

  trait LowPriority1 {

    given fromPlainText: [A: PlainTextSchema as underlying] => JsonSchema[A] = JsonSchema.StringSchema(underlying)

  }

}
