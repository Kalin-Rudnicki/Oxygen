package oxygen.schema

import java.time.*
import java.util.{TimeZone, UUID}
import oxygen.core.SourcePosition
import oxygen.json.*
import oxygen.json.generic.*
import oxygen.meta.{*, given}
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

  final def @@(encoding: PlainTextSchema.Encoding): PlainTextSchema[A] = PlainTextSchema.JsonEncoded(this) @@ encoding

}
object JsonSchema extends Derivable[JsonSchema.ProductLike], JsonSchemaLowPriority.LowPriority1 {

  def apply[A: JsonSchema as schema]: JsonSchema[A] = schema

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given json: JsonSchema[Json] = ASTSchema(TypeTag[Json], None, JsonEncoder.json, JsonDecoder.json)
  given jsonString: JsonSchema[Json.Str] = ASTSchema(TypeTag[Json.Str], Json.Type.String.some, JsonEncoder.json, JsonDecoder.jsonString)
  given jsonBoolean: JsonSchema[Json.Bool] = ASTSchema(TypeTag[Json.Bool], Json.Type.Boolean.some, JsonEncoder.json, JsonDecoder.jsonBoolean)
  given jsonNumber: JsonSchema[Json.Number] = ASTSchema(TypeTag[Json.Number], Json.Type.Number.some, JsonEncoder.json, JsonDecoder.jsonNumber)
  given jsonArray: JsonSchema[Json.Arr] = ASTSchema(TypeTag[Json.Arr], Json.Type.Array.some, JsonEncoder.json, JsonDecoder.jsonArray)
  given jsonObject: JsonSchema[Json.Obj] = ASTSchema(TypeTag[Json.Obj], Json.Type.Object.some, JsonEncoder.json, JsonDecoder.jsonObject)
  given jsonNull: JsonSchema[Json.Null.type] = ASTSchema(TypeTag[Json.Null.type], Json.Type.Null.some, JsonEncoder.json, JsonDecoder.jsonNull)

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

  given option: [A: JsonSchema as underlying] => JsonSchema[Option[A]] = OptionSchema(underlying)
  given specified: [A: JsonSchema as underlying] => JsonSchema[Specified[A]] = SpecifiedSchema(underlying)
  given arraySeq: [A: {JsonSchema as underlying, TypeTag}] => JsonSchema[ArraySeq[A]] =
    ArraySchema(underlying, TypeTag.derived, JsonEncoder.arraySeq[A](using underlying.jsonEncoder), JsonDecoder.arraySeq[A](using underlying.jsonDecoder))
  given map: [K: {PlainTextSchema as keySchema, TypeTag}, V: {JsonSchema as valueSchema, TypeTag}] => JsonSchema[Map[K, V]] =
    MapSchema(TypeTag.derived, keySchema, valueSchema)

  given period: JsonSchema[Period] = standardJavaTime.period
  given instant: JsonSchema[Instant] = standardJavaTime.instant
  given offsetDateTime: JsonSchema[OffsetDateTime] = standardJavaTime.offsetDateTime
  given zonedDateTime: JsonSchema[ZonedDateTime] = standardJavaTime.zonedDateTime
  given zoneId: JsonSchema[ZoneId] = standardJavaTime.zoneId
  given zoneOffset: JsonSchema[ZoneOffset] = standardJavaTime.zoneOffset
  given timeZone: JsonSchema[TimeZone] = standardJavaTime.timeZone

  given duration: JsonSchema[Duration] = standardJavaTime.duration
  given localDate: JsonSchema[LocalDate] = standardJavaTime.localDate
  given localTime: JsonSchema[LocalTime] = standardJavaTime.localTime
  given localDateTime: JsonSchema[LocalDateTime] = standardJavaTime.localDateTime

  object standardJavaTime {

    given period: JsonSchema[Period] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.period)
    given instant: JsonSchema[Instant] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.instant)
    given offsetDateTime: JsonSchema[OffsetDateTime] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.offsetDateTime)
    given zonedDateTime: JsonSchema[ZonedDateTime] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.zonedDateTime)
    given zoneId: JsonSchema[ZoneId] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.zoneId)
    given zoneOffset: JsonSchema[ZoneOffset] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.zoneOffset)
    given timeZone: JsonSchema[TimeZone] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.timeZone)

    given duration: JsonSchema[Duration] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.duration)
    given localDate: JsonSchema[LocalDate] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.localDate)
    given localTime: JsonSchema[LocalTime] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.localTime)
    given localDateTime: JsonSchema[LocalDateTime] = JsonSchema.fromPlainText(using PlainTextSchema.standardJavaTime.localDateTime)

  }

  object oxygenTime {

    given duration: JsonSchema[Duration] = JsonSchema.fromPlainText(using PlainTextSchema.oxygenTime.duration)
    given localDate: JsonSchema[LocalDate] = JsonSchema.fromPlainText(using PlainTextSchema.oxygenTime.localDate)
    given localTime: JsonSchema[LocalTime] = JsonSchema.fromPlainText(using PlainTextSchema.oxygenTime.localTime)
    given localDateTime: JsonSchema[LocalDateTime] = JsonSchema.fromPlainText(using PlainTextSchema.oxygenTime.localDateTime)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Instances
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// Non-Object ///////////////////////////////////////////////////////////////

  sealed trait NonProductLike[A] extends JsonSchema[A] {

    override final type S[a] = JsonSchema[a]

    override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A)(using pos: SourcePosition): JsonSchema[B] =
      JsonSchema.Transform(this, newTypeTag, ab, ba, pos)
    override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A)(using pos: SourcePosition): JsonSchema[B] =
      JsonSchema.TransformOrFail(this, newTypeTag, ab, ba, pos)

  }

  private[schema] final case class StringSchema[A](underlying: PlainTextSchema[A]) extends NonProductLike[A] {
    override val typeTag: TypeTag[A] = underlying.typeTag

    // this exactly matches the reference string of the corresponding PlainTextSchema so that there is no penalty or duplication from switching back and forth
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = builder.referenceOf(underlying)

    override val jsonEncoder: JsonEncoder[A] = JsonEncoder.string.contramap(underlying.encode)
    override val jsonDecoder: JsonDecoder[A] = JsonDecoder.string.mapOrFail(underlying.decode)
  }

  private[schema] case object BooleanSchema extends NonProductLike[Boolean] {
    override val typeTag: TypeTag[Boolean] = TypeTag[Boolean]
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("JsonBoolean")
    override val jsonEncoder: JsonEncoder[Boolean] = JsonEncoder.boolean
    override val jsonDecoder: JsonDecoder[Boolean] = JsonDecoder.boolean
  }

  private[schema] final case class ASTSchema[A] private[JsonSchema] (typeTag: TypeTag[A], specificType: Option[Json.Type], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A])
      extends NonProductLike[A] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("JsonAST", "jsonType" -> specificType.fold("<any>")(_.toString))
  }

  private[schema] final case class IntNumberSchema[A] private[JsonSchema] (typeTag: TypeTag[A], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A]) extends NonProductLike[A] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("JsonInt")
  }
  private[schema] final case class NumberSchema[A] private[JsonSchema] (typeTag: TypeTag[A], jsonEncoder: JsonEncoder[A], jsonDecoder: JsonDecoder[A]) extends NonProductLike[A] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("JsonNumber")
  }

  private[schema] final case class OptionSchema[A] private[JsonSchema] (
      underlying: JsonSchema[A],
  ) extends JsonSchema.NonProductLike[Option[A]] {

    override val typeTag: TypeTag[Option[A]] = {
      given TypeTag[A] = underlying.typeTag
      TypeTag.derived
    }

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = s"JsonOption(${builder.referenceOf(underlying)})"

    override val jsonEncoder: JsonEncoder[Option[A]] = JsonEncoder.option(using underlying.jsonEncoder)
    override val jsonDecoder: JsonDecoder[Option[A]] = JsonDecoder.option(using underlying.jsonDecoder)

  }

  private[schema] final case class SpecifiedSchema[A] private[JsonSchema] (
      underlying: JsonSchema[A],
  ) extends JsonSchema.NonProductLike[Specified[A]] {

    override val typeTag: TypeTag[Specified[A]] = {
      given TypeTag[A] = underlying.typeTag
      TypeTag.derived
    }

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = s"JsonSpecified(${builder.referenceOf(underlying)})"

    override val jsonEncoder: JsonEncoder[Specified[A]] = JsonEncoder.specified(using underlying.jsonEncoder)
    override val jsonDecoder: JsonDecoder[Specified[A]] = JsonDecoder.specified(using underlying.jsonDecoder)

  }

  private[schema] final case class ArraySchema[A] private[schema] (
      underlying: JsonSchema[?],
      typeTag: TypeTag[A],
      jsonEncoder: JsonEncoder[A],
      jsonDecoder: JsonDecoder[A],
  ) extends NonProductLike[A] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = withHeader("JsonArray", "elemType" -> builder.referenceOf(underlying))
  }

  private[schema] final case class MapSchema[K, V] private[schema] (
      typeTag: TypeTag[Map[K, V]],
      keySchema: PlainTextSchema[K],
      valueSchema: JsonSchema[V],
  ) extends NonProductLike[Map[K, V]] {

    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("JsonMap", "keyType" -> builder.referenceOf(keySchema), "valueType" -> builder.referenceOf(valueSchema))

    private val jsonFieldEncoder: JsonFieldEncoder[K] = JsonFieldEncoder.string.contramap(keySchema.encode)
    private val jsonFieldDecoder: JsonFieldDecoder[K] = JsonFieldDecoder.string.mapOrFail(keySchema.decode)

    override val jsonEncoder: JsonEncoder[Map[K, V]] = JsonEncoder.map[K, V](using jsonFieldEncoder, valueSchema.jsonEncoder)
    override val jsonDecoder: JsonDecoder[Map[K, V]] = JsonDecoder.map[K, V](using jsonFieldDecoder, valueSchema.jsonDecoder)

  }

  private[schema] final case class Transform[A, B] private[JsonSchema] (
      underlying: JsonSchema[A],
      typeTag: TypeTag[B],
      ab: A => B,
      ba: B => A,
      pos: SourcePosition,
  ) extends JsonSchema.NonProductLike[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("Json.Transform", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
    override val jsonEncoder: JsonEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder[B] = underlying.jsonDecoder.map(ab)
  }

  private[schema] final case class TransformOrFail[A, B] private[JsonSchema] (
      underlying: JsonSchema[A],
      typeTag: TypeTag[B],
      ab: A => Either[String, B],
      ba: B => A,
      pos: SourcePosition,
  ) extends JsonSchema.NonProductLike[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("Json.TransformOrFail", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
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

    override final def transform[B: TypeTag as newTypeTag](ab: A => B, ba: B => A)(using pos: SourcePosition): JsonSchema.ProductLike[B] =
      JsonSchema.TransformProduct(this, newTypeTag, ab, ba, pos)
    override final def transformOrFail[B: TypeTag as newTypeTag](ab: A => Either[String, B], ba: B => A)(using pos: SourcePosition): JsonSchema.ProductLike[B] =
      JsonSchema.TransformOrFailProduct(this, newTypeTag, ab, ba, pos)

  }
  object ProductLike {

    def apply[A: JsonSchema.ProductLike as schema]: JsonSchema.ProductLike[A] = schema

    inline def derived[A]: JsonSchema.ProductLike[A] = JsonSchema.derived[A]

  }

  private[schema] trait ProductSchema[A] extends ProductLike[A] {
    lazy val fields: ArraySeq[ProductField[?]]
    override protected final def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("JsonProduct", "fields" -> fields.map { f => s"${f.name}=${builder.referenceOf(f.schema)}" }.mkString("{", ",", "}"))
  }

  private[schema] trait SumSchema[A] extends ProductLike[A] {
    val discriminator: Option[String]
    lazy val children: ArraySeq[SumCase[? <: A]]
    override protected final def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String = discriminator match
      case Some(discriminator) => withHeader("JsonSum", "discriminator" -> discriminator, "children" -> children.map { c => s"${c.name}=${builder.referenceOf(c.schema)}" }.mkString("{", ",", "}"))
      case None                => withHeader("JsonSum", "children" -> children.map { c => s"${c.name}=${builder.referenceOf(c.schema)}" }.mkString("{", ",", "}"))
  }

  private[schema] final case class TransformProduct[A, B] private[JsonSchema] (
      underlying: ProductLike[A],
      typeTag: TypeTag[B],
      ab: A => B,
      ba: B => A,
      pos: SourcePosition,
  ) extends JsonSchema.ProductLike[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("Json.Transform", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
    override val jsonEncoder: JsonEncoder.ObjectEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder.ObjectDecoder[B] = underlying.jsonDecoder.map(ab)
  }

  private[schema] final case class TransformOrFailProduct[A, B] private[JsonSchema] (
      underlying: ProductLike[A],
      typeTag: TypeTag[B],
      ab: A => Either[String, B],
      ba: B => A,
      pos: SourcePosition,
  ) extends JsonSchema.ProductLike[B] {
    override protected def __internalReferenceOf(builder: SchemaLike.ReferenceBuilder): String =
      withHeader("Json.TransformOrFail", "pos" -> pos.toString, "underlying" -> builder.referenceOf(underlying))
    override val jsonEncoder: JsonEncoder.ObjectEncoder[B] = underlying.jsonEncoder.contramap(ba)
    override val jsonDecoder: JsonDecoder.ObjectDecoder[B] = underlying.jsonDecoder.mapOrFail(ab)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Generic
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def productDeriver[A](using Quotes, Type[ProductLike], Type[A], ProductGeneric[A], Derivable[ProductLike]): Derivable.ProductDeriver[ProductLike, A] =
    Derivable.ProductDeriver.withDisjointInstances[JsonSchema.ProductLike, JsonSchema, A] { instances =>
      new Derivable.ProductDeriver[JsonSchema.ProductLike, A] {

        private val typeTagExpr: Expr[TypeTag[A]] = Implicits.companion.searchRequiredIgnoreExplanation[TypeTag[A]]

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

        private val typeTagExpr: Expr[TypeTag[A]] = Implicits.companion.searchRequiredIgnoreExplanation[TypeTag[A]]

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

  private def deriveWrappedImpl[A: Type](using Quotes): Expr[JsonSchema[A]] = {
    type B
    val wrapping = K0.ProductGeneric.extractSingleCaseClassField[A, B]
    given Type[B] = wrapping.field.tpe

    '{ ${ wrapping.field.summonTypeClass[JsonSchema] }.transform[A](${ wrapping.wrapExpr }, ${ wrapping.unwrapExpr }) }
  }

  /**
    * Expects [[A]] to be a case class with a single field.
    * Will then derive an instance
    *
    * Example:
    * ```scala
    * final case class Wrapped(value: String)
    * object Wrapped {
    *   given JsonSchema[Wrapped] = JsonSchema.deriveWrapped
    * }
    * ```
    */
  inline def deriveWrapped[A]: JsonSchema[A] = ${ deriveWrappedImpl[A] }

}

object JsonSchemaLowPriority {

  trait LowPriority1 {

    given fromPlainText: [A: PlainTextSchema as underlying] => JsonSchema[A] = JsonSchema.StringSchema(underlying)

    given seq: [S[_]: {SeqOps as seqOps, TypeTag}, A: {JsonSchema as underlying, ClassTag as ct, TypeTag}] => JsonSchema[S[A]] =
      JsonSchema.ArraySchema(underlying, TypeTag.derived, JsonEncoder.seq[S, A](using seqOps, underlying.jsonEncoder, ct), JsonDecoder.seq[S, A](using seqOps, underlying.jsonDecoder))

    given nonEmptyList: [A: {JsonSchema, ClassTag, TypeTag}] => JsonSchema[NonEmptyList[A]] =
      seq[List, A].transformOrFail[NonEmptyList[A]](
        NonEmptyList.fromList(_).toRight("Array can not be empty"),
        _.toList,
      )

  }

}
