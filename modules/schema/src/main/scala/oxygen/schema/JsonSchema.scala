package oxygen.schema

import oxygen.meta.*
import oxygen.schema.derivation.DeriveProductSchema
import scala.quoted.*
import zio.Chunk
import zio.json.JsonCodec

sealed trait JsonSchema[A] extends SchemaLike[JsonSchema, A] {

  val jsonTypes: Set[JsonSchema.JsonType]

  val jsonCodec: JsonCodec[A]

  override final def transform[B](ab: A => B, ba: B => A): JsonSchema[B] = JsonSchema.Transform(name, this, ab, ba)
  override final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): JsonSchema[B] = JsonSchema.TransformOrFail(name, this, ab, ba)

  override final val encode: A => String = jsonCodec.encodeJson(_, None).toString
  override final val decode: String => Either[String, A] = jsonCodec.decodeJson(_)
  // TODO (KR) :

}
object JsonSchema extends K0.Derivable[JsonSchema] {

  inline def apply[A](using inst: JsonSchema[A]): JsonSchema[A] = inst

  enum JsonType { case String, Number, Boolean, Array, Object, Null }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Implementations
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Transform[A, B](
      name: String,
      a: JsonSchema[A],
      ab: A => B,
      ba: B => A,
  ) extends JsonSchema[B] {
    override val jsonTypes: Set[JsonType] = a.jsonTypes
    override val jsonCodec: JsonCodec[B] = a.jsonCodec.transform(ab, ba)
  }

  final case class TransformOrFail[A, B](
      name: String,
      a: JsonSchema[A],
      ab: A => Either[String, B],
      ba: B => A,
  ) extends JsonSchema[B] {
    override val jsonTypes: Set[JsonType] = a.jsonTypes
    override val jsonCodec: JsonCodec[B] = a.jsonCodec.transformOrFail(ab, ba)
  }

  final case class JsonString[A](plainTextSchema: PlainTextSchema[A]) extends JsonSchema[A] {
    override val name: String = plainTextSchema.name
    override val jsonTypes: Set[JsonType] = Set(JsonType.String)
    override val jsonCodec: JsonCodec[A] = JsonCodec.string.transformOrFail(plainTextSchema.decode, plainTextSchema.encode)
  }

  // TODO (KR) : Option

  // TODO (KR) : Specified

  // TODO (KR) : private within
  final case class ProductSchema[A](
      name: String,
      jsonCodec: JsonCodec[A],
      fields: Chunk[ProductSchema.Field[?]],
  ) extends JsonSchema[A] {
    override val jsonTypes: Set[JsonType] = Set(JsonType.Object)
  }
  object ProductSchema {

    final case class Field[A](
        name: String,
        schema: JsonSchema[A],
    )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Deriving
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  override protected def internalDeriveProduct[Q <: Quotes, A](k0: K0[Q])(g: k0.ProductGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonSchema[A]] =
    g.builders.instanceFromLazyTypeClasses[JsonSchema] { tcs => new DeriveProductSchema[Q, A](k0)(g, tcs).makeJsonSchema }

  override protected def internalDeriveSum[Q <: Quotes, A](k0: K0[Q])(g: k0.SumGeneric[A])(using quotes: Q, tpe: Type[A]): Expr[JsonSchema[A]] =
    ??? // TODO (KR) :

  inline def derived[A]: JsonSchema[A] = ${ derivedImpl[A] }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def fromPlainTextSchema[A](using plainTextSchema: PlainTextSchema[A]): JsonSchema[A] = JsonSchema.JsonString(plainTextSchema)

  given string: JsonSchema[String] = fromPlainTextSchema

}
