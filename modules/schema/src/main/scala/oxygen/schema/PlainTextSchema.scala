package oxygen.schema

import oxygen.predef.core.*
import scala.util.Try

sealed trait PlainTextSchema[A] extends SchemaLike[PlainTextSchema, A] {

  override final def transform[B](ab: A => B, ba: B => A): PlainTextSchema[B] = PlainTextSchema.Transform(name, this, ab, ba)
  override final def transformOrFail[B](ab: A => Either[String, B], ba: B => A): PlainTextSchema[B] = PlainTextSchema.TransformOrFail(name, this, ab, ba)

  final def base64: PlainTextSchema[A] = PlainTextSchema.Base64(this)
  final def base64Url: PlainTextSchema[A] = PlainTextSchema.Base64Url(this)
  final def base64Mime: PlainTextSchema[A] = PlainTextSchema.Base64Mime(this)

}
object PlainTextSchema {

  inline def apply[A](using inst: PlainTextSchema[A]): PlainTextSchema[A] = inst

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Implementations
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object PlainString extends PlainTextSchema[String] {
    override val name: String = "String"
    override val encode: String => String = identity
    override val decode: String => Either[String, String] = _.asRight
  }

  final case class JsonEncoded[A](jsonSchema: JsonSchema[A]) extends PlainTextSchema[A] {
    override val name: String = jsonSchema.name
    override val encode: A => String = jsonSchema.encode
    override val decode: String => Either[String, A] = jsonSchema.decode
  }

  final case class Transform[A, B](
      name: String,
      a: PlainTextSchema[A],
      ab: A => B,
      ba: B => A,
  ) extends PlainTextSchema[B] {
    override val encode: B => String = b => a.encode(ba(b))
    override val decode: String => Either[String, B] = a.decode(_).map(ab)
  }

  final case class TransformOrFail[A, B](
      name: String,
      a: PlainTextSchema[A],
      ab: A => Either[String, B],
      ba: B => A,
  ) extends PlainTextSchema[B] {
    override val encode: B => String = b => a.encode(ba(b))
    override val decode: String => Either[String, B] = a.decode(_).flatMap(ab)
  }

  sealed trait Base64Like[A] extends PlainTextSchema[A] {
    val root: PlainTextSchema[A]
    override final val name: String = s"Base64[${root.name}]"
    val enc: java.util.Base64.Encoder
    val dec: java.util.Base64.Decoder
    override final val encode: A => String = a => enc.encodeToString(root.encode(a).getBytes)
    override final val decode: String => Either[String, A] = str => Try { dec.decode(str) }.toOption.toRight("Invalid Base64").flatMap { bytes => root.decode(new String(bytes)) }
  }

  final case class Base64[A] private[PlainTextSchema] (root: PlainTextSchema[A]) extends Base64Like[A] {
    override val enc: java.util.Base64.Encoder = java.util.Base64.getEncoder
    override val dec: java.util.Base64.Decoder = java.util.Base64.getDecoder
  }

  final case class Base64Url[A] private[PlainTextSchema] (root: PlainTextSchema[A]) extends Base64Like[A] {
    override val enc: java.util.Base64.Encoder = java.util.Base64.getUrlEncoder
    override val dec: java.util.Base64.Decoder = java.util.Base64.getUrlDecoder
  }

  final case class Base64Mime[A] private[PlainTextSchema] (root: PlainTextSchema[A]) extends Base64Like[A] {
    override val enc: java.util.Base64.Encoder = java.util.Base64.getMimeEncoder
    override val dec: java.util.Base64.Decoder = java.util.Base64.getMimeDecoder
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Givens
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  given string: PlainTextSchema[String] = PlainString
  given base64: [A: {PlainTextSchema as root}] => PlainTextSchema[oxygen.schema.Base64[A]] = root.base64.transform(oxygen.schema.Base64(_), _.value)
  given base64Url: [A: {PlainTextSchema as root}] => PlainTextSchema[oxygen.schema.Base64.Url[A]] = root.base64Url.transform(oxygen.schema.Base64.Url(_), _.value)
  given base64Mime: [A: {PlainTextSchema as root}] => PlainTextSchema[oxygen.schema.Base64.Mime[A]] = root.base64Mime.transform(oxygen.schema.Base64.Mime(_), _.value)

}
