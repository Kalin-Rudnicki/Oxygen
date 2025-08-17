package oxygen.schema

import oxygen.json.*
import oxygen.predef.core.*
import scala.annotation.tailrec

// FIX-PRE-MERGE (KR) : move into package
private[schema] sealed trait TemporaryRepr
private[schema] object TemporaryRepr {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Reprs
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait PlainRepr extends TemporaryRepr
  case object PlainString extends PlainRepr
  final case class PlainEnum(values: Seq[String], caseSensitive: Boolean) extends PlainRepr
  final case class PlainTransform(plainRef: SimpleTypeRef.Plain) extends PlainRepr

  sealed trait JsonRepr extends TemporaryRepr
  final case class JsonString(plainRef: SimpleTypeRef.Plain) extends JsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends JsonRepr
  final case class JsonOptional(jsonRef: SimpleTypeRef.Json) extends JsonRepr
  final case class JsonArray(jsonRef: SimpleTypeRef.Json) extends JsonRepr
  final case class JsonProduct(fields: ArraySeq[JsonField]) extends JsonRepr
  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[JsonCase]) extends JsonRepr
  final case class JsonTransform(jsonRef: SimpleTypeRef.Json) extends JsonRepr

  final case class JsonField(
      name: String,
      ref: SimpleTypeRef.Json,
  )

  final case class JsonCase(
      name: String,
      ref: SimpleTypeRef.Json,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Reprs(
      plain: Map[SimpleTypeRef.Plain, PlainRepr],
      json: Map[SimpleTypeRef.Json, JsonRepr],
  ) {

    def ++(that: Reprs): Reprs = Reprs(this.plain ++ that.plain, this.json ++ that.json)

    def withPlain(typeTag: TypeTag[?], repr: PlainRepr): Generated.Plain = {
      val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
      Generated.Plain(ref, Reprs(this.plain + (ref -> repr), this.json))
    }

    def withJson(typeTag: TypeTag[?], repr: JsonRepr): Generated.Json = {
      val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
      Generated.Json(ref, Reprs(this.plain, this.json + (ref -> repr)))
    }

  }
  object Reprs {
    val empty: Reprs = Reprs(Map.empty, Map.empty)
  }

  final case class Generating(
      reprs: Reprs,
      recursive: Set[SimpleTypeRef.Json],
  )

  enum Generated {
    case Plain(ref: SimpleTypeRef.Plain, reprs: Reprs)
    case Json(ref: SimpleTypeRef.Json, reprs: Reprs)

    val ref: SimpleTypeRef
    val reprs: Reprs

    def withPlain(typeTag: TypeTag[?], repr: PlainRepr): Generated.Plain = reprs.withPlain(typeTag, repr)
    def withJson(typeTag: TypeTag[?], repr: JsonRepr): Generated.Json = reprs.withJson(typeTag, repr)

  }
  object Generated {

    def plain(typeTag: TypeTag[?], repr: PlainRepr): Generated.Plain = {
      val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
      Generated.Plain(ref, Reprs(Map(ref -> repr), Map.empty))
    }

    def json(typeTag: TypeTag[?], repr: JsonRepr): Generated.Json = {
      val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
      Generated.Json(ref, Reprs(Map.empty, Map(ref -> repr)))
    }

    def emptyPlain(typeTag: TypeTag[?]): Generated.Plain =
      Generated.Plain(SimpleTypeRef.Plain(typeTag), Reprs.empty)

    def emptyJson(typeTag: TypeTag[?]): Generated.Json =
      Generated.Json(SimpleTypeRef.Json(typeTag), Reprs.empty)

  }

  private def convertPlain(
      schema: PlainTextSchema[?],
      generating: Generating,
  ): Generated.Plain = {
    val myRef: SimpleTypeRef.Plain = SimpleTypeRef.Plain(schema.typeTag)
    val res =
      if (generating.reprs.plain.contains(myRef))
        Generated.emptyPlain(schema.typeTag)
      else
        schema match {
          case PlainTextSchema.StringSchema =>
            Generated.plain(schema.typeTag, PlainString)
          case schema: PlainTextSchema.EnumSchema[?] =>
            Generated.plain(schema.typeTag, PlainEnum(schema.encodedValues, schema.caseSensitive))
          case schema: PlainTextSchema.Transform[?, ?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withPlain(schema.typeTag, PlainTransform(gen.ref))
          case schema: PlainTextSchema.TransformOrFail[?, ?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withPlain(schema.typeTag, PlainTransform(gen.ref))
        }

    println(
      s"plain[${schema.typeTag}]\n  (in.plain = ${generating.reprs.plain.keySet}, in.json = ${generating.reprs.json.keySet})\n  (out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
    )

    res
  }

  private def convertJson(
      schema: JsonSchema[?],
      generating: Generating,
  ): Generated.Json = {
    val myRef: SimpleTypeRef.Json = SimpleTypeRef.Json(schema.typeTag)
    val res =
      if (generating.reprs.json.contains(myRef) || generating.recursive.contains(myRef)) {
        println("json.short-circuit")
        Generated.emptyJson(schema.typeTag)
      } else
        schema match {
          //
          case schema: JsonSchema.StringSchema[?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonString(gen.ref))
          case JsonSchema.BooleanSchema =>
            Generated.json(schema.typeTag, JsonAST(Json.Type.Boolean.some))
          case schema: JsonSchema.ASTSchema[?] =>
            Generated.json(schema.typeTag, JsonAST(schema.specificType))
          case schema: JsonSchema.IntNumberSchema[?] =>
            Generated.json(schema.typeTag, JsonAST(Json.Type.Number.some))
          case schema: JsonSchema.NumberSchema[?] =>
            Generated.json(schema.typeTag, JsonAST(Json.Type.Number.some))
          case schema: JsonSchema.OptionalSchema[?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonOptional(gen.ref))
          case schema: JsonSchema.ArraySchema[?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonArray(gen.ref))
          case schema: JsonSchema.Transform[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          case schema: JsonSchema.TransformOrFail[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          //
          case schema: JsonSchema.ProductSchema[?] =>
            val recursive = generating.recursive + myRef
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.ProductField[?]], acc: Growable[JsonField]): (ArraySeq[JsonField], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = convertJson(head.schema, Generating(in, recursive))
                  val field = JsonField(head.name, gen.ref)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ field)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (fields, gen) = loop(generating.reprs, Reprs.empty, schema.fields.toList, Growable.empty)
            gen.withJson(schema.typeTag, JsonProduct(fields))
          case schema: JsonSchema.SumSchema[?] =>
            val recursive = generating.recursive + myRef
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.SumCase[?]], acc: Growable[JsonCase]): (ArraySeq[JsonCase], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = convertJson(head.schema, Generating(in, recursive))
                  val kase = JsonCase(head.name, gen.ref)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ kase)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (cases, gen) = loop(generating.reprs, Reprs.empty, schema.children.toList, Growable.empty)
            gen.withJson(schema.typeTag, JsonSum(schema.discriminator, cases))
          case schema: JsonSchema.TransformProduct[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          case schema: JsonSchema.TransformOrFailProduct[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
        }

    println(
      s"json[${schema.typeTag}]\n  (in.plain = ${generating.reprs.plain.keySet}, in.json = ${generating.reprs.json.keySet})\n  (out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
    )

    res
  }

  def from(
      schema: AnySchema,
      generating: Generating,
  ): Generated =
    schema match {
      case schema: PlainTextSchema[?] => convertPlain(schema, generating)
      case schema: JsonSchema[?]      => convertJson(schema, generating)
    }

}
