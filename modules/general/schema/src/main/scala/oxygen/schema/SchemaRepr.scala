package oxygen.schema

import oxygen.json.*
import oxygen.predef.core.*
import scala.annotation.tailrec

// FIX-PRE-MERGE (KR) : naming
object SchemaRepr {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Reprs
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait Repr

  sealed trait PlainRepr extends Repr
  case object PlainString extends PlainRepr
  final case class PlainEnum(values: Seq[String], caseSensitive: Boolean) extends PlainRepr
  final case class PlainTransform(plainRef: TypeTag[?]) extends PlainRepr

  sealed trait JsonRepr extends Repr
  final case class JsonString(plainRef: TypeTag[?]) extends JsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends JsonRepr
  final case class JsonOptional(jsonRef: TypeTag[?]) extends JsonRepr
  final case class JsonArray(jsonRef: TypeTag[?]) extends JsonRepr
  final case class JsonProduct(fields: ArraySeq[JsonField]) extends JsonRepr
  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[JsonCase]) extends JsonRepr
  final case class JsonTransform(jsonRef: TypeTag[?]) extends JsonRepr

  final case class JsonField(
      name: String,
      ref: TypeTag[?],
  )

  final case class JsonCase(
      name: String,
      ref: TypeTag[?],
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Reprs(
      plain: Map[TypeTag[?], PlainRepr],
      json: Map[TypeTag[?], JsonRepr],
  ) {
    def ++(that: Reprs): Reprs = Reprs(this.plain ++ that.plain, this.json ++ that.json)
  }
  object Reprs {
    val empty: Reprs = Reprs(Map.empty, Map.empty)
  }

  final case class Generating(
      reprs: Reprs,
      recursive: Set[TypeTag[?]],
  )

  final case class Generated(
      typeTag: TypeTag[?],
      reprs: Reprs,
  ) {

    def withPlain(typeTag: TypeTag[?], repr: PlainRepr): Generated =
      Generated(typeTag, Reprs(reprs.plain + (typeTag -> repr), Map.empty))

    def withJson(typeTag: TypeTag[?], repr: JsonRepr): Generated =
      Generated(typeTag, Reprs(Map.empty, reprs.json + (typeTag -> repr)))

  }
  object Generated {

    def plain(typeTag: TypeTag[?], repr: PlainRepr): Generated =
      Generated(typeTag, Reprs(Map(typeTag -> repr), Map.empty))

    def json(typeTag: TypeTag[?], repr: JsonRepr): Generated =
      Generated(typeTag, Reprs(Map.empty, Map(typeTag -> repr)))

  }

  private def convertPlain(
      schema: PlainTextSchema[?],
      generating: Generating,
  ): Generated = {
    val res =
      if (generating.reprs.plain.contains(schema.typeTag)) Generated(schema.typeTag, Reprs.empty)
      else
        schema match {
          case PlainTextSchema.StringSchema            => Generated.plain(schema.typeTag, PlainString)
          case schema: PlainTextSchema.EnumSchema[?]   => Generated.plain(schema.typeTag, PlainEnum(schema.encodedValues, schema.caseSensitive))
          case schema: PlainTextSchema.Transform[?, ?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withPlain(schema.typeTag, PlainTransform(gen.typeTag))
          case schema: PlainTextSchema.TransformOrFail[?, ?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withPlain(schema.typeTag, PlainTransform(gen.typeTag))
        }

    println(
      s"plain[${schema.typeTag}](in.plain = ${generating.reprs.plain.keySet}, in.json = ${generating.reprs.json.keySet})(out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
    )

    res
  }

  private def convertJson(
      schema: JsonSchema[?],
      generating: Generating,
  ): Generated = {
    val res =
      if (generating.reprs.json.contains(schema.typeTag) || generating.recursive.contains(schema.typeTag)) Generated(schema.typeTag, Reprs.empty)
      else
        schema match {
          //
          case schema: JsonSchema.StringSchema[?] =>
            val gen = convertPlain(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonString(gen.typeTag))
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
            gen.withJson(schema.typeTag, JsonOptional(gen.typeTag))
          case schema: JsonSchema.ArraySchema[?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonArray(gen.typeTag))
          case schema: JsonSchema.Transform[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.typeTag))
          case schema: JsonSchema.TransformOrFail[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.typeTag))
          //
          case schema: JsonSchema.ProductSchema[?] =>
            val recursive = generating.recursive + schema.typeTag
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.ProductField[?]], acc: Growable[JsonField]): (ArraySeq[JsonField], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = convertJson(head.schema, Generating(in, recursive))
                  val field = JsonField(head.name, gen.typeTag)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ field)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (fields, gen) = loop(generating.reprs, Reprs.empty, schema.fields.toList, Growable.empty)
            val productRepr = JsonProduct(fields)
            Generated(schema.typeTag, gen ++ Reprs(Map.empty, Map(schema.typeTag -> productRepr)))
          case schema: JsonSchema.SumSchema[?] =>
            val recursive = generating.recursive + schema.typeTag
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.SumCase[?]], acc: Growable[JsonCase]): (ArraySeq[JsonCase], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = convertJson(head.schema, Generating(in, recursive))
                  val kase = JsonCase(head.name, gen.typeTag)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ kase)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (cases, gen) = loop(generating.reprs, Reprs.empty, schema.children.toList, Growable.empty)
            val sumRepr = JsonSum(schema.discriminator, cases)
            Generated(schema.typeTag, gen ++ Reprs(Map.empty, Map(schema.typeTag -> sumRepr)))
          case schema: JsonSchema.TransformProduct[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.typeTag))
          case schema: JsonSchema.TransformOrFailProduct[?, ?] =>
            val gen = convertJson(schema.underlying, generating)
            gen.withJson(schema.typeTag, JsonTransform(gen.typeTag))
        }

    println(
      s"json[${schema.typeTag}](in.plain = ${generating.reprs.plain.keySet}, in.json = ${generating.reprs.json.keySet})(out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
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
