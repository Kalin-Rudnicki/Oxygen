package oxygen.schema.intermediate

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.*
import scala.annotation.tailrec

sealed trait TemporaryRepr
object TemporaryRepr {

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

  private[intermediate] final case class CompileInput(
      reprs: Reprs,
      recursive: Set[SimpleTypeRef.Json],
  )

  private[intermediate] def compilePlain(
      schema: PlainTextSchema[?],
      input: CompileInput,
  ): Compiled.Plain = {
    val myRef: SimpleTypeRef.Plain = SimpleTypeRef.Plain(schema.typeTag)
    val res =
      if (input.reprs.plain.contains(myRef))
        Compiled.emptyPlain(schema.typeTag)
      else
        schema match {
          case PlainTextSchema.StringSchema =>
            Compiled.plain(schema.typeTag, PlainString)
          case schema: PlainTextSchema.EnumSchema[?] =>
            Compiled.plain(schema.typeTag, PlainEnum(schema.encodedValues, schema.caseSensitive))
          case schema: PlainTextSchema.Transform[?, ?] =>
            val gen = compilePlain(schema.underlying, input)
            gen.withPlain(schema.typeTag, PlainTransform(gen.ref))
          case schema: PlainTextSchema.TransformOrFail[?, ?] =>
            val gen = compilePlain(schema.underlying, input)
            gen.withPlain(schema.typeTag, PlainTransform(gen.ref))
        }

    println(
      s"plain[${schema.typeTag}]\n  (in.plain = ${input.reprs.plain.keySet}, in.json = ${input.reprs.json.keySet})\n  (out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
    )

    res
  }

  private[intermediate] def compileJson(
      schema: JsonSchema[?],
      input: CompileInput,
  ): Compiled.Json = {
    val myRef: SimpleTypeRef.Json = SimpleTypeRef.Json(schema.typeTag)
    val res =
      if (input.reprs.json.contains(myRef) || input.recursive.contains(myRef)) {
        println("json.short-circuit")
        Compiled.emptyJson(schema.typeTag)
      } else
        schema match {
          //
          case schema: JsonSchema.StringSchema[?] =>
            val gen = compilePlain(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonString(gen.ref))
          case JsonSchema.BooleanSchema =>
            Compiled.json(schema.typeTag, JsonAST(Json.Type.Boolean.some))
          case schema: JsonSchema.ASTSchema[?] =>
            Compiled.json(schema.typeTag, JsonAST(schema.specificType))
          case schema: JsonSchema.IntNumberSchema[?] =>
            Compiled.json(schema.typeTag, JsonAST(Json.Type.Number.some))
          case schema: JsonSchema.NumberSchema[?] =>
            Compiled.json(schema.typeTag, JsonAST(Json.Type.Number.some))
          case schema: JsonSchema.OptionalSchema[?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonOptional(gen.ref))
          case schema: JsonSchema.ArraySchema[?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonArray(gen.ref))
          case schema: JsonSchema.Transform[?, ?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          case schema: JsonSchema.TransformOrFail[?, ?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          //
          case schema: JsonSchema.ProductSchema[?] =>
            val recursive = input.recursive + myRef
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.ProductField[?]], acc: Growable[JsonField]): (ArraySeq[JsonField], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = compileJson(head.schema, CompileInput(in, recursive))
                  val field = JsonField(head.name, gen.ref)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ field)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (fields, gen) = loop(input.reprs, Reprs.empty, schema.fields.toList, Growable.empty)
            gen.withJson(schema.typeTag, JsonProduct(fields))
          case schema: JsonSchema.SumSchema[?] =>
            val recursive = input.recursive + myRef
            @tailrec
            def loop(in: Reprs, out: Reprs, queue: List[JsonSchema.SumCase[?]], acc: Growable[JsonCase]): (ArraySeq[JsonCase], Reprs) =
              queue match {
                case head :: tail =>
                  val gen = compileJson(head.schema, CompileInput(in, recursive))
                  val kase = JsonCase(head.name, gen.ref)
                  loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ kase)
                case Nil =>
                  (acc.toArraySeq, out)
              }

            val (cases, gen) = loop(input.reprs, Reprs.empty, schema.children.toList, Growable.empty)
            gen.withJson(schema.typeTag, JsonSum(schema.discriminator, cases))
          case schema: JsonSchema.TransformProduct[?, ?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
          case schema: JsonSchema.TransformOrFailProduct[?, ?] =>
            val gen = compileJson(schema.underlying, input)
            gen.withJson(schema.typeTag, JsonTransform(gen.ref))
        }

    println(
      s"json[${schema.typeTag}]\n  (in.plain = ${input.reprs.plain.keySet}, in.json = ${input.reprs.json.keySet})\n  (out.plain = ${res.reprs.plain.keySet}, out.json = ${res.reprs.json.keySet})",
    )

    res
  }

}
// FIX-PRE-MERGE (KR) : remove `println`
