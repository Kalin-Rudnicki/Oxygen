package oxygen.schema.intermediate

import oxygen.core.SourcePosition
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.*
import scala.annotation.tailrec

private[schema] sealed trait IntermediateRepr {

  final def ignoreWhenComputingDistinctTypes: Boolean = this match
    case _: IntermediateRepr.JsonEncodedText => true
    case _: IntermediateRepr.JsonString      => true
    case _                                   => false

}
object IntermediateRepr {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Reprs
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait PlainRepr extends IntermediateRepr
  final case class PlainText(sourceFile: SourcePosition) extends PlainRepr
  final case class FormattedText(plainRef: IntermediateTypeRef.Plain, formats: NonEmptyList[String]) extends PlainRepr
  final case class Enum(values: Seq[String], caseSensitive: Boolean, exhaustive: Boolean) extends PlainRepr
  final case class BearerToken(payloadRef: IntermediateTypeRef.Plain) extends PlainRepr
  final case class EncodedText(plainRef: IntermediateTypeRef.Plain, encoding: String) extends PlainRepr
  final case class JsonEncodedText(jsonRef: IntermediateTypeRef.Json) extends PlainRepr
  final case class PlainTransform(plainRef: IntermediateTypeRef.Plain, sourceFile: SourcePosition) extends PlainRepr

  sealed trait JsonRepr extends IntermediateRepr
  final case class JsonString(plainRef: IntermediateTypeRef.Plain) extends JsonRepr
  final case class JsonNumber(numberFormat: NumberFormat) extends JsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends JsonRepr
  final case class JsonOption(jsonRef: IntermediateTypeRef.Json) extends JsonRepr
  final case class JsonSpecified(jsonRef: IntermediateTypeRef.Json) extends JsonRepr
  final case class JsonArray(jsonRef: IntermediateTypeRef.Json) extends JsonRepr
  final case class JsonMap(keyPlainRef: IntermediateTypeRef.Plain, valueJsonRef: IntermediateTypeRef.Json) extends JsonRepr
  final case class JsonProduct(fields: ArraySeq[JsonField]) extends JsonRepr
  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[JsonCase]) extends JsonRepr
  final case class JsonTransform(jsonRef: IntermediateTypeRef.Json, sourceFile: SourcePosition) extends JsonRepr

  final case class JsonField(
      name: String,
      ref: IntermediateTypeRef.Json,
  )

  final case class JsonCase(
      name: String,
      ref: IntermediateTypeRef.Json,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[intermediate] final case class CompileInput(
      reprs: IntermediateReprs,
      recursive: Set[IntermediateTypeRef.Json],
  )

  private[intermediate] def compilePlain(
      schema: PlainTextSchema[?],
      input: CompileInput,
  ): IntermediateCompiledRef.Plain = {
    val myRef: IntermediateTypeRef.Plain = IntermediateTypeRef.plain(schema)
    if input.reprs.plain.contains(myRef) then IntermediateCompiledRef.emptyPlain(schema, input.reprs)
    else
      schema match {
        case schema: PlainTextSchema.StringSchema.type =>
          IntermediateCompiledRef.plain(schema, PlainText(schema.pos), input.reprs)
        case schema: PlainTextSchema.EnumSchema[?] =>
          IntermediateCompiledRef.plain(schema, Enum(schema.encodedValues, schema.caseSensitive, schema.exhaustive), input.reprs)
        case PlainTextSchema.FromStringCodec(_, pos) =>
          IntermediateCompiledRef.plain(schema, PlainText(pos), input.reprs)
        case PlainTextSchema.WithFormats(underlying, formats) =>
          val gen = compilePlain(underlying, input)
          gen.withPlain(schema, FormattedText(gen.ref, formats))
        case PlainTextSchema.EncodedText(underlying, encoding) =>
          val gen = compilePlain(underlying, input)
          gen.withPlain(schema, EncodedText(gen.ref, encoding.name))
        case schema: PlainTextSchema.BearerTokenSchema[?] =>
          val gen = compilePlain(schema.payloadSchema, input)
          gen.withPlain(schema, BearerToken(gen.ref))
        case schema: PlainTextSchema.JsonEncoded[?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withPlain(schema, JsonEncodedText(gen.ref))
        case schema: PlainTextSchema.Transform[?, ?] =>
          val gen = compilePlain(schema.underlying, input)
          gen.withPlain(schema, PlainTransform(gen.ref, schema.pos))
        case schema: PlainTextSchema.TransformOrFail[?, ?] =>
          val gen = compilePlain(schema.underlying, input)
          gen.withPlain(schema, PlainTransform(gen.ref, schema.pos))
      }
  }

  private[intermediate] def compileJson(
      schema: JsonSchema[?],
      input: CompileInput,
  ): IntermediateCompiledRef.Json = {
    val myRef: IntermediateTypeRef.Json = IntermediateTypeRef.json(schema)
    if input.reprs.json.contains(myRef) || input.recursive.contains(myRef) then IntermediateCompiledRef.emptyJson(schema, input.reprs)
    else
      schema match {
        //
        case schema: JsonSchema.StringSchema[?] =>
          val gen = compilePlain(schema.underlying, input)
          gen.withJson(schema, JsonString(gen.ref))
        case JsonSchema.BooleanSchema =>
          IntermediateCompiledRef.json(schema, JsonAST(Json.Type.Boolean.some), input.reprs)
        case schema: JsonSchema.ASTSchema[?] =>
          IntermediateCompiledRef.json(schema, JsonAST(schema.specificType), input.reprs)
        case schema: JsonSchema.JsonNumber[?] =>
          IntermediateCompiledRef.json(schema, JsonNumber(schema.numberFormat), input.reprs)
        case schema: JsonSchema.OptionSchema[?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonOption(gen.ref))
        case schema: JsonSchema.SpecifiedSchema[?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonSpecified(gen.ref))
        case schema: JsonSchema.ArraySchema[?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonArray(gen.ref))
        case schema: JsonSchema.MapSchema[?, ?] =>
          val keyGen = compilePlain(schema.keySchema, input)
          val valueGen = compileJson(schema.valueSchema, CompileInput(keyGen.reprs, input.recursive))
          valueGen.withJson(schema, JsonMap(keyGen.ref, valueGen.ref))
        case schema: JsonSchema.Transform[?, ?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonTransform(gen.ref, schema.pos))
        case schema: JsonSchema.TransformOrFail[?, ?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonTransform(gen.ref, schema.pos))
        case schema: JsonSchema.TransformObject[?, ?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonTransform(gen.ref, schema.pos))
        case schema: JsonSchema.TransformOrFailObject[?, ?] =>
          val gen = compileJson(schema.underlying, input)
          gen.withJson(schema, JsonTransform(gen.ref, schema.pos))

        /////// ProductSchema ///////////////////////////////////////////////////////////////
        case schema: JsonSchema.ProductSchema[?] =>
          val recursive = input.recursive + myRef
          @tailrec
          def loop(in: IntermediateReprs, out: IntermediateReprs, queue: List[JsonSchema.ProductField[?]], acc: Growable[JsonField]): (ArraySeq[JsonField], IntermediateReprs) =
            queue match {
              case head :: tail =>
                val gen = compileJson(head.schema, CompileInput(in, recursive))
                val field = JsonField(head.name, gen.ref)
                loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ field)
              case Nil =>
                (acc.toArraySeq, out)
            }

          val (fields, gen) = loop(input.reprs, IntermediateReprs.empty, schema.fields.toList, Growable.empty)
          gen.withJson(schema, JsonProduct(fields))

        /////// SumSchema ///////////////////////////////////////////////////////////////
        case schema: JsonSchema.SumSchema[?] =>
          val recursive = input.recursive + myRef
          @tailrec
          def loop(in: IntermediateReprs, out: IntermediateReprs, queue: List[JsonSchema.SumCase[?]], acc: Growable[JsonCase]): (ArraySeq[JsonCase], IntermediateReprs) =
            queue match {
              case head :: tail =>
                val gen = compileJson(head.schema, CompileInput(in, recursive))
                val kase = JsonCase(head.name, gen.ref)
                loop(in ++ gen.reprs, out ++ gen.reprs, tail, acc :+ kase)
              case Nil =>
                (acc.toArraySeq, out)
            }

          val (cases, gen) = loop(input.reprs, IntermediateReprs.empty, schema.children.toList, Growable.empty)
          gen.withJson(schema, JsonSum(schema.discriminator, cases))
      }
  }

}
