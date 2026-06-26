package oxygen.schema.compiled

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.NumberFormat
import scala.collection.mutable

/**
  * Emits standard JSON Schema (draft 2020-12) from oxygen's compiled schema graph.
  *
  * This lives in `oxygen-schema` because it is a pure function of the schema layer and is
  * independently useful, but its first consumer is oxygen-http's MCP support, where every tool's
  * `inputSchema` must be standard JSON Schema (the dialect MCP standardized on as of the
  * `2025-11-25` revision).
  *
  * Recursive/named product & sum types are emitted into `$defs` and referenced via `$ref` — this is
  * what makes recursive types terminate. Everything else (scalars, arrays, maps, enums, options) is
  * emitted inline. Option/Nullable/Specified wrappers are already resolved away by
  * [[FullCompiledSchemas]]; nullability is handled at the product-field level.
  *
  * Use the instance API ([[emit]] + [[defsObject]]) when emitting several refs that should share one
  * `$defs` bundle (e.g. all params of one MCP tool); use [[JsonSchemaEmitter.emitStandalone]] for a
  * single, self-contained schema.
  */
final class JsonSchemaEmitter(schemas: FullCompiledSchemas) {

  private val defs: mutable.LinkedHashMap[String, Json] = mutable.LinkedHashMap.empty
  private val building: mutable.Set[String] = mutable.Set.empty

  /** Emit the JSON Schema for a single ref, accumulating any `$defs` it requires. */
  def emit(ref: CompiledSchemaRef): Json =
    emitSchema(schemas.resolve(ref))

  /** The `$defs` accumulated across all [[emit]] calls, or `None` if none were needed. */
  def defsObject: Option[Json] =
    Option.when(defs.nonEmpty)(Json.Obj(ArraySeq.from(defs)))

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def obj(fields: (String, Json)*): Json.Obj = Json.Obj(fields.toArraySeq)

  private def emitSchema(schema: FullCompiledSchema): Json =
    schema match
      case j: FullCompiledJsonSchema  => emitJson(j)
      case p: FullCompiledPlainSchema => emitPlain(p)

  private def emitJson(schema: FullCompiledJsonSchema): Json = {
    import FullCompiledJsonSchema.*
    schema match
      case s: JsonNumber  => numberSchema(s.numberFormat)
      case s: JsonAST     => astSchema(s.jsonType)
      case s: JsonString  => emitPlain(s.elemType.value)
      case s: JsonArray   => obj("type" -> Json.string("array"), "items" -> emitJson(s.elemType.value))
      case s: JsonMap     => obj("type" -> Json.string("object"), "additionalProperties" -> emitJson(s.valueType.value))
      case s: JsonProduct => defRef(s.ref, productBody(s))
      case s: JsonSum     => defRef(s.ref, sumBody(s))
      case s: JsonOneOf   => obj("oneOf" -> Json.Arr(s.oneOf.map(emitJson)))
      case s: Transformed => maybeNullable(emitJson(s.underlyingType.value), s.isInherentlyNullable)
  }

  private def emitPlain(schema: FullCompiledPlainSchema): Json = {
    import FullCompiledPlainSchema.*
    schema match
      case _: PlainText     => stringSchema
      case e: Enum          => enumSchema(e)
      case s: FormattedText => emitPlain(s.underlyingType.value)
      case s: EncodedText   => emitPlain(s.underlyingType.value)
      case _: JsonEncoded   => stringSchema // plain text whose content is JSON-encoded — a string on the wire
      case _: BearerToken   => stringSchema
      case s: Transformed   => emitPlain(s.underlyingType.value)
  }

  /** Named product/sum types become `$defs` entries referenced by `$ref` (terminates recursion). */
  private def defRef(ref: CompiledSchemaRef, buildBody: => Json): Json = {
    val name = defName(ref)
    if !defs.contains(name) && !building.contains(name) then {
      building += name
      val body = buildBody
      defs.update(name, body)
      building -= name
    }
    obj("$ref" -> Json.string(s"#/$$defs/$name"))
  }

  private def defName(ref: CompiledSchemaRef): String =
    ref.primaryReference.fullTypeName.replaceAll("[^A-Za-z0-9_]", "_")

  private def productBody(s: FullCompiledJsonSchema.JsonProduct): Json = {
    val fieldEntries: ArraySeq[(String, Json)] =
      s.fields.map { f =>
        val base = emitJson(f.fieldType.value)
        val nulled = maybeNullable(base, f.nullable)
        val described = f.description.fold(nulled)(withDescription(nulled, _))
        f.fieldName -> described
      }
    val required: ArraySeq[Json] =
      s.fields.collect { case f if f.onMissing.isEmpty => Json.string(f.fieldName) }
    val base: List[(String, Json)] =
      List("type" -> Json.string("object"), "properties" -> Json.Obj(fieldEntries)) :::
        (if required.nonEmpty then List("required" -> Json.Arr(required)) else Nil) :::
        List("additionalProperties" -> Json.boolean(false))
    Json.Obj(base.toArraySeq)
  }

  // draft 2020-12 has no `discriminator` keyword (that is OpenAPI). A sum becomes a `oneOf` of its
  // case shapes; the model picks the matching branch by shape.
  private def sumBody(s: FullCompiledJsonSchema.JsonSum): Json =
    obj("oneOf" -> Json.Arr(s.cases.map(c => emitJson(c.caseType.value))))

  private def numberSchema(fmt: NumberFormat): Json =
    fmt match
      case _: NumberFormat.Whole      => obj("type" -> Json.string("integer"))
      case _: NumberFormat.Fractional => obj("type" -> Json.string("number"))

  private val stringSchema: Json = obj("type" -> Json.string("string"))

  private def enumSchema(e: FullCompiledPlainSchema.Enum): Json =
    if e.exhaustive && e.values.nonEmpty then
      obj("type" -> Json.string("string"), "enum" -> Json.Arr(e.values.map(Json.string).toArraySeq))
    else stringSchema

  private def astSchema(jsonType: Option[Json.Type]): Json =
    jsonType match
      case None                    => Json.Obj(ArraySeq.empty) // `{}` — any JSON value
      case Some(Json.Type.String)  => obj("type" -> Json.string("string"))
      case Some(Json.Type.Number)  => obj("type" -> Json.string("number"))
      case Some(Json.Type.Boolean) => obj("type" -> Json.string("boolean"))
      case Some(Json.Type.Array)   => obj("type" -> Json.string("array"))
      case Some(Json.Type.Object)  => obj("type" -> Json.string("object"))
      case Some(Json.Type.Null)    => obj("type" -> Json.string("null"))

  private def maybeNullable(schema: Json, nullable: Boolean): Json =
    if !nullable then schema
    else obj("anyOf" -> Json.arr(schema, obj("type" -> Json.string("null"))))

  private def withDescription(schema: Json, description: String): Json =
    schema match
      case Json.Obj(fields) => Json.Obj(fields :+ ("description" -> Json.string(description)))
      case other            => obj("value" -> other, "description" -> Json.string(description))

}
object JsonSchemaEmitter {

  /** The JSON Schema dialect MCP standardized on (`2025-11-25` revision, SEP-1613). */
  val dialect: String = "https://json-schema.org/draft/2020-12/schema"

  /** Emit a complete, self-contained JSON Schema for a single ref (with `$schema` + `$defs`). */
  def emitStandalone(schemas: FullCompiledSchemas, ref: CompiledSchemaRef): Json = {
    val emitter = new JsonSchemaEmitter(schemas)
    val body = emitter.emit(ref)
    val extras: List[(String, Json)] =
      ("$schema" -> Json.string(dialect)) ::
        emitter.defsObject.map("$defs" -> _).toList
    body match
      case Json.Obj(fields) => Json.Obj(extras.toArraySeq ++ fields)
      case other            => Json.Obj((("$value" -> other) :: extras).toArraySeq)
  }

}
