package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema
import oxygen.schema.compiled.*

/**
  * A tool's `inputSchema` (MCP `2026-07-28`) — a JSON Schema draft 2020-12 object describing a tool's
  * arguments. Opaque (`<: Json.Obj`) so it can't be confused with any other json object, yet still
  * usable as a [[oxygen.json.Json.Obj]] without unwrapping.
  *
  * [[fromParams]] builds one from a derived tool's parameters (reusing oxygen-schema's compiled-schema
  * pipeline + [[JsonSchemaEmitter]], the transport-agnostic core of v1's `McpRequestCodec.inputSchema`);
  * the derivation macro ([[oxygen.mcp.generic.McpDerive]]) emits a call to it.
  */
opaque type ToolInputSchema <: Json.Obj = Json.Obj
object ToolInputSchema {

  def wrap(json: Json.Obj): ToolInputSchema = json

  given JsonSchema[ToolInputSchema] = JsonSchema.jsonObject.transform(wrap, x => x)

  /** One model-supplied parameter of a tool (the auth param, if any, is excluded upstream). */
  final case class Param(
      name: String,
      schema: JsonSchema[?],
      required: Boolean,
      doc: Option[String],
  )

  def fromParams(params: List[Param]): ToolInputSchema = {
    val program: Compiled[ArraySeq[(Param, CompiledSchemaRef)]] =
      params
        .foldRight(Compiled.succeed(List.empty[(Param, CompiledSchemaRef)])) { (p, acc) =>
          Compiled.json(p.schema).flatMap(ref => acc.map((p, ref) :: _))
        }
        .map(_.toArraySeq)

    val out: Compiled.Output[ArraySeq[(Param, CompiledSchemaRef)]] = program.compiled
    val schemas: FullCompiledSchemas = FullCompiledSchemas(out.schemas)
    val emitter: JsonSchemaEmitter = new JsonSchemaEmitter(schemas)

    val properties: ArraySeq[(String, Json)] = out.value.map { case (p, ref) => p.name -> describe(emitter.emit(ref), p.doc) }
    val required: ArraySeq[Json] = out.value.collect { case (p, _) if p.required => Json.string(p.name) }

    val fields: List[(String, Json)] =
      List[(String, Json)]("type" -> Json.string("object"), "properties" -> Json.Obj(properties)) :::
        (if required.nonEmpty then List("required" -> Json.Arr(required)) else Nil) :::
        emitter.defsObject.map("$defs" -> _).toList :::
        List("additionalProperties" -> Json.boolean(false))

    wrap(Json.Obj(fields.toArraySeq))
  }

  /** Attach a param's doc as the property's `description` (a `$ref` sibling is legal in draft 2020-12). */
  private def describe(schema: Json, description: Option[String]): Json =
    description match {
      case None    => schema
      case Some(d) =>
        schema match {
          case Json.Obj(fields) => Json.Obj(fields :+ ("description" -> Json.string(d)))
          case other            => Json.obj("description" -> Json.string(d), "allOf" -> Json.arr(other))
        }
    }

}
