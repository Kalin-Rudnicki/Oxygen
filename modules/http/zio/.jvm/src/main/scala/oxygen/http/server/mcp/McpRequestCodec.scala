package oxygen.http.server.mcp

import oxygen.http.server.McpInput
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.*
import oxygen.schema.compiled.*

/**
  * Decodes the flat JSON `arguments` object of an MCP `tools/call` directly into an endpoint's `In`
  * tuple — the MCP analogue of [[RequestCodec]], deliberately *not* routed through a synthetic HTTP
  * request. Macro-derived per `@mcp.tool` endpoint (see `RouteRepr.mcpToolCodecExpr`):
  *
  *   - each non-auth function param is decoded from `args[name]` via its `JsonSchema` (absent key ⇒
  *     `null`, so `Option` params decode to `None`),
  *   - the `@mcp.auth` param (if any) is decoded from the validated bearer token via its
  *     `PlainTextSchema` — so the handler receives its normal auth type (`JWT.Std[…]`, `BearerToken`, …),
  *     identical to the HTTP path; it is excluded from [[inputParams]] / the tool input schema.
  */
trait McpRequestCodec[In] {

  /** The model-supplied params (auth param excluded), in declaration order — used to build the tool input schema. */
  def inputParams: ArraySeq[McpRequestCodec.InputParam]

  /** Decode the `tools/call` `arguments` object (+ the validated bearer, for an `@mcp.auth` param) into `In`. */
  def decode(in: McpInput): Either[String, In]

}
object McpRequestCodec {

  final case class InputParam(
      name: String,
      schema: AnySchema,
      required: Boolean,
      description: Option[String],
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decode helpers (called from generated code — keeps the macro trivial)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Pull a named field from the args object, defaulting to `null` when absent (so `Option` ⇒ `None`). */
  def argValue(args: Json, name: String): Json =
    args match
      case Json.Obj(fields) => fields.collectFirst { case (k, v) if k == name => v }.getOrElse(Json.Null)
      case _                => Json.Null

  def decodeJsonArg[T](decoder: JsonDecoder[T], args: Json, name: String): Either[String, T] =
    decoder.decodeJsonAST(argValue(args, name)).leftMap(e => s"arg `$name`: ${e.getMessage}")

  def decodeAuthArg[T](schema: PlainTextSchema[T], bearer: Option[String]): Either[String, T] =
    bearer match
      case Some(token) => schema.decode(token)
      case None        => "missing bearer token for @mcp.auth param".asLeft

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Input schema (runtime; reuses the compiled-schema pipeline + JsonSchemaEmitter)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Build the MCP tool `inputSchema` (JSON Schema draft 2020-12) for a codec's params. */
  def inputSchema(codec: McpRequestCodec[?]): Json = {
    val program: Compiled[ArraySeq[(InputParam, CompiledSchemaRef)]] =
      codec.inputParams.foldRight(Compiled.succeed(List.empty[(InputParam, CompiledSchemaRef)])) { (p, acc) =>
        compileAny(p.schema).flatMap(ref => acc.map((p, ref) :: _))
      }.map(_.toArraySeq)

    val out: Compiled.Output[ArraySeq[(InputParam, CompiledSchemaRef)]] = program.compiled
    val schemas: FullCompiledSchemas = FullCompiledSchemas(out.schemas)
    val emitter: JsonSchemaEmitter = new JsonSchemaEmitter(schemas)

    val properties: ArraySeq[(String, Json)] = out.value.map { case (p, ref) => p.name -> describe(emitter.emit(ref), p.description) }
    val required: ArraySeq[Json] = out.value.collect { case (p, _) if p.required => Json.string(p.name) }

    val fields: List[(String, Json)] =
      List("type" -> Json.string("object"), "properties" -> Json.Obj(properties)) :::
        (if required.nonEmpty then List("required" -> Json.Arr(required)) else Nil) :::
        emitter.defsObject.map("$defs" -> _).toList :::
        List("additionalProperties" -> Json.boolean(false))

    Json.Obj(fields.toArraySeq)
  }

  private def compileAny(schema: AnySchema): Compiled[CompiledSchemaRef] =
    schema match
      case p: PlainTextSchema[?] => Compiled.plain(p)
      case j: JsonSchema[?]      => Compiled.json(j)

  /** Attach a param's `@httpDoc` as the property's `description` (a `$ref` sibling is legal in draft 2020-12). */
  private def describe(schema: Json, description: Option[String]): Json =
    description match
      case None    => schema
      case Some(d) =>
        schema match
          case Json.Obj(fields) => Json.Obj(fields :+ ("description" -> Json.string(d)))
          case other            => Json.obj("description" -> Json.string(d), "allOf" -> Json.arr(other))

}
