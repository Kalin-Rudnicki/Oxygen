package oxygen.schema.intermediate

import oxygen.schema.*

private[schema] enum IntermediateCompiledRef {
  case Plain(ref: IntermediateTypeRef.Plain, reprs: IntermediateReprs)
  case Json(ref: IntermediateTypeRef.Json, reprs: IntermediateReprs)

  val ref: IntermediateTypeRef
  val reprs: IntermediateReprs

  private[intermediate] def withPlain(schema: PlainTextSchema[?], repr: IntermediateRepr.PlainRepr): IntermediateCompiledRef.Plain = reprs.withPlain(schema, repr)
  private[intermediate] def withJson(schema: JsonSchema[?], repr: IntermediateRepr.JsonRepr): IntermediateCompiledRef.Json = reprs.withJson(schema, repr)

}
object IntermediateCompiledRef {

  private[intermediate] def plain(schema: PlainTextSchema[?], repr: IntermediateRepr.PlainRepr, reprs: IntermediateReprs): IntermediateCompiledRef.Plain = {
    val ref: IntermediateTypeRef.Plain = IntermediateTypeRef.plain(schema)
    IntermediateCompiledRef.Plain(ref, IntermediateReprs(reprs.plain.updated(ref, repr), reprs.json))
  }

  private[intermediate] def json(schema: JsonSchema[?], repr: IntermediateRepr.JsonRepr, reprs: IntermediateReprs): IntermediateCompiledRef.Json = {
    val ref: IntermediateTypeRef.Json = IntermediateTypeRef.json(schema)
    IntermediateCompiledRef.Json(ref, IntermediateReprs(reprs.plain, reprs.json.updated(ref, repr)))
  }

  private[intermediate] def emptyPlain(schema: PlainTextSchema[?], reprs: IntermediateReprs): IntermediateCompiledRef.Plain =
    IntermediateCompiledRef.Plain(IntermediateTypeRef.plain(schema), reprs)

  private[intermediate] def emptyJson(schema: JsonSchema[?], reprs: IntermediateReprs): IntermediateCompiledRef.Json =
    IntermediateCompiledRef.Json(IntermediateTypeRef.json(schema), reprs)

  def compilePlain(schema: PlainTextSchema[?], reprs: IntermediateReprs): IntermediateCompiledRef.Plain = IntermediateRepr.compilePlain(schema, IntermediateRepr.CompileInput(reprs, Set.empty))
  def compileJson(schema: JsonSchema[?], reprs: IntermediateReprs): IntermediateCompiledRef.Json = IntermediateRepr.compileJson(schema, IntermediateRepr.CompileInput(reprs, Set.empty))
  def compile(schema: AnySchema, reprs: IntermediateReprs): IntermediateCompiledRef = schema match
    case schema: PlainTextSchema[?] => compilePlain(schema, reprs)
    case schema: JsonSchema[?]      => compileJson(schema, reprs)

}
