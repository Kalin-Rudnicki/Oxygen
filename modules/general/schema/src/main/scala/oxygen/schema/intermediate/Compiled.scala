package oxygen.schema.intermediate

import oxygen.predef.core.*
import oxygen.schema.*

enum Compiled {
  case Plain(ref: SimpleTypeRef.Plain, reprs: Reprs)
  case Json(ref: SimpleTypeRef.Json, reprs: Reprs)

  val ref: SimpleTypeRef
  val reprs: Reprs

  private[intermediate] def withPlain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Compiled.Plain = reprs.withPlain(typeTag, repr)
  private[intermediate] def withJson(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Compiled.Json = reprs.withJson(typeTag, repr)

}
object Compiled {

  private[intermediate] def plain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Compiled.Plain = {
    val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
    Compiled.Plain(ref, Reprs(Map(ref -> repr), Map.empty))
  }

  private[intermediate] def json(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Compiled.Json = {
    val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
    Compiled.Json(ref, Reprs(Map.empty, Map(ref -> repr)))
  }

  private[intermediate] def emptyPlain(typeTag: TypeTag[?]): Compiled.Plain =
    Compiled.Plain(SimpleTypeRef.Plain(typeTag), Reprs.empty)

  private[intermediate] def emptyJson(typeTag: TypeTag[?]): Compiled.Json =
    Compiled.Json(SimpleTypeRef.Json(typeTag), Reprs.empty)

  // FIX-PRE-MERGE (KR) : figure out if this belongs somewhere else
  def compile(schema: AnySchema, reprs: Reprs): Compiled = {
    println(s"\n--- compile: ${schema.typeTag.prefixAll} ---")
    schema match
      case schema: PlainTextSchema[?] => TemporaryRepr.compilePlain(schema, TemporaryRepr.CompileInput(reprs, Set.empty))
      case schema: JsonSchema[?]      => TemporaryRepr.compileJson(schema, TemporaryRepr.CompileInput(reprs, Set.empty))
  }

}
