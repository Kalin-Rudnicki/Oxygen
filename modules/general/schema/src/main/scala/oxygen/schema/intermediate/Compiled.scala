package oxygen.schema.intermediate

import oxygen.predef.core.*

enum Compiled {
  case Plain(ref: SimpleTypeRef.Plain, reprs: Reprs)
  case Json(ref: SimpleTypeRef.Json, reprs: Reprs)

  val ref: SimpleTypeRef
  val reprs: Reprs

  def withPlain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Compiled.Plain = reprs.withPlain(typeTag, repr)
  def withJson(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Compiled.Json = reprs.withJson(typeTag, repr)

}
object Compiled {

  def plain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Compiled.Plain = {
    val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
    Compiled.Plain(ref, Reprs(Map(ref -> repr), Map.empty))
  }

  def json(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Compiled.Json = {
    val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
    Compiled.Json(ref, Reprs(Map.empty, Map(ref -> repr)))
  }

  def emptyPlain(typeTag: TypeTag[?]): Compiled.Plain =
    Compiled.Plain(SimpleTypeRef.Plain(typeTag), Reprs.empty)

  def emptyJson(typeTag: TypeTag[?]): Compiled.Json =
    Compiled.Json(SimpleTypeRef.Json(typeTag), Reprs.empty)

}
