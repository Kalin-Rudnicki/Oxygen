package oxygen.schema.intermediate

import oxygen.predef.core.*

enum Generated {
  case Plain(ref: SimpleTypeRef.Plain, reprs: Reprs)
  case Json(ref: SimpleTypeRef.Json, reprs: Reprs)

  val ref: SimpleTypeRef
  val reprs: Reprs

  def withPlain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Generated.Plain = reprs.withPlain(typeTag, repr)
  def withJson(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Generated.Json = reprs.withJson(typeTag, repr)

}
object Generated {

  def plain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Generated.Plain = {
    val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
    Generated.Plain(ref, Reprs(Map(ref -> repr), Map.empty))
  }

  def json(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Generated.Json = {
    val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
    Generated.Json(ref, Reprs(Map.empty, Map(ref -> repr)))
  }

  def emptyPlain(typeTag: TypeTag[?]): Generated.Plain =
    Generated.Plain(SimpleTypeRef.Plain(typeTag), Reprs.empty)

  def emptyJson(typeTag: TypeTag[?]): Generated.Json =
    Generated.Json(SimpleTypeRef.Json(typeTag), Reprs.empty)

}
