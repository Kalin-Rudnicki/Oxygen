package oxygen.schema.intermediate

import oxygen.predef.core.*

final case class Reprs(
    plain: Map[SimpleTypeRef.Plain, TemporaryRepr.PlainRepr],
    json: Map[SimpleTypeRef.Json, TemporaryRepr.JsonRepr],
) {

  def ++(that: Reprs): Reprs = Reprs(this.plain ++ that.plain, this.json ++ that.json)

  def withPlain(typeTag: TypeTag[?], repr: TemporaryRepr.PlainRepr): Compiled.Plain = {
    val ref: SimpleTypeRef.Plain = SimpleTypeRef.Plain(typeTag)
    Compiled.Plain(ref, Reprs(this.plain + (ref -> repr), this.json))
  }

  def withJson(typeTag: TypeTag[?], repr: TemporaryRepr.JsonRepr): Compiled.Json = {
    val ref: SimpleTypeRef.Json = SimpleTypeRef.Json(typeTag)
    Compiled.Json(ref, Reprs(this.plain, this.json + (ref -> repr)))
  }

}
object Reprs {
  val empty: Reprs = Reprs(Map.empty, Map.empty)
}
