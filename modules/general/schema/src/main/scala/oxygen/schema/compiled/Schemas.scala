package oxygen.schema.compiled

import oxygen.schema.intermediate as I

// FIX-PRE-MERGE (KR) :
trait Schemas {

  def schemas: Seq[Schema]

  def ref(r: I.SimpleTypeRef.Plain): Schema.PlainSchema
  def ref(r: I.SimpleTypeRef.Json): Schema.JsonSchema

  final def ref(r: I.SimpleTypeRef): Schema = r match
    case r: I.SimpleTypeRef.Plain => ref(r)
    case r: I.SimpleTypeRef.Json  => ref(r)

  // TODO (KR) :

}
