package oxygen.schema

// FIX-PRE-MERGE (KR) :
trait CompiledSchemas {

  def schemas: Seq[CompiledSchema]

  def ref(r: SimpleTypeRef.Plain): CompiledSchema.PlainSchema
  def ref(r: SimpleTypeRef.Json): CompiledSchema.JsonSchema

  final def ref(r: SimpleTypeRef): CompiledSchema = r match
    case r: SimpleTypeRef.Plain => ref(r)
    case r: SimpleTypeRef.Json  => ref(r)

  // TODO (KR) :

}
