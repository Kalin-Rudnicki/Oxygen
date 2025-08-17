package oxygen.schema

// FIX-PRE-MERGE (KR) : move into package
enum CompiledSchema {
  case PlainSchema(ref: TypeRef.ConcretePlainRef, repr: CompiledSchemaRepr.PlainRepr)
  case JsonSchema(ref: TypeRef.ConcreteJsonRef, repr: CompiledSchemaRepr.JsonRepr)

  val ref: TypeRef.Concrete
  val repr: CompiledSchemaRepr
}
object CompiledSchema {

  given Ordering[CompiledSchema] =
    Ordering
      .by[CompiledSchema, Int] {
        case _: CompiledSchema.JsonSchema  => 1
        case _: CompiledSchema.PlainSchema => 2
      }
      .orElseBy(_.ref.ref.prefixObject)

}
