package oxygen.schema.compiled

// FIX-PRE-MERGE (KR) : move into package
enum Schema {
  case PlainSchema(ref: TypeRef.ConcretePlainRef, repr: SchemaRepr.PlainRepr)
  case JsonSchema(ref: TypeRef.ConcreteJsonRef, repr: SchemaRepr.JsonRepr)

  val ref: TypeRef.Concrete
  val repr: SchemaRepr
}
object Schema {

  given Ordering[Schema] =
    Ordering
      .by[Schema, Int] {
        case _: Schema.JsonSchema  => 1
        case _: Schema.PlainSchema => 2
      }
      .orElseBy(_.ref.ref.prefixObject)

}
