package oxygen.schema.compat

import oxygen.schema.compiled.*

final class CompatibilityChecker(source: FullCompiledSchemas, target: FullCompiledSchemas) {

  def diffSourceAndTarget(sourceRef: CompiledSchemaRef, targetRef: CompiledSchemaRef): Seq[CompatibilityChecker.Difference] =
    ??? // FIX-PRE-MERGE (KR) :

}
object CompatibilityChecker {

  // these are changes in the "required-ness" of a field, this is technically different than a change in nullability
  // a change in nullability is a change in "type" of the field

  // [=] {ifMissing:None}            -> {ifMissing:None}               ~ same
  // [=] {ifMissing:Some(Undefined)} -> {ifMissing:Some(Undefined)}    ~ same
  // [=] {ifMissing:Some(Null)}      -> {ifMissing:Some(Null)}         ~ same

  // [ ] {ifMissing:None}            -> {ifMissing:Some(Undefined)}    ~ field went from `required` to optional (but not necessarily nullable)
  // [ ] {ifMissing:None}            -> {ifMissing:Some(Null)}         ~ field went from `required` to optional (missing = null)

  // [ ] {ifMissing:Some(Undefined)} -> {ifMissing:None}               ~ field went from
  // [ ] {ifMissing:Some(Undefined)} -> {ifMissing:Some(Null)}

  // [ ] {ifMissing:Some(Null)}      -> {ifMissing:None}
  // [ ] {ifMissing:Some(Null)}      -> {ifMissing:Some(Undefined)}

  //

  // - add new field
  // - remove existing field
  // - modify existing field
  // - add new sum case
  // - remove existing sum case
  // - modify existing sum case

  final case class Difference(
      scope: List[String],
  )

  // FIX-PRE-MERGE (KR) :

}
