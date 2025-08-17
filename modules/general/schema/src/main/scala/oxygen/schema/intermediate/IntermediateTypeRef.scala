package oxygen.schema.intermediate

import oxygen.core.TypeTag
import oxygen.schema.{JsonSchema, PlainTextSchema, SchemaLike}

private[schema] enum IntermediateTypeRef {

  case Plain(typeTag: TypeTag[?], reference: SchemaLike.ReferenceName)
  case Json(typeTag: TypeTag[?], reference: SchemaLike.ReferenceName)

  val typeTag: TypeTag[?]
  val reference: SchemaLike.ReferenceName

  final def fullNameWithKind: String = s"$productPrefix ~ ${typeTag.prefixAll}"

  override final def toString: String = fullNameWithKind

}
object IntermediateTypeRef {

  def plain(schema: PlainTextSchema[?]): IntermediateTypeRef.Plain = IntermediateTypeRef.Plain(schema.typeTag, schema.referenceName)
  def json(schema: JsonSchema[?]): IntermediateTypeRef.Json = IntermediateTypeRef.Json(schema.typeTag, schema.referenceName)

  given Ordering[IntermediateTypeRef] =
    Ordering
      .by[IntermediateTypeRef, String](_.typeTag.prefixObjectNoGenerics)
      .orElseBy(_.typeTag.prefixObject)
      .orElseBy(_.typeTag.prefixAll)
      .orElseBy(_.reference)

}
