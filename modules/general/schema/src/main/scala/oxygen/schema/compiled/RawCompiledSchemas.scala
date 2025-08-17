package oxygen.schema.compiled

import oxygen.json.*
import oxygen.predef.core.*

final case class RawCompiledSchemas(
    plain: ArraySeq[RawCompiledPlainSchema],
    json: ArraySeq[RawCompiledJsonSchema],
) derives JsonCodec {

  val all: ArraySeq[RawCompiledSchema] = (plain ++ json).sortBy(_.ref: CompiledSchemaRef)

  private val plainMap: Map[CompiledSchemaRef.PlainRef, RawCompiledPlainSchema.Repr] = plain.map(s => (s.ref, s.repr)).toMap
  private val jsonMap: Map[CompiledSchemaRef.JsonRef, RawCompiledJsonSchema.Repr] = json.map(s => (s.ref, s.repr)).toMap

  // if you want to resolve anything other than PlainRef or JsonRef, you need a `FullCompiledSchema`

  def resolvePlain(ref: CompiledSchemaRef.PlainRef): RawCompiledPlainSchema.Repr =
    plainMap.getOrElse(ref, throw new RuntimeException(s"Internal Defect : Missing plain schema ref ~ ${ref.fullTypeName}"))

  def resolveJson(ref: CompiledSchemaRef.JsonRef): RawCompiledJsonSchema.Repr =
    jsonMap.getOrElse(ref, throw new RuntimeException(s"Internal Defect : Missing json schema ref ~ ${ref.fullTypeName}"))

  def withoutLineNos: RawCompiledSchemas =
    RawCompiledSchemas(
      plain.map(_.withoutLineNos),
      json.map(_.withoutLineNos),
    )

  def toFullCompiledSchemas: FullCompiledSchemas = FullCompiledSchemas(this)

}
