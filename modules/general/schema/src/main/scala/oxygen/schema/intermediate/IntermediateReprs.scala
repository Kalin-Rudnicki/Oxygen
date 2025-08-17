package oxygen.schema.intermediate

import oxygen.predef.core.*
import oxygen.schema.{JsonSchema, PlainTextSchema, SchemaLike}
import oxygen.schema.compiled.{SchemaType, TypeIdentifier}

private[schema] final case class IntermediateReprs(
    plain: Map[IntermediateTypeRef.Plain, IntermediateRepr.PlainRepr],
    json: Map[IntermediateTypeRef.Json, IntermediateRepr.JsonRepr],
) {

  def ++(that: IntermediateReprs): IntermediateReprs = IntermediateReprs(this.plain ++ that.plain, this.json ++ that.json)

  def withPlain(schema: PlainTextSchema[?], repr: IntermediateRepr.PlainRepr): IntermediateCompiledRef.Plain = {
    val ref: IntermediateTypeRef.Plain = IntermediateTypeRef.plain(schema)
    IntermediateCompiledRef.Plain(ref, IntermediateReprs(this.plain + (ref -> repr), this.json))
  }

  def withJson(schema: JsonSchema[?], repr: IntermediateRepr.JsonRepr): IntermediateCompiledRef.Json = {
    val ref: IntermediateTypeRef.Json = IntermediateTypeRef.json(schema)
    IntermediateCompiledRef.Json(ref, IntermediateReprs(this.plain, this.json + (ref -> repr)))
  }

  def getPlain(ref: IntermediateTypeRef.Plain): IntermediateRepr.PlainRepr =
    plain.getOrElse(ref, throw new RuntimeException(s"Internal Defect : missing plain type ref ~ ${ref.typeTag.prefixAll}"))

  def getJson(ref: IntermediateTypeRef.Json): IntermediateRepr.JsonRepr =
    json.getOrElse(ref, throw new RuntimeException(s"Internal Defect : missing json type ref ~ ${ref.typeTag.prefixAll}"))

  private lazy val typeIdentifiers: Map[SchemaLike.ReferenceName, TypeIdentifier.Full] = {
    val allRefs: List[IntermediateTypeRef] = (plain.iterator ++ json.iterator).filterNot(_._2.ignoreWhenComputingDistinctTypes).map(_._1).toList
    val grouped: Seq[(SchemaType, NonEmptyList[SchemaLike.ReferenceName])] =
      NonEmptyList.fromList(allRefs) match
        case Some(allRefs) => allRefs.map(r => (SchemaType.fromTypeRef(r.typeTag.tag), r.reference)).groupMap(_._1)(_._2).toSeq
        case None          => Nil

    grouped.flatMap {
      case (schemaType, NonEmptyList(iRef, Nil)) => (iRef, TypeIdentifier.Full(schemaType, None)) :: Nil
      case (schemaType, iRefs)                   => iRefs.toList.sorted.zipWithIndex.map { case (iRef, idx) => (iRef, TypeIdentifier.Full(schemaType, (idx + 1).some)) }
    }.toMap
  }

  def typeIdentifier(ref: IntermediateTypeRef): TypeIdentifier.Full =
    typeIdentifiers.getOrElse(ref.reference, throw new RuntimeException(s"Internal Defect : missing type ref ~ $ref"))

}
object IntermediateReprs {
  val empty: IntermediateReprs = IntermediateReprs(Map.empty, Map.empty)
}
