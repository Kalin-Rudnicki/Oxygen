package oxygen.schema.compiled

import oxygen.json.JsonCodec
import oxygen.predef.core.*
import scala.collection.mutable

final case class FullCompiledSchemas(rawSchemas: RawCompiledSchemas) {

  // yes, yes... we all hate mutability... there's just too much recursive referencing going on to avoid it here...
  this.mutableInternalState.init.seedRawSchemas()

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val allSchemas: ArraySeq[FullCompiledSchema] =
    ArraySeq.from(mutableInternalState.getPlainSchemas() ++ mutableInternalState.getJsonSchemas()).sortBy(_.ref)

  def resolvePlain(ref: CompiledSchemaRef.PlainLike): FullCompiledPlainSchema =
    mutableInternalState.resolvePlain(ref).value

  def resolveJson(ref: CompiledSchemaRef.JsonLike): FullCompiledJsonSchema =
    mutableInternalState.resolveJson(ref).value

  def resolve(ref: CompiledSchemaRef): FullCompiledSchema = ref match
    case ref: CompiledSchemaRef.PlainLike => resolvePlain(ref)
    case ref: CompiledSchemaRef.JsonLike  => resolveJson(ref)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private object conversion {

    def rawPlainSchema(raw: RawCompiledPlainSchema): FullCompiledPlainSchema =
      raw.repr match {
        case repr: RawCompiledPlainSchema.PlainText =>
          FullCompiledPlainSchema.PlainText(raw, repr)
        case repr: RawCompiledPlainSchema.Enum =>
          FullCompiledPlainSchema.Enum(raw, repr)
        case repr: RawCompiledPlainSchema.PlainTransform =>
          FullCompiledPlainSchema.Transformed(raw, repr, mutableInternalState.resolvePlain(repr.underlyingType))
      }

    def rawJsonSchema(raw: RawCompiledJsonSchema): FullCompiledJsonSchema =
      raw.repr match {
        case repr: RawCompiledJsonSchema.JsonNumber =>
          FullCompiledJsonSchema.JsonNumber(raw, repr)
        case repr: RawCompiledJsonSchema.JsonAST =>
          FullCompiledJsonSchema.JsonAST(raw, repr)
        case repr: RawCompiledJsonSchema.JsonProduct =>
          FullCompiledJsonSchema.JsonProduct(raw, repr, repr.fields.map { f => FullCompiledJsonSchema.ProductField(f, mutableInternalState.resolveJson(f.fieldType)) })
        case repr: RawCompiledJsonSchema.JsonSum =>
          FullCompiledJsonSchema.JsonSum(raw, repr, repr.cases.map { f => FullCompiledJsonSchema.SumCase(f, mutableInternalState.resolveJson(f.caseType)) })
        case repr: RawCompiledJsonSchema.JsonTransform =>
          FullCompiledJsonSchema.Transformed(raw, repr, mutableInternalState.resolveJson(repr.underlyingType))
      }

    def plainRef(ref: CompiledSchemaRef.PlainLike): FullCompiledPlainSchema =
      ref match {
        case _: CompiledSchemaRef.PlainRef =>
          throw new RuntimeException(s"Internal Defect : Missing schema type ~ $ref")
        case ref @ CompiledSchemaRef.EncodedText(plainRef, _) =>
          FullCompiledPlainSchema.EncodedText(ref, mutableInternalState.resolvePlain(plainRef))
        case ref @ CompiledSchemaRef.FormattedText(plainRef, _) =>
          FullCompiledPlainSchema.FormattedText(ref, mutableInternalState.resolvePlain(plainRef))
        case ref @ CompiledSchemaRef.JsonEncodedText(rawJsonRef) =>
          FullCompiledPlainSchema.JsonEncoded(ref, mutableInternalState.resolveJson(rawJsonRef))
        case ref @ CompiledSchemaRef.BearerToken(payloadType) =>
          FullCompiledPlainSchema.BearerToken(ref, mutableInternalState.resolvePlain(payloadType))
      }

    def jsonRef(ref: CompiledSchemaRef.JsonLike): FullCompiledJsonSchema =
      ref match {
        case _: CompiledSchemaRef.JsonRef =>
          throw new RuntimeException(s"Internal Defect : Missing schema type ~ $ref")
        case ref @ CompiledSchemaRef.JsonString(plainRef) =>
          FullCompiledJsonSchema.JsonString(ref, mutableInternalState.resolvePlain(plainRef))
        case CompiledSchemaRef.JsonOption(elemType) =>
          mutableInternalState.resolveJson(elemType).value // TODO (KR) : might need to keep some representation of nullable
        case CompiledSchemaRef.JsonSpecified(elemType) =>
          mutableInternalState.resolveJson(elemType).value
        case ref @ CompiledSchemaRef.JsonArray(elemType) =>
          FullCompiledJsonSchema.JsonArray(ref, mutableInternalState.resolveJson(elemType))
        case ref @ CompiledSchemaRef.JsonMap(keyType, valueType) =>
          FullCompiledJsonSchema.JsonMap(ref, mutableInternalState.resolvePlain(keyType), mutableInternalState.resolveJson(valueType))
      }

  }

  private object mutableInternalState {

    private val internalPlainMap: mutable.Map[CompiledSchemaRef.PlainLike, Lazy[FullCompiledPlainSchema]] = mutable.Map.empty
    private val internalJsonMap: mutable.Map[CompiledSchemaRef.JsonLike, Lazy[FullCompiledJsonSchema]] = mutable.Map.empty

    private object raw {

      def resolveOrCalculatePlain(ref: CompiledSchemaRef.PlainLike)(calc: => FullCompiledPlainSchema): Lazy[FullCompiledPlainSchema] =
        internalPlainMap.get(ref) match {
          case Some(value) =>
            value
          case None =>
            val res = Lazy { calc }
            internalPlainMap.update(ref, res)
            res
        }

      def resolveOrCalculateJson(ref: CompiledSchemaRef.JsonLike)(calc: => FullCompiledJsonSchema): Lazy[FullCompiledJsonSchema] =
        internalJsonMap.get(ref) match {
          case Some(value) =>
            value
          case None =>
            val res = Lazy { calc }
            internalJsonMap.update(ref, res)
            res
        }

    }

    ///////  ///////////////////////////////////////////////////////////////

    def resolvePlain(ref: CompiledSchemaRef.PlainLike): Lazy[FullCompiledPlainSchema] =
      Lazy { mutableInternalState.raw.resolveOrCalculatePlain(ref) { conversion.plainRef(ref) }.value }

    def resolveJson(ref: CompiledSchemaRef.JsonLike): Lazy[FullCompiledJsonSchema] =
      Lazy { mutableInternalState.raw.resolveOrCalculateJson(ref) { conversion.jsonRef(ref) }.value }

    def getPlainSchemas(): Iterable[FullCompiledPlainSchema] = internalPlainMap.values.map(_.value)
    def getJsonSchemas(): Iterable[FullCompiledJsonSchema] = internalJsonMap.values.map(_.value)

    object init {

      def seedRawSchemas(): Unit = {
        rawSchemas.plain.foreach { raw =>
          mutableInternalState.raw.resolveOrCalculatePlain(raw.ref) {
            conversion.rawPlainSchema(raw)
          }
        }

        rawSchemas.json.foreach { raw =>
          mutableInternalState.raw.resolveOrCalculateJson(raw.ref) {
            conversion.rawJsonSchema(raw)
          }
        }
      }

    }

  }

  def show: String = allSchemas.map(_.toIndentedString.toString("  ")).mkString("\n", "\n\n", "\n")

  override def toString: String = show

}
object FullCompiledSchemas {
  given JsonCodec[FullCompiledSchemas] = JsonCodec.deriveWrapped
}
