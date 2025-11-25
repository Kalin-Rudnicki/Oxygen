package oxygen.schema.compiled

import oxygen.json.Json.Type as JsonType
import oxygen.predef.core.*
import oxygen.schema.NumberFormat

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      FullCompiledSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait FullCompiledSchema {

  val ref: CompiledSchemaRef

  final def toPlain: FullCompiledPlainSchema = this match
    case schema: FullCompiledPlainSchema => schema
    case schema: FullCompiledJsonSchema  => FullCompiledPlainSchema.JsonEncoded(CompiledSchemaRef.JsonEncodedText(schema.ref), Lazy(schema))

  protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString

  protected final def makeIndentedString(typeDescr: String)(elems: (String, String | IndentedString)*): IndentedString =
    IndentedString.inline(
      IndentedString.keyValuesSuffixKey(": ")(
        "type" -> typeDescr,
      ),
      IndentedString.keyValuesSuffixKey(": ")(elems*),
    )

  protected final def makeIndentedStringOpt(typeDescr: String)(elems: (String, Option[String | IndentedString])*): IndentedString =
    makeIndentedString(typeDescr)(elems.collect { case (key, Some(value)) => (key, value) }*)

  final def toIndentedString(seen: Set[CompiledSchemaRef]): IndentedString =
    if seen.contains(ref) then s"< recursive reference > ~ $ref"
    else IndentedString.section(s"$ref:")(__toIndentedStringInternal(seen + ref))

  final def toIndentedString: IndentedString =
    toIndentedString(Set.empty)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      FullCompiledPlainSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait FullCompiledPlainSchema extends FullCompiledSchema {
  override val ref: CompiledSchemaRef.PlainLike
}
object FullCompiledPlainSchema {

  final case class PlainText(
      raw: RawCompiledPlainSchema,
      rawRepr: RawCompiledPlainSchema.PlainText,
  ) extends FullCompiledPlainSchema {

    override val ref: CompiledSchemaRef.PlainLike = raw.ref

    def sourceFile: RawCompiledSchema.SourceFile = rawRepr.sourceFile

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedStringOpt("Plain Text")(
        "source-file" -> rawRepr.sourceFile.file.some,
        "source-file-line" -> rawRepr.sourceFile.lineNo.map(_.toString),
      )

  }

  final case class FormattedText(
      ref: CompiledSchemaRef.FormattedText,
      underlyingType: Lazy[FullCompiledPlainSchema],
  ) extends FullCompiledPlainSchema {

    def formats: NonEmptyList[String] = ref.formats

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Formatted Text")(
        ("formats", IndentedString.inline(formats.toList.map { f => s"- $f" }).some),
        ("underlying-type", underlyingType.value.toIndentedString(seen)),
      )

  }

  final case class Enum(
      raw: RawCompiledPlainSchema,
      rawRepr: RawCompiledPlainSchema.Enum,
  ) extends FullCompiledPlainSchema {

    override val ref: CompiledSchemaRef.PlainLike = raw.ref

    def values: Seq[String] = rawRepr.values
    def caseSensitive: Boolean = rawRepr.caseSensitive
    def exhaustive: Boolean = rawRepr.exhaustive

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Enum")(
        ("case-sensitive", rawRepr.caseSensitive.toString),
        ("exhaustive", rawRepr.exhaustive.toString),
        ("values", rawRepr.values.map { v => s"- $v" }),
      )

  }

  final case class EncodedText(
      ref: CompiledSchemaRef.EncodedText,
      underlyingType: Lazy[FullCompiledPlainSchema],
  ) extends FullCompiledPlainSchema {

    def encoding: String = ref.encoding

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Encoded Text")(
        ("encoding", encoding),
        ("underlying-type", underlyingType.value.toIndentedString(seen)),
      )

  }

  final case class JsonEncoded(
      ref: CompiledSchemaRef.JsonEncodedText,
      underlyingType: Lazy[FullCompiledJsonSchema],
  ) extends FullCompiledPlainSchema {

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Encoded Text")(
        ("underlying-type", underlyingType.value.toIndentedString(seen)),
      )

  }

  final case class BearerToken(
      ref: CompiledSchemaRef.BearerToken,
      payloadType: Lazy[FullCompiledPlainSchema],
  ) extends FullCompiledPlainSchema {

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("BearerToken")(
        "payload-type" -> payloadType.value.toIndentedString(seen),
      )

  }

  final case class Transformed(
      raw: RawCompiledPlainSchema,
      repr: RawCompiledPlainSchema.PlainTransform,
      underlyingType: Lazy[FullCompiledPlainSchema],
  ) extends FullCompiledPlainSchema {

    override val ref: CompiledSchemaRef.PlainLike = raw.ref

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedStringOpt("Transformed")(
        "source-file" -> repr.sourceFile.file.some,
        "source-file-line" -> repr.sourceFile.lineNo.map(_.toString),
        "underlying-type" -> underlyingType.value.toIndentedString(seen).some,
      )

  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      FullCompiledJsonSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait FullCompiledJsonSchema extends FullCompiledSchema {
  val isInherentlyNullable: Boolean
  override val ref: CompiledSchemaRef.JsonLike
}
object FullCompiledJsonSchema {

  final case class JsonNumber(
      raw: RawCompiledJsonSchema,
      rawRepr: RawCompiledJsonSchema.JsonNumber,
  ) extends FullCompiledJsonSchema {

    override val ref: CompiledSchemaRef.JsonLike = raw.ref

    def numberFormat: NumberFormat = rawRepr.numberFormat

    override val isInherentlyNullable: Boolean = false

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("JsonNumber")(
        "number-format" -> numberFormat.toString,
      )

  }

  final case class JsonAST(
      raw: RawCompiledJsonSchema,
      rawRepr: RawCompiledJsonSchema.JsonAST,
  ) extends FullCompiledJsonSchema {

    override val ref: CompiledSchemaRef.JsonLike = raw.ref

    def jsonType: Option[JsonType] = rawRepr.jsonType
    def jsonTypeAnyOr(tpe: JsonType): Boolean = jsonType.fold(true)(_ == tpe)

    override val isInherentlyNullable: Boolean =
      rawRepr.jsonType.isEmpty || rawRepr.jsonType.contains(JsonType.Null)

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Json AST")(
        "json-type" -> rawRepr.jsonType.fold("<any>")(_.toString),
      )

  }

  final case class JsonString(
      ref: CompiledSchemaRef.JsonString,
      elemType: Lazy[FullCompiledPlainSchema],
  ) extends FullCompiledJsonSchema {

    override val isInherentlyNullable: Boolean = false

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Json String")(
        "string-type" -> elemType.value.toIndentedString(seen),
      )

  }

  final case class JsonArray(
      ref: CompiledSchemaRef.JsonArray,
      elemType: Lazy[FullCompiledJsonSchema],
  ) extends FullCompiledJsonSchema {

    override val isInherentlyNullable: Boolean = false

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Json Array")(
        "elem-type" -> elemType.value.toIndentedString(seen),
      )

  }

  final case class JsonMap(
      ref: CompiledSchemaRef.JsonMap,
      keyType: Lazy[FullCompiledPlainSchema],
      valueType: Lazy[FullCompiledJsonSchema],
  ) extends FullCompiledJsonSchema {

    override val isInherentlyNullable: Boolean = false

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Json Array")(
        "key-type" -> keyType.value.toIndentedString(seen),
        "value-type" -> valueType.value.toIndentedString(seen),
      )

  }

  sealed trait ProductLike extends FullCompiledJsonSchema

  final case class JsonProduct(
      raw: RawCompiledJsonSchema,
      rawRepr: RawCompiledJsonSchema.JsonProduct,
      fields: ArraySeq[ProductField],
  ) extends ProductLike {

    override val isInherentlyNullable: Boolean = false

    override val ref: CompiledSchemaRef.JsonLike = raw.ref

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedString("Json Object (Product)")(
        ("fields", fields.map(_.toIndentedString(seen))),
      )

  }

  final case class JsonSum(
      raw: RawCompiledJsonSchema,
      rawRepr: RawCompiledJsonSchema.JsonSum,
      cases: ArraySeq[SumCase],
  ) extends ProductLike {

    override val isInherentlyNullable: Boolean = false

    override val ref: CompiledSchemaRef.JsonLike = raw.ref

    def discriminator: Option[String] = rawRepr.discriminator

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedStringOpt("Json Object (Product)")(
        ("discriminator", rawRepr.discriminator),
        ("cases", cases.map(_.toIndentedString(seen)).some),
      )

  }

  final case class Transformed(
      raw: RawCompiledJsonSchema,
      repr: RawCompiledJsonSchema.JsonTransform,
      underlyingType: Lazy[FullCompiledJsonSchema],
  ) extends FullCompiledJsonSchema {

    override val ref: CompiledSchemaRef.JsonLike = raw.ref

    override val isInherentlyNullable: Boolean = repr.underlyingIsNullable
    def sourceFile: RawCompiledSchema.SourceFile = repr.sourceFile

    override protected def __toIndentedStringInternal(seen: Set[CompiledSchemaRef]): IndentedString =
      makeIndentedStringOpt("Transformed")(
        "source-file" -> repr.sourceFile.file.some,
        "source-file-line" -> repr.sourceFile.lineNo.map(_.toString),
        "underlying-type" -> underlyingType.value.toIndentedString(seen).some,
      )

  }

  /////// Extras ///////////////////////////////////////////////////////////////

  final case class ProductField(
      raw: RawCompiledJsonSchema.ProductField,
      fieldType: Lazy[FullCompiledJsonSchema],
  ) {

    def fieldName: String = raw.fieldName
    def nullable: Boolean = raw.nullable
    def onMissing: Option[RawCompiledJsonSchema.ProductField.DecodeMissingAs] = raw.onMissing

    def toIndentedString(seen: Set[CompiledSchemaRef]): IndentedString =
      IndentedString.keyValuesSuffixKey(": ")(
        "- name" -> raw.fieldName,
        "  nullable" -> raw.nullable.toString,
        "  on-missing" -> raw.onMissing.fold("Error")(_.toString),
        "  type" -> fieldType.value.toIndentedString(seen),
      )

  }

  final case class SumCase(
      raw: RawCompiledJsonSchema.SumCase,
      caseType: Lazy[FullCompiledJsonSchema],
  ) {

    def caseName: String = raw.caseName

    def toIndentedString(seen: Set[CompiledSchemaRef]): IndentedString =
      IndentedString.section(s"${raw.caseName}:")(
        caseType.value.toIndentedString(seen),
      )

  }

}
