package oxygen.schema.compiled

import oxygen.core.SourcePosition
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.intermediate as I

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      RawCompiledSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed trait RawCompiledSchema {

  val fullTypeName: TypeIdentifier.ByName
  val typeIdentifier: TypeIdentifier.Full
  val ref: CompiledSchemaRef.RootReference

  def mapTypeIdentifier(f: TypeIdentifier.MapFunction): RawCompiledSchema
  def withoutLineNos: RawCompiledSchema

  def toIndentedString: IndentedString

}
object RawCompiledSchema {

  final case class SourceFile(file: String, lineNo: Option[Int]) derives JsonCodec {
    def withoutLineNo: SourceFile = SourceFile(file, None)
  }
  object SourceFile {
    def fromSourcePosition(pos: SourcePosition): SourceFile = SourceFile(pos.estimatedScalaFile, pos.startLine.some)
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      RawCompiledPlainSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class RawCompiledPlainSchema(
    fullTypeName: TypeIdentifier.ByName,
    typeIdentifier: TypeIdentifier.Full,
    repr: RawCompiledPlainSchema.Repr,
) extends RawCompiledSchema derives JsonCodec {

  override val ref: CompiledSchemaRef.PlainRef = CompiledSchemaRef.PlainRef(fullTypeName)

  def mapTypeIdentifier(f: TypeIdentifier.MapFunction): RawCompiledPlainSchema =
    RawCompiledPlainSchema(
      fullTypeName = f(fullTypeName),
      typeIdentifier = f(typeIdentifier),
      repr = repr.mapTypeIdentifier(f),
    )

  override def toIndentedString: IndentedString =
    repr match {
      case RawCompiledPlainSchema.PlainText(_) =>
        s"[${ref.fullTypeName}]: <PlainString>"
      case RawCompiledPlainSchema.Enum(values, caseSensitive, exhaustive) =>
        IndentedString.keyValueSectionSuffixKey(s"[${ref.fullTypeName}]: <Enum>", ": ")(
          "values" -> values,
          "case-sensitive" -> caseSensitive.toString,
          "exhaustive" -> exhaustive.toString,
        )
      case RawCompiledPlainSchema.PlainTransform(plainRef, _) =>
        s"[${ref.fullTypeName}]: <PlainTransform> - $plainRef"
    }

  def withoutLineNos: RawCompiledPlainSchema =
    copy(
      repr = repr match {
        case rt: RawCompiledPlainSchema.PlainText      => rt.copy(sourceFile = rt.sourceFile.withoutLineNo)
        case e: RawCompiledPlainSchema.Enum            => e
        case pt: RawCompiledPlainSchema.PlainTransform => pt.copy(sourceFile = pt.sourceFile.withoutLineNo)
      },
    )

}
object RawCompiledPlainSchema {

  private[compiled] def from(
      ref: I.IntermediateTypeRef.Plain,
      repr: I.IntermediateRepr.PlainRepr,
      reprs: I.IntermediateReprs,
  ): Either[CompiledSchemaRef.PlainLike, RawCompiledPlainSchema] =
    convertRepr(ref, repr.some, reprs).map { case (typeIdentifier, repr) => RawCompiledPlainSchema(typeIdentifier.byName, typeIdentifier, repr.value) }

  private[compiled] def convertRepr(
      ref: I.IntermediateTypeRef.Plain,
      optRepr: Option[I.IntermediateRepr.PlainRepr],
      reprs: I.IntermediateReprs,
  ): Either[CompiledSchemaRef.PlainLike, (TypeIdentifier.Full, Lazy[RawCompiledPlainSchema.Repr])] = {
    def typeIdentifier: TypeIdentifier.Full = reprs.typeIdentifier(ref)

    val repr: I.IntermediateRepr.PlainRepr = optRepr.getOrElse(reprs.getPlain(ref))

    repr match {
      case I.IntermediateRepr.PlainText(sourceFile) =>
        (typeIdentifier, Lazy(RawCompiledPlainSchema.PlainText(RawCompiledSchema.SourceFile.fromSourcePosition(sourceFile)))).asRight
      case I.IntermediateRepr.PlainTransform(plainRef, sourceFile) =>
        (typeIdentifier, Lazy(RawCompiledPlainSchema.PlainTransform(CompiledSchemaRef.resolvePlain(plainRef, reprs), RawCompiledSchema.SourceFile.fromSourcePosition(sourceFile)))).asRight
      case I.IntermediateRepr.Enum(values, caseSensitive, exhaustive) => (typeIdentifier, Lazy(RawCompiledPlainSchema.Enum(values, caseSensitive, exhaustive))).asRight
      case I.IntermediateRepr.BearerToken(plainRef)                   => CompiledSchemaRef.BearerToken(CompiledSchemaRef.resolvePlain(plainRef, reprs)).asLeft
      case I.IntermediateRepr.EncodedText(plainRef, encoding)         => CompiledSchemaRef.EncodedText(CompiledSchemaRef.resolvePlain(plainRef, reprs), encoding).asLeft
      case I.IntermediateRepr.FormattedText(plainRef, formats)        => CompiledSchemaRef.FormattedText(CompiledSchemaRef.resolvePlain(plainRef, reprs), formats).asLeft
      case I.IntermediateRepr.JsonEncodedText(jsonRef)                => CompiledSchemaRef.JsonEncodedText(CompiledSchemaRef.resolveJson(jsonRef, reprs)).asLeft
    }
  }

  @jsonDiscriminator("reprType")
  sealed trait Repr derives JsonCodec {
    def mapTypeIdentifier(f: TypeIdentifier.MapFunction): Repr
  }

  final case class PlainText(sourceFile: RawCompiledSchema.SourceFile) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): PlainText = this
  }

  final case class Enum(values: Seq[String], caseSensitive: Boolean, exhaustive: Boolean) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): Enum = this
  }

  final case class PlainTransform(underlyingType: CompiledSchemaRef.PlainLike, sourceFile: RawCompiledSchema.SourceFile) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): PlainTransform = PlainTransform(underlyingType.mapTypeIdentifier(f), sourceFile)
  }

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      RawCompiledJsonSchema
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class RawCompiledJsonSchema(
    fullTypeName: TypeIdentifier.ByName,
    typeIdentifier: TypeIdentifier.Full,
    repr: RawCompiledJsonSchema.Repr,
) extends RawCompiledSchema derives JsonCodec {

  override val ref: CompiledSchemaRef.JsonRef = CompiledSchemaRef.JsonRef(fullTypeName)

  override def toIndentedString: IndentedString =
    repr match {
      case RawCompiledJsonSchema.JsonAST(specificType) =>
        IndentedString.section(s"[${ref.fullTypeName}]: <JsonAST>")(
          s"type: ${specificType.fold("<any>")(_.toString)}",
        )
      case RawCompiledJsonSchema.JsonProduct(fields) =>
        IndentedString.section(s"[${ref.fullTypeName}]: <JsonProduct>")(
          IndentedString.section("fields:")(fields.map(f => s"${f.fieldName}: ${f.fieldType}")),
        )
      case RawCompiledJsonSchema.JsonSum(discriminator, cases) =>
        IndentedString.section(s"[${ref.fullTypeName}]: <JsonSum>")(
          discriminator.map { d => s"discriminator: $d" },
          IndentedString.section("cases:")(cases.map(c => s"${c.caseName}: ${c.caseType}")),
        )
      case RawCompiledJsonSchema.JsonTransform(jsonRef, false, _) =>
        s"[${ref.fullTypeName}]: <JsonTransform> - $jsonRef"
      case RawCompiledJsonSchema.JsonTransform(jsonRef, true, _) =>
        s"[${ref.fullTypeName}]: <JsonTransform : nullable> - $jsonRef"
    }

  def mapTypeIdentifier(f: TypeIdentifier.MapFunction): RawCompiledJsonSchema =
    RawCompiledJsonSchema(
      fullTypeName = f(fullTypeName),
      typeIdentifier = f(typeIdentifier),
      repr = repr.mapTypeIdentifier(f),
    )

  def withoutLineNos: RawCompiledJsonSchema =
    copy(
      repr = repr match {
        case ja: RawCompiledJsonSchema.JsonAST       => ja
        case jp: RawCompiledJsonSchema.JsonProduct   => jp
        case js: RawCompiledJsonSchema.JsonSum       => js
        case jt: RawCompiledJsonSchema.JsonTransform => jt.copy(sourceFile = jt.sourceFile.withoutLineNo)
      },
    )

}
object RawCompiledJsonSchema {

  private[compiled] def from(
      ref: I.IntermediateTypeRef.Json,
      repr: I.IntermediateRepr.JsonRepr,
      reprs: I.IntermediateReprs,
  ): Either[CompiledSchemaRef.JsonLike, RawCompiledJsonSchema] =
    convertRepr(ref, repr.some, reprs).map { case (typeIdentifier, repr) => RawCompiledJsonSchema(typeIdentifier.byName, typeIdentifier, repr.value) }

  private[compiled] def convertRepr(
      ref: I.IntermediateTypeRef.Json,
      optRepr: Option[I.IntermediateRepr.JsonRepr],
      reprs: I.IntermediateReprs,
  ): Either[CompiledSchemaRef.JsonLike, (TypeIdentifier.Full, Lazy[RawCompiledJsonSchema.Repr])] = {
    def typeIdentifier: TypeIdentifier.Full = reprs.typeIdentifier(ref)

    val repr: I.IntermediateRepr.JsonRepr = optRepr.getOrElse(reprs.getJson(ref))

    repr match {
      case I.IntermediateRepr.JsonString(plainRef)               => CompiledSchemaRef.JsonString(CompiledSchemaRef.resolvePlain(plainRef, reprs)).asLeft
      case I.IntermediateRepr.JsonAST(specificType)              => (typeIdentifier, Lazy(RawCompiledJsonSchema.JsonAST(specificType))).asRight
      case I.IntermediateRepr.JsonOption(jsonRef)                => CompiledSchemaRef.JsonOption(CompiledSchemaRef.resolveJson(jsonRef, reprs)).asLeft
      case I.IntermediateRepr.JsonSpecified(jsonRef)             => CompiledSchemaRef.JsonSpecified(CompiledSchemaRef.resolveJson(jsonRef, reprs)).asLeft
      case I.IntermediateRepr.JsonArray(jsonRef)                 => CompiledSchemaRef.JsonArray(CompiledSchemaRef.resolveJson(jsonRef, reprs)).asLeft
      case I.IntermediateRepr.JsonMap(keyPlainRef, valueJsonRef) =>
        CompiledSchemaRef.JsonMap(CompiledSchemaRef.resolvePlain(keyPlainRef, reprs), CompiledSchemaRef.resolveJson(valueJsonRef, reprs)).asLeft

      case I.IntermediateRepr.JsonProduct(fields) =>
        def makeField(f: I.IntermediateRepr.JsonField, reprs: I.IntermediateReprs): RawCompiledJsonSchema.ProductField = {
          val (underlyingType, nullable, ifMissing) = CompiledSchemaRef.resolveJsonConcrete(f.ref, reprs)
          ProductField(f.name, nullable, ifMissing, underlyingType)
        }

        (typeIdentifier, Lazy(JsonProduct(fields.map(makeField(_, reprs))))).asRight
      case I.IntermediateRepr.JsonSum(discriminator, cases) =>
        (typeIdentifier, Lazy(JsonSum(discriminator, cases.map(c => SumCase(c.name, CompiledSchemaRef.resolveJson(c.ref, reprs)))))).asRight
      case I.IntermediateRepr.JsonTransform(jsonRef, sourceFile) =>
        def transform: RawCompiledJsonSchema.JsonTransform = {
          val (resolvedUnderlying, underlyingNullable, _) = CompiledSchemaRef.resolveJsonConcrete(jsonRef, reprs)
          RawCompiledJsonSchema.JsonTransform(resolvedUnderlying, underlyingNullable, RawCompiledSchema.SourceFile.fromSourcePosition(sourceFile))
        }

        (typeIdentifier, Lazy { transform }).asRight
    }
  }

  @jsonDiscriminator("reprType")
  sealed trait Repr derives JsonCodec {
    def mapTypeIdentifier(f: TypeIdentifier.MapFunction): Repr
  }

  final case class JsonAST(jsonType: Option[oxygen.json.Json.Type]) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): JsonAST = this
  }

  final case class JsonProduct(fields: ArraySeq[ProductField]) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): JsonProduct = JsonProduct(fields.map(_.mapTypeIdentifier(f)))
  }

  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[SumCase]) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): JsonSum = JsonSum(discriminator, cases.map(_.mapTypeIdentifier(f)))
  }

  final case class JsonTransform(underlyingType: CompiledSchemaRef.JsonConcrete, underlyingIsNullable: Boolean, sourceFile: RawCompiledSchema.SourceFile) extends Repr {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): JsonTransform = JsonTransform(underlyingType.mapTypeIdentifier(f), underlyingIsNullable, sourceFile)
  }

  final case class ProductField(
      fieldName: String,
      nullable: Boolean,
      onMissing: Option[ProductField.DecodeMissingAs],
      fieldType: CompiledSchemaRef.JsonConcrete,
  ) derives JsonCodec {

    def mapTypeIdentifier(f: TypeIdentifier.MapFunction): ProductField =
      ProductField(
        fieldName = fieldName,
        nullable = nullable,
        onMissing = onMissing,
        fieldType = fieldType.mapTypeIdentifier(f),
      )

  }
  object ProductField {

    enum DecodeMissingAs derives StrictEnum {
      case Undefined
      case Null
    }

  }

  final case class SumCase(
      caseName: String,
      caseType: CompiledSchemaRef.JsonLike,
  ) derives JsonCodec {

    def mapTypeIdentifier(f: TypeIdentifier.MapFunction): SumCase =
      SumCase(
        caseName = caseName,
        caseType = caseType.mapTypeIdentifier(f),
      )

  }

}
