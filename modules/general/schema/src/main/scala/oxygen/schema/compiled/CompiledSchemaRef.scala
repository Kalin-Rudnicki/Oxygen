package oxygen.schema.compiled

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.intermediate as I
import scala.annotation.tailrec

sealed trait CompiledSchemaRef derives JsonCodec {

  @tailrec
  final def primaryReference: CompiledSchemaRef.RootReference = this match
    case ref: CompiledSchemaRef.RootReference      => ref
    case CompiledSchemaRef.JsonString(ref)         => ref.primaryReference
    case CompiledSchemaRef.JsonEncodedText(ref)    => ref.primaryReference
    case CompiledSchemaRef.EncodedText(ref, _)     => ref.primaryReference
    case CompiledSchemaRef.FormattedText(ref, _)   => ref.primaryReference
    case CompiledSchemaRef.JWT(payloadType)        => payloadType.primaryReference
    case CompiledSchemaRef.JsonOption(elemType)    => elemType.primaryReference
    case CompiledSchemaRef.JsonSpecified(elemType) => elemType.primaryReference
    case CompiledSchemaRef.JsonArray(elemType)     => elemType.primaryReference
    case CompiledSchemaRef.JsonMap(_, valueType)   => valueType.primaryReference

  final lazy val showBase: String = this match
    case CompiledSchemaRef.PlainRef(tpe)               => tpe.fullTypeName
    case CompiledSchemaRef.JsonRef(tpe)                => tpe.fullTypeName
    case CompiledSchemaRef.EncodedText(ref, encoding)  => s"EncodedText<$encoding>(${ref.showBase})"
    case CompiledSchemaRef.FormattedText(ref, formats) => s"FormattedText<${formats.mkString("|")}>(${ref.showBase})"
    case CompiledSchemaRef.JsonString(ref)             => s"Json.PlainText(${ref.showBase})"
    case CompiledSchemaRef.JsonEncodedText(ref)        => s"PlainText.JsonEncoded(${ref.showBase})"
    case CompiledSchemaRef.JWT(payloadType)            => s"JWT<${payloadType.showBase}>"
    case CompiledSchemaRef.JsonArray(elemType)         => s"Array<${elemType.showBase}>"
    case CompiledSchemaRef.JsonMap(keyType, valueType) => s"Map<key=${keyType.showBase},value=${valueType.showBase}>"
    case CompiledSchemaRef.JsonOption(elemType)        => s"Option<${elemType.showBase}>"
    case CompiledSchemaRef.JsonSpecified(elemType)     => s"Specified<${elemType.showBase}>"

  final lazy val coreType: String = this match
    case _: CompiledSchemaRef.PlainLike => "Plain Text"
    case _: CompiledSchemaRef.JsonLike  => "JSON"

  final lazy val showCore: String =
    s"$coreType ~ $showBase"

  def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef

  override final def toString: String = showCore

}
object CompiledSchemaRef {

  sealed trait RootReference extends CompiledSchemaRef {
    val typeIdentifier: TypeIdentifier.ByName
    final def fullTypeName: String = typeIdentifier.fullTypeName
  }

  /////// Plain ///////////////////////////////////////////////////////////////

  @jsonDiscriminator("refType")
  sealed trait PlainLike extends CompiledSchemaRef derives JsonCodec {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.PlainLike
  }

  final case class PlainRef(typeIdentifier: TypeIdentifier.ByName) extends PlainLike, RootReference {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.PlainRef = PlainRef(f(typeIdentifier))
  }
  object PlainRef {
    given JsonCodec[PlainRef] = JsonCodec.deriveWrapped
  }

  final case class JsonEncodedText(ref: JsonLike) extends PlainLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonEncodedText = JsonEncodedText(ref.mapTypeIdentifier(f))
  }

  final case class EncodedText(ref: PlainLike, encoding: String) extends PlainLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.EncodedText = EncodedText(ref.mapTypeIdentifier(f), encoding)
  }

  final case class FormattedText(ref: PlainLike, formats: NonEmptyList[String]) extends PlainLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.FormattedText = FormattedText(ref.mapTypeIdentifier(f), formats)
  }

  final case class JWT(payloadType: JsonLike) extends PlainLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JWT = JWT(payloadType.mapTypeIdentifier(f))
  }

  private[compiled] def resolvePlain(ref: I.IntermediateTypeRef.Plain, reprs: I.IntermediateReprs): CompiledSchemaRef.PlainLike =
    RawCompiledPlainSchema.convertRepr(ref, None, reprs).fold(identity, tup => CompiledSchemaRef.PlainRef(tup._1.byName))

  /////// Json ///////////////////////////////////////////////////////////////

  @jsonDiscriminator("refType")
  sealed trait JsonLike extends CompiledSchemaRef derives JsonCodec {

    final lazy val isInherentlyNullable: Boolean = this match
      case _: JsonOption => true
      case _             => false

    @tailrec
    final def toConcrete: CompiledSchemaRef.JsonConcrete = this match
      case concrete: JsonConcrete  => concrete
      case JsonOption(elemType)    => elemType.toConcrete
      case JsonSpecified(elemType) => elemType.toConcrete

    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonLike

  }

  @jsonDiscriminator("refType")
  sealed trait JsonConcrete extends JsonLike derives JsonCodec {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonConcrete
  }

  final case class JsonRef(typeIdentifier: TypeIdentifier.ByName) extends JsonConcrete, RootReference {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonRef = JsonRef(f(typeIdentifier))
  }
  object JsonRef {
    given JsonCodec[JsonRef] = JsonCodec.deriveWrapped
  }

  final case class JsonString(ref: PlainLike) extends JsonConcrete {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonString = JsonString(ref.mapTypeIdentifier(f))
  }
  final case class JsonArray(elemType: JsonLike) extends JsonConcrete {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonArray = JsonArray(elemType.mapTypeIdentifier(f))
  }
  final case class JsonMap(keyType: PlainLike, valueType: JsonLike) extends JsonConcrete {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonMap = JsonMap(keyType.mapTypeIdentifier(f), valueType.mapTypeIdentifier(f))
  }

  final case class JsonOption(elemType: JsonLike) extends JsonLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonOption = JsonOption(elemType.mapTypeIdentifier(f))
  }
  final case class JsonSpecified(elemType: JsonLike) extends JsonLike {
    override def mapTypeIdentifier(f: TypeIdentifier.MapFunction): CompiledSchemaRef.JsonSpecified = JsonSpecified(elemType.mapTypeIdentifier(f))
  }

  private[compiled] def resolveJson(ref: I.IntermediateTypeRef.Json, reprs: I.IntermediateReprs): CompiledSchemaRef.JsonLike =
    RawCompiledJsonSchema.convertRepr(ref, None, reprs).fold(identity, tup => CompiledSchemaRef.JsonRef(tup._1.byName))

  private[compiled] def resolveJsonConcrete(
      r: I.IntermediateTypeRef.Json,
      reprs: I.IntermediateReprs,
  ): (CompiledSchemaRef.JsonConcrete, Boolean, Option[RawCompiledJsonSchema.ProductField.DecodeMissingAs]) = {
    def rec(
        r: I.IntermediateTypeRef.Json,
    ): (CompiledSchemaRef.JsonLike, Boolean, Option[RawCompiledJsonSchema.ProductField.DecodeMissingAs]) =
      reprs.getJson(r) match {
        case _: I.IntermediateRepr.JsonString                        => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case _: I.IntermediateRepr.JsonArray                         => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case _: I.IntermediateRepr.JsonMap                           => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case _: I.IntermediateRepr.JsonProduct                       => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case _: I.IntermediateRepr.JsonSum                           => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case I.IntermediateRepr.JsonAST(Some(Json.Type.Null) | None) => (CompiledSchemaRef.resolveJson(r, reprs), true, None)
        case I.IntermediateRepr.JsonAST(_)                           => (CompiledSchemaRef.resolveJson(r, reprs), false, None)
        case I.IntermediateRepr.JsonOption(elemType)                 =>
          val (resolvedUnderlying, _, _) = rec(elemType)
          (resolvedUnderlying, true, RawCompiledJsonSchema.ProductField.DecodeMissingAs.Null.some)
        case I.IntermediateRepr.JsonSpecified(elemType) =>
          val (resolvedUnderlying, underlyingNullable, _) = rec(elemType)
          (resolvedUnderlying, underlyingNullable, RawCompiledJsonSchema.ProductField.DecodeMissingAs.Undefined.some)
        case I.IntermediateRepr.JsonTransform(jsonRef, _) =>
          val (_, underlyingNullable, underlyingIfMissing) = rec(jsonRef)
          (CompiledSchemaRef.resolveJson(r, reprs), underlyingNullable, underlyingIfMissing)
      }

    val (a, b, c) = rec(r)
    (a.toConcrete, b, c)
  }

  given Ordering[CompiledSchemaRef] =
    Ordering
      .by[CompiledSchemaRef, Int] {
        case _: PlainLike => 1
        case _: JsonLike  => 2
      }
      .orElseBy(_.primaryReference.fullTypeName)

}
