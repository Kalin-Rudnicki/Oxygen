package oxygen.schema.compiled

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.intermediate as I

// FIX-PRE-MERGE (KR) : move into package
sealed trait SchemaRepr {
  // TODO (KR) :
}
object SchemaRepr {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Plain
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait PlainRepr extends SchemaRepr

  case object PlainString extends PlainRepr
  final case class PlainEnum(values: Seq[String], caseSensitive: Boolean) extends PlainRepr

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Json
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait JsonRepr extends SchemaRepr

  final case class JsonString(plain: TypeRef.PlainRef) extends JsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends JsonRepr
  final case class JsonProduct(fields: ArraySeq[JsonField]) extends JsonRepr
  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[JsonCase]) extends JsonRepr

  final case class JsonField(
      name: String,
      ref: TypeRef.JsonRef, // TODO (KR) : Either[const, TypeRef.JsonRef]
  )

  final case class JsonCase(
      name: String,
      ref: TypeRef.JsonRef,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def plainTypeRef(iType: I.SimpleTypeRef.Plain, iReprs: I.Reprs): TypeRef.PlainRef = {
    val iRepr = iReprs.plain(iType)
    iRepr match {
      case I.TemporaryRepr.PlainString =>
        TypeRef.ConcretePlainRef(iType.typeTag)
      case I.TemporaryRepr.PlainEnum(_, _) =>
        TypeRef.ConcretePlainRef(iType.typeTag)
      case I.TemporaryRepr.PlainTransform(plainRef) =>
        // TODO (KR) : should this return current or underlying type ref?
        plainTypeRef(plainRef, iReprs)
    }
  }

  def jsonTypeRef(iType: I.SimpleTypeRef.Json, iReprs: I.Reprs): TypeRef.JsonRef = {
    val iRepr = iReprs.json(iType)
    iRepr match {
      case I.TemporaryRepr.JsonString(_) =>
        TypeRef.ConcreteJsonRef(iType.typeTag)
      case I.TemporaryRepr.JsonAST(_) =>
        TypeRef.ConcreteJsonRef(iType.typeTag)
      case I.TemporaryRepr.JsonOptional(jsonRef) =>
        TypeRef.jsonOption(jsonTypeRef(jsonRef, iReprs))
      case I.TemporaryRepr.JsonArray(jsonRef) =>
        TypeRef.jsonArray(jsonTypeRef(jsonRef, iReprs))
      case I.TemporaryRepr.JsonProduct(_) =>
        TypeRef.ConcreteJsonRef(iType.typeTag)
      case I.TemporaryRepr.JsonSum(_, _) =>
        TypeRef.ConcreteJsonRef(iType.typeTag)
      case I.TemporaryRepr.JsonTransform(jsonRef) =>
        // TODO (KR) : should this return current or underlying type ref?
        jsonTypeRef(jsonRef, iReprs)
    }
  }

}
