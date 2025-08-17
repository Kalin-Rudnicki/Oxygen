package oxygen.schema.compiled

import oxygen.json.*

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

  final case class JsonString(plain: PlainRepr) extends JsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends JsonRepr
  final case class JsonProduct(fields: Seq[JsonField]) extends JsonRepr
  final case class JsonSum(discriminator: Option[String], cases: Seq[JsonCase]) extends JsonRepr

  final case class JsonField(
      name: String,
      ref: TypeRef.PlainRef,
  )

  final case class JsonCase(
      name: String,
      ref: TypeRef.JsonRef,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Conversion
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) :

}
