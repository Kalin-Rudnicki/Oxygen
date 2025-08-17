package oxygen.schema

import oxygen.json.*
import oxygen.predef.core.*

enum SchemaRepr {
  case Plain(ref: TypeRef.Plain, repr: SchemaRepr.PlainRepr)
  case Json(ref: TypeRef.Json, repr: SchemaRepr.JsonRepr)

  val ref: TypeRef
  val repr: SchemaRepr.Repr

  final def toIndentedString: IndentedString =
    IndentedString.section(s"${ref.typeTag.prefixAll}:")(repr.toIndentedString)

}
object SchemaRepr {

  sealed trait Repr {

    final def toIndentedString: IndentedString =
      this match {
        case repr: PlainRepr =>
          repr match {
            case PlainString =>
              IndentedString.inline()
            case PlainEnum(values, caseSensitive) =>
              IndentedString.inline(
                IndentedString.section("values:")(values.map(v => s"- $v")),
                s"caseSensitive: $caseSensitive",
              )
          }
        case repr: JsonRepr =>
          repr match {
            case JsonOptional(underlying) =>
              IndentedString.inline(
                "[nullable]",
                IndentedString.section("underlying:")(underlying.toIndentedString),
              )
            case JsonArray(underlying) =>
              IndentedString.inline(
                "[array]",
                IndentedString.section("underlying:")(underlying.toIndentedString),
              )
            case repr: RootJsonRepr =>
              repr match {
                case JsonString(plain)           => plain.toIndentedString
                case JsonAST(Some(specificType)) => s"json-type: $specificType"
                case JsonAST(None)               => "json-type: Any"
                case JsonProduct(fields)         =>
                  IndentedString.section("fields:")(
                    fields.map { f =>
                      IndentedString.section(s"${f.name}:")(
                        f.ref.toIndentedString,
                      )
                    },
                  )
                case JsonSum(discriminator, cases) =>
                  IndentedString.inline(
                    s"discriminator: ${discriminator.fold("null")(_.unesc)}",
                    IndentedString.section("cases:")(
                      cases.map { c =>
                        IndentedString.section(s"${c.name}")(
                          c.ref.toIndentedString,
                        )
                      },
                    ),
                  )
              }
          }
      }

  }

  sealed trait PlainRepr extends Repr
  case object PlainString extends PlainRepr
  final case class PlainEnum(values: Seq[String], caseSensitive: Boolean) extends PlainRepr

  sealed trait JsonRepr extends Repr

  final case class JsonOptional(underlying: JsonRepr) extends JsonRepr
  final case class JsonArray(underlying: JsonRepr) extends JsonRepr

  sealed trait RootJsonRepr extends JsonRepr
  final case class JsonString(plain: PlainRepr) extends RootJsonRepr
  final case class JsonAST(specificType: Option[Json.Type]) extends RootJsonRepr
  final case class JsonProduct(fields: ArraySeq[JsonField]) extends RootJsonRepr
  final case class JsonSum(discriminator: Option[String], cases: ArraySeq[JsonCase]) extends RootJsonRepr

  enum JsonRef {
    case Ref(ref: TypeTag[?])
    case Option(ref: JsonRef)
    case Array(ref: JsonRef)

    final lazy val underlyingTypeTag: TypeTag[?] = this match
      case JsonRef.Ref(ref)    => ref
      case JsonRef.Option(ref) => ref.underlyingTypeTag
      case JsonRef.Array(ref)  => ref.underlyingTypeTag

    final def toIndentedString: IndentedString = this match {
      case Ref(ref) =>
        ref.prefixAll
      case Option(underlying) =>
        IndentedString.inline(
          "[nullable]",
          IndentedString.section("underlying:")(underlying.toIndentedString),
        )
      case Array(underlying) =>
        IndentedString.inline(
          "[array]",
          IndentedString.section("underlying:")(underlying.toIndentedString),
        )
    }

  }

  final case class JsonField(
      name: String,
      ref: JsonRef,
  )

  final case class JsonCase(
      name: String,
      ref: JsonRef.Ref,
  )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def dealiasPlain(repr: TemporaryRepr.Reprs, plainRef: TypeTag[?]): SchemaRepr.PlainRepr =
    convertPlain(repr, repr.plain(plainRef))

  private def convertPlain(repr: TemporaryRepr.Reprs, plainRepr: TemporaryRepr.PlainRepr): SchemaRepr.PlainRepr =
    plainRepr match {
      case TemporaryRepr.PlainString                      => SchemaRepr.PlainString
      case TemporaryRepr.PlainEnum(values, caseSensitive) => SchemaRepr.PlainEnum(values, caseSensitive)
      case TemporaryRepr.PlainTransform(plainRef)         => dealiasPlain(repr, plainRef)
    }

  private def convertJson(repr: TemporaryRepr.Reprs, jsonRef: TypeTag[?]): SchemaRepr.JsonRef =
    repr.json(jsonRef) match
      case TemporaryRepr.JsonOptional(jsonRef) => SchemaRepr.JsonRef.Option(convertJson(repr, jsonRef))
      case TemporaryRepr.JsonArray(jsonRef)    => SchemaRepr.JsonRef.Array(convertJson(repr, jsonRef))
      case _                                   => SchemaRepr.JsonRef.Ref(jsonRef)

  private def convertJson(repr: TemporaryRepr.Reprs, jsonRepr: TemporaryRepr.JsonRepr): SchemaRepr.JsonRepr =
    jsonRepr match {
      case TemporaryRepr.JsonString(plainRef)  => JsonString(dealiasPlain(repr, plainRef))
      case TemporaryRepr.JsonAST(specificType) => JsonAST(specificType)
      case TemporaryRepr.JsonOptional(jsonRef) => JsonOptional(convertJson(repr, repr.json(jsonRef)))
      case TemporaryRepr.JsonArray(jsonRef)    => JsonArray(convertJson(repr, repr.json(jsonRef)))
      case TemporaryRepr.JsonProduct(fields)   =>
        JsonProduct(fields.map { f => JsonField(f.name, convertJson(repr, f.ref)) })
      case TemporaryRepr.JsonSum(discriminator, cases) =>
        JsonSum(discriminator, cases.map { c => JsonCase(c.name, JsonRef.Ref(c.ref)) })
      case TemporaryRepr.JsonTransform(jsonRef) =>
        convertJson(repr, repr.json(jsonRef))
    }

  def from(repr: TemporaryRepr.Reprs): Seq[SchemaRepr] = {
    val fromPlain: Seq[SchemaRepr] =
      repr.plain.toSeq.map { case (typeTag, plainRepr) =>
        SchemaRepr.Plain(
          typeTag,
          convertPlain(repr, plainRepr),
        )
      }
    val fromJson: Seq[SchemaRepr] =
      repr.json.toSeq.map { case (typeTag, plainRepr) =>
        SchemaRepr.Json(
          typeTag,
          convertJson(repr, plainRepr),
        )
      }

    fromPlain ++ fromJson
  }

}
