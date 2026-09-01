package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/**
  * The restricted subset of JSON Schema an `elicitation/create` **form** may request — only top-level,
  * non-nested primitive fields ("the 4 form ones"). MCP's `PrimitiveSchemaDefinition` is
  * `StringSchema | NumberSchema | BooleanSchema | EnumSchema`; on the wire the discriminant is `type`,
  * where `string` covers both a plain string and an enum (an enum is a string field carrying `enum`
  * (untitled) or `oneOf` (titled)), and a number field is either `number` or `integer`. We model those
  * four wire tags directly.
  *
  * This is model-only (per the 2026-07-28 port) — nothing yet *handles* elicitation, and URL-mode
  * elicitation is intentionally left as a bare passthrough ([[Elicitation.Params.Url]]).
  */
@jsonDiscriminator("type")
sealed trait PrimitiveSchemaDefinition derives JsonSchema
object PrimitiveSchemaDefinition {

  @jsonType("string")
  final case class StringField(
      title: Option[String],
      description: Option[String],
      minLength: Option[Int],
      maxLength: Option[Int],
      format: Option[StringFormat],
      `enum`: Option[List[String]], // untitled single-select
      oneOf: Option[List[EnumOption]], // titled single-select ({ const, title })
      default: Option[String],
  ) extends PrimitiveSchemaDefinition derives JsonSchema

  @jsonType("number")
  final case class NumberField(
      title: Option[String],
      description: Option[String],
      minimum: Option[Double],
      maximum: Option[Double],
      default: Option[Double],
  ) extends PrimitiveSchemaDefinition derives JsonSchema

  @jsonType("integer")
  final case class IntegerField(
      title: Option[String],
      description: Option[String],
      minimum: Option[Long],
      maximum: Option[Long],
      default: Option[Long],
  ) extends PrimitiveSchemaDefinition derives JsonSchema

  @jsonType("boolean")
  final case class BooleanField(
      title: Option[String],
      description: Option[String],
      default: Option[Boolean],
  ) extends PrimitiveSchemaDefinition derives JsonSchema

  /** One titled enum option (`{ const, title }`). */
  final case class EnumOption(const: String, title: String) derives JsonSchema

  /** The `format` hints a string field may carry. */
  enum StringFormat(final val value: String) derives StrictEnum {
    case Email extends StringFormat("email")
    case Uri extends StringFormat("uri")
    case Date extends StringFormat("date")
    case DateTime extends StringFormat("date-time")

    override final def toString: String = value
  }

}
