package oxygen.mcp.api.model.response

import oxygen.schema.JsonSchema

/** Behavior hints for a [[Tool]] — hints only, never trusted for security decisions. */
final case class ToolAnnotations(
    title: Option[String],
    readOnlyHint: Option[Boolean],
    destructiveHint: Option[Boolean],
    idempotentHint: Option[Boolean],
    openWorldHint: Option[Boolean],
) derives JsonSchema
