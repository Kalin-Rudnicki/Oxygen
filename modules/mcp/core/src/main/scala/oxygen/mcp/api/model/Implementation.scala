package oxygen.mcp.api.model

import oxygen.schema.JsonSchema

/** Identifies an MCP implementation (used as `serverInfo` / `clientInfo`). */
final case class Implementation(
    name: String,
    title: Option[String],
    version: String,
    description: Option[String],
    websiteUrl: Option[String],
    icons: Option[List[Icon]],
) derives JsonSchema
