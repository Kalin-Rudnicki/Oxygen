package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.mcp.api.model.Icon
import oxygen.schema.JsonSchema

/** Definition of a tool the client can call (from `tools/list`). */
final case class Tool(
    name: String,
    title: Option[String],
    description: Option[String],
    inputSchema: ToolInputSchema,
    outputSchema: Option[Json.Obj],
    annotations: Option[ToolAnnotations],
    icons: Option[List[Icon]],
) derives JsonSchema
