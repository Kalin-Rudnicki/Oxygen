package oxygen.mcp.api.model.response

import oxygen.schema.JsonSchema

/** Optional hints attached to content / resources. */
final case class Annotations(
    audience: Option[List[Role]],
    priority: Option[Double],
    lastModified: Option[String],
) derives JsonSchema
