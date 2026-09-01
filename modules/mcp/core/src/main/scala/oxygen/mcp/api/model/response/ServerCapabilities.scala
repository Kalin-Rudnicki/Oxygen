package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.schema.JsonSchema

/** What a server declares it supports. */
final case class ServerCapabilities(
    experimental: Option[Map[String, Json.Obj]],
    logging: Option[Json.Obj], // deprecated as of 2026-07-28 (SEP-2577)
    completions: Option[Json.Obj],
    prompts: Option[ServerCapabilities.Prompts],
    resources: Option[ServerCapabilities.Resources],
    tools: Option[ServerCapabilities.Tools],
    extensions: Option[Map[String, Json.Obj]],
) derives JsonSchema
object ServerCapabilities {
  final case class Prompts(listChanged: Option[Boolean]) derives JsonSchema
  final case class Resources(subscribe: Option[Boolean], listChanged: Option[Boolean]) derives JsonSchema
  final case class Tools(listChanged: Option[Boolean]) derives JsonSchema
}
