package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.schema.JsonSchema

/** What a client declares it supports. */
final case class ClientCapabilities(
    experimental: Option[Map[String, Json.Obj]],
    roots: Option[Json.Obj], // deprecated as of 2026-07-28 (SEP-2577)
    sampling: Option[ClientCapabilities.Sampling], // deprecated as of 2026-07-28 (SEP-2577)
    elicitation: Option[ClientCapabilities.Elicitation],
    extensions: Option[Map[String, Json.Obj]],
) derives JsonSchema
object ClientCapabilities {

  final case class Sampling(
      context: Option[Json.Obj], // deprecated as of 2026-07-28
      tools: Option[Json.Obj],
  ) derives JsonSchema

  final case class Elicitation(
      form: Option[Json.Obj],
      url: Option[Json.Obj],
  ) derives JsonSchema

}
