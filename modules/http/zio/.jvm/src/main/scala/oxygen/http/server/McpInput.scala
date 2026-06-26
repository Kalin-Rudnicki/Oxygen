package oxygen.http.server

import oxygen.json.Json

final case class McpInput(
    id: Json,
    arguments: Json,
    auth: Option[String],
    errorConfig: ServerErrorConfig,
)
