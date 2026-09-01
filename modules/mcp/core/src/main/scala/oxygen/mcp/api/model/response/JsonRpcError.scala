package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.schema.JsonSchema

/**
  * The `error` object of a JSON-RPC 2.0 failure response. `code` is a numeric
  * [[oxygen.mcp.domain.RpcErrorCode]]; `data` carries optional structured detail (e.g. the
  * `{ supported, requested }` payload of an `UnsupportedProtocolVersion`).
  */
final case class JsonRpcError(
    code: Int,
    message: String,
    data: Option[Json],
) derives JsonSchema
