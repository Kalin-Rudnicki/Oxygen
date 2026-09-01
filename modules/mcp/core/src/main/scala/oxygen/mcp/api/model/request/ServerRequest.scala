package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.mcp.api.model.{JsonRpcVersion, RequestId}
import oxygen.schema.JsonSchema

/**
  * A JSON-RPC client -> server request (MCP `2026-07-28`), scoped to init + tools. The full envelope —
  * `jsonrpc` (always `"2.0"`), `id` (JSON-RPC `string | number`, modeled as raw [[oxygen.json.Json]]),
  * `method` (the discriminator), and the nested method-specific `params`. Every `params` carries the
  * **required** `_meta` envelope ([[RequestMeta]]); `tools/call` also carries the MRTR
  * `inputResponses` / `requestState`.
  */
@jsonDiscriminator("method")
sealed trait ServerRequest derives JsonSchema {
  def jsonrpc: JsonRpcVersion
  def id: RequestId
}
object ServerRequest {

  @jsonType("server/discover")
  final case class ServerDiscover(jsonrpc: JsonRpcVersion, id: RequestId, params: ServerDiscover.Params) extends ServerRequest derives JsonSchema
  object ServerDiscover {
    final case class Params(
        @jsonField("_meta") meta: RequestMeta,
    ) derives JsonSchema
  }

  @jsonType("tools/list")
  final case class ToolsList(jsonrpc: JsonRpcVersion, id: RequestId, params: ToolsList.Params) extends ServerRequest derives JsonSchema
  object ToolsList {
    final case class Params(
        cursor: Option[String],
        @jsonField("_meta") meta: RequestMeta,
    ) derives JsonSchema
  }

  @jsonType("tools/call")
  final case class ToolsCall(jsonrpc: JsonRpcVersion, id: RequestId, params: ToolsCall.Params) extends ServerRequest derives JsonSchema
  object ToolsCall {
    final case class Params(
        name: String,
        arguments: Option[ToolArguments],
        inputResponses: Option[Map[String, InputResponse]],
        requestState: Option[String],
        @jsonField("_meta") meta: RequestMeta,
    ) derives JsonSchema
  }

}
