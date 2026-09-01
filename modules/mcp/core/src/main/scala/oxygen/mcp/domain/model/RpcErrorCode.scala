package oxygen.mcp.domain.model

/**
  * JSON-RPC 2.0 + MCP error codes.
  *
  *   - `-32700`..`-32603` are the standard JSON-RPC codes.
  *   - `-32020`..`-32099` are reserved by MCP for protocol-level errors (allocated sequentially from
  *     `-32020`); we model the three defined as of `2026-07-28`.
  *   - `-32000`..`-32019` stay implementation-defined and are not enumerated here.
  *
  * The numeric [[code]] is what goes on the wire ([[oxygen.mcp.api.model.response.JsonRpcError.code]]).
  */
enum RpcErrorCode(final val code: Int) {

  // JSON-RPC 2.0 (standard)
  case ParseError extends RpcErrorCode(-32700)
  case InvalidRequest extends RpcErrorCode(-32600)
  case MethodNotFound extends RpcErrorCode(-32601)
  case InvalidParams extends RpcErrorCode(-32602)
  case InternalError extends RpcErrorCode(-32603)

  // MCP protocol-level (2026-07-28)
  case HeaderMismatch extends RpcErrorCode(-32020)
  case MissingRequiredClientCapability extends RpcErrorCode(-32021)
  case UnsupportedProtocolVersion extends RpcErrorCode(-32022)

}
