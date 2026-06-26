package oxygen.http.server.mcp

// JSON-RPC 2.0 error codes
enum RpcErrorCode(final val code: Int) {
  case ParseError extends RpcErrorCode(-32700)
  case InvalidRequest extends RpcErrorCode(-32600)
  case MethodNotFound extends RpcErrorCode(-32601)
  case InvalidParams extends RpcErrorCode(-32602)
  case InternalError extends RpcErrorCode(-32603)
  // case Custom(c: Int) extends RpcErrorCode(c)
}
