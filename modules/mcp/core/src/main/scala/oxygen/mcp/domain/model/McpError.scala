package oxygen.mcp.domain.model

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/**
  * A protocol-level failure raised while dispatching a request — the domain counterpart of a JSON-RPC
  * error. Each case fixes an [[RpcErrorCode]] + human message (+ optional structured `data`), and a
  * [[McpError.Kind]] the transport uses to pick an HTTP status (auth failures are surfaced by the HTTP
  * layer as `401`/`403` with a `WWW-Authenticate` challenge, not as a JSON-RPC error body — but they
  * still carry a code/message so a non-HTTP transport can render *something*).
  *
  * A derived tool's typed error `E` (and an argument-decode failure) is forced into an [[McpError]] via
  * [[McpResponseSchema.Failure]] — so a consumer's auth error can surface as [[Unauthorized]]/[[Forbidden]]
  * and become a `401`/`403` at the HTTP layer. A tool that instead wants the model to see and self-correct
  * from a failure returns a successful [[McpToolResult]] with `isError = true` (per MCP SEP-1303).
  */
sealed trait McpError {

  def code: RpcErrorCode
  def message: String
  def data: Option[Json]
  def kind: McpError.Kind

  /** Render as a JSON-RPC `2.0` error response for the given request `id`. */
  final def toResponse(id: API.RequestId): API.response.ErrorResponse =
    API.response.ErrorResponse(API.JsonRpcVersion.V2, id, API.response.JsonRpcError(code.code, message, data))

}
object McpError {

  /** How the transport should surface an error (core stays HTTP-free; the middleware maps to a status). */
  enum Kind { case Protocol, Unauthorized, Forbidden }

  private def none: Option[Json] = None

  final case class ParseError(detail: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.ParseError
    override val message: String = s"parse error: $detail"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class InvalidRequest(message: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InvalidRequest
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class MethodNotFound(method: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.MethodNotFound
    override val message: String = s"method not found: $method"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class InvalidParams(param: String, detail: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InvalidParams
    override val message: String = s"error with input param '$param' : $detail"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class InvalidTool(toolName: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InvalidParams
    override val message: String = s"invalid tool '$toolName'"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class UnsupportedProtocolVersion(requested: API.ProtocolVersion, supported: List[API.ProtocolVersion]) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.UnsupportedProtocolVersion
    override val message: String = s"unsupported protocol version: ${requested.value}"
    override val data: Option[Json] = Some(
      Json.obj(
        "requested" -> JsonSchema[API.ProtocolVersion].jsonEncoder.encodeJsonAST(requested),
        "supported" -> Json.Arr(supported.map(JsonSchema[API.ProtocolVersion].jsonEncoder.encodeJsonAST).toArraySeq),
      ),
    )
    override val kind: Kind = Kind.Protocol
  }

  final case class MissingRequiredClientCapability(capability: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.MissingRequiredClientCapability
    override val message: String = s"missing required client capability: $capability"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  final case class InternalError(detail: Option[String]) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InternalError
    override val message: String = detail.fold("internal error")(d => s"internal error: $d")
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Protocol
  }

  /** No / invalid credentials — the HTTP layer answers `401` + `WWW-Authenticate`. */
  final case class Unauthorized(message: String) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InvalidRequest
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Unauthorized
  }

  /** Authenticated but missing required scope(s) — the HTTP layer answers `403` (`insufficient_scope`). */
  final case class Forbidden(missingScopes: List[String]) extends McpError {
    override val code: RpcErrorCode = RpcErrorCode.InvalidRequest
    override val message: String = s"insufficient_scope; missing: ${missingScopes.mkString(" ")}"
    override val data: Option[Json] = none
    override val kind: Kind = Kind.Forbidden
  }

}
