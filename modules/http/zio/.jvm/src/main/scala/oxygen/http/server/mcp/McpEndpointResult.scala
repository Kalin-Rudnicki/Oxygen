package oxygen.http.server.mcp

import oxygen.http.model.ServerErrors
import oxygen.json.{Json, JsonEncoder}
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

sealed trait McpEndpointResult { _self: (McpEndpointResult.Success | McpEndpointResult.Failure) & (McpEndpointResult.Basic | McpEndpointResult.NonBasic) => }
object McpEndpointResult {

  def fromMcpToolResult(id: Json, res: McpToolResult): McpEndpointResult.RespondSuccess | McpEndpointResult.RespondFailure =
    if res.isError then McpEndpointResult.RespondFailure(id, res.results)
    else McpEndpointResult.RespondSuccess(id, res.results)

  sealed trait Basic extends McpEndpointResult { _self: McpEndpointResult.Success | McpEndpointResult.Failure =>
    def json: Json
    final lazy val jsonString: String = json.showCompact
  }

  sealed trait NonBasic extends McpEndpointResult { _self: McpEndpointResult.Success | McpEndpointResult.Failure => }

  /////// Success ///////////////////////////////////////////////////////////////

  sealed trait Success extends McpEndpointResult { _self: McpEndpointResult.Basic | McpEndpointResult.NonBasic => }

  final case class RespondSuccess(id: Json, results: List[McpResponseContent]) extends McpEndpointResult.Success, McpEndpointResult.Basic {
    override def json: Json = rpcResult(id, callToolResult(McpToolResult(results, false)))
  }

  final case class InitializeResult(
      id: Json,
      protocolVersion: String,
      capabilities: InitializeResult.Capabilities,
      serverInfo: McpServer.ServerInfo,
  ) extends McpEndpointResult.Success, McpEndpointResult.Basic {

    override def json: Json =
      rpcResult(
        id,
        Json.obj(
          "protocolVersion" -> Json.string(protocolVersion),
          "capabilities" -> JsonEncoder[InitializeResult.Capabilities].encodeJsonAST(capabilities),
          "serverInfo" -> JsonEncoder[McpServer.ServerInfo].encodeJsonAST(serverInfo),
        ),
      )

  }
  object InitializeResult {

    final case class Capabilities(
        tools: Option[ToolCapability],
    ) derives JsonEncoder

    final case class ToolCapability(
        supportsListChanged: Boolean,
    ) derives JsonEncoder

  }

  final case class ToolResult(id: Json, tools: ArraySeq[AppliedMcpEndpoint]) extends McpEndpointResult.Success, McpEndpointResult.Basic {

    override def json: Json =
      rpcResult(id, Json.obj("tools" -> Json.Arr(tools.map(toolJson))))

    private def toolJson(t: AppliedMcpEndpoint): Json = {
      val fields: Growable[(String, Json)] =
        Growable.single("name" -> Json.string(t.schema.toolName)) ++
          Growable.option(t.schema.description).map(d => "description" -> Json.string(d)) ++
          Growable.single("inputSchema" -> t.schema.inputSchema)
      Json.Obj(fields.toArraySeq)
    }

  }

  case object NoContent extends McpEndpointResult.Success, McpEndpointResult.NonBasic // a notification — no response body

  /////// Failure ///////////////////////////////////////////////////////////////

  sealed trait Failure extends McpEndpointResult { _self: McpEndpointResult.Basic | McpEndpointResult.NonBasic => }

  final case class RespondFailure(id: Json, results: List[McpResponseContent]) extends McpEndpointResult.Failure, McpEndpointResult.Basic {
    override def json: Json = rpcResult(id, callToolResult(McpToolResult(results, true)))
  }

  sealed trait SpecificFailure extends McpEndpointResult.Failure, McpEndpointResult.Basic {
    val id: Json
    val code: RpcErrorCode
    val message: String
    override final def json: Json = rpcError(id, code.code, message)
  }

  final case class Unauthorized(error: McpAuthError) extends McpEndpointResult.Failure, McpEndpointResult.NonBasic {
    def message: String = error.message
  }

  final case class MissingScopes(scopes: NonEmptyList[String]) extends McpEndpointResult.Failure, McpEndpointResult.NonBasic {
    def message: String = s"insufficient_scope; missing: ${scopes.mkString(" ")}"
  }

  final case class MethodNotFound(id: Json, method: String) extends McpEndpointResult.SpecificFailure {
    override val code: RpcErrorCode = RpcErrorCode.MethodNotFound
    override val message: String = s"method not found: $method"
  }

  final case class InvalidRequest(id: Json, message: String) extends McpEndpointResult.SpecificFailure {
    override val code: RpcErrorCode = RpcErrorCode.InvalidRequest
  }

  final case class InvalidParam(id: Json, param: String, error: String) extends McpEndpointResult.SpecificFailure {
    override val code: RpcErrorCode = RpcErrorCode.InvalidParams
    override val message: String = s"error with input param '$param' : $error"
  }

  final case class InvalidTool(id: Json, toolName: String) extends McpEndpointResult.SpecificFailure {
    override val code: RpcErrorCode = RpcErrorCode.InvalidParams
    override val message: String = s"Invalid tool '$toolName'"
  }

  final case class ParseError(error: String) extends McpEndpointResult.SpecificFailure {
    override val id: Json = Json.Null
    override val code: RpcErrorCode = RpcErrorCode.ParseError
    override val message: String = s"parse error: $error"
  }

  final case class InternalError(id: Json, error: Option[ServerErrors]) extends McpEndpointResult.SpecificFailure {
    override val code: RpcErrorCode = RpcErrorCode.InternalError
    override val message: String = error match
      case Some(value) => s"internal error:\n${JsonSchema[ServerErrors].jsonEncoder.encodeJsonStringCompact(value)}"
      case None        => "internal error"
  }

  /////// Helpers ///////////////////////////////////////////////////////////////

  def rpcResult(id: Json, result: Json): Json =
    Json.obj("jsonrpc" -> Json.string("2.0"), "id" -> id, "result" -> result)

  def rpcError(id: Json, code: Int, message: String): Json =
    Json.obj("jsonrpc" -> Json.string("2.0"), "id" -> id, "error" -> Json.obj("code" -> Json.number(code), "message" -> Json.string(message)))

  private def callToolResult(r: McpToolResult): Json = {
    // `structuredContent` mirrors a tool's (success) output; on error the payload is the error value,
    // which has no output schema — emit only the text content block + isError, per the MCP spec.
    val structured: Growable[(String, Json)] = r.results match
      case _ if r.isError                                     => Growable.empty
      case McpResponseContent.JsonText(json: Json.Obj) :: Nil => Growable.single("structuredContent" -> json)
      case _                                                  => Growable.empty
    val fields: Growable[(String, Json)] =
      Growable.single("content" -> Json.Arr(r.results.toArraySeq.map(_.toJson))) ++
        structured ++
        Growable.single("isError" -> Json.boolean(r.isError))

    Json.Obj(fields.toArraySeq)
  }

}
