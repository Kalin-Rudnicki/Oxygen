package oxygen.mcp.domain

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*
import oxygen.schema.JsonSchema

/**
  * The wire boundary between raw JSON-RPC text and the typed [[ServerRequest]] / [[ServerResponse]]
  * models — the `mcp-2026-07-28` counterpart of v1's `McpRequestCodec` / `McpResponseCodec`, but
  * pure and transport-neutral (it takes/returns strings + [[oxygen.json.Json]], no HTTP).
  */
object McpCodec {

  private val knownMethods: Set[String] = Set("server/discover", "tools/list", "tools/call")

  /**
    * Decode a request body into a [[ServerRequest]], classifying failures the way v1 did:
    *   - unparseable JSON            -> [[McpError.ParseError]]
    *   - a recognizable-but-unknown `method` -> [[McpError.MethodNotFound]]
    *   - otherwise a bad shape       -> [[McpError.InvalidRequest]]
    */
  def decodeRequest(body: String): Either[McpError, API.request.ServerRequest] =
    JsonSchema[API.request.ServerRequest].jsonCodec.decoder.decodeJsonString(body) match {
      case Right(req) => Right(req)
      case Left(err)  =>
        Json.parse(body) match {
          case Left(parseErr) => Left(McpError.ParseError(parseErr.getMessage))
          case Right(json)    =>
            methodOf(json) match {
              case Some(method) if !knownMethods.contains(method) => Left(McpError.MethodNotFound(method))
              case _                                              => Left(McpError.InvalidRequest(err.toString))
            }
        }
    }

  /** Render a dispatch outcome into a full JSON-RPC `2.0` response object for request `id`. */
  def renderResponse(id: API.RequestId, outcome: Either[McpError, API.response.ServerResponse]): Json =
    outcome match {
      case Right(sr)   => encode(API.response.SuccessResponse(API.JsonRpcVersion.V2, id, sr))
      case Left(error) => encode(error.toResponse(id))
    }

  /** Best-effort `id` recovery from a raw body — used to echo the id on a pre-dispatch decode failure. */
  def recoverId(body: String): API.RequestId =
    API.RequestId.wrap(Json.parse(body).toOption.flatMap(field(_, "id")).getOrElse(Json.Null))

  private def field(json: Json, name: String): Option[Json] =
    json match {
      case Json.Obj(fields) => fields.collectFirst { case (`name`, v) => v }
      case _                => None
    }

  private def methodOf(json: Json): Option[String] =
    field(json, "method").collect { case Json.Str(s) => s }

  private def encode[A](a: A)(using schema: JsonSchema[A]): Json =
    schema.jsonEncoder.encodeJsonAST(a)

}
