package oxygen.mcp.domain

import oxygen.json.*
import oxygen.mcp.api.model as API
import oxygen.mcp.domain.model.*

/**
  * The full (named / positional) codec for a single parameter of a derived MCP tool — the
  * [[oxygen.mcp.generic.McpDerive]] analog of oxygen-http's applied `RequestCodec.PathLike` (the codec you
  * get once a name has been paired with a name-agnostic partial). A parameter is either:
  *
  *   - model-supplied — a named [[Field]], produced by [[McpFieldParamCodec.named]]: it decodes from the
  *     `tools/call` `arguments` and contributes an [[inputParam]] to the tool's `inputSchema`; or
  *   - injected — supplied from the [[McpToolInput]] rather than the arguments, and excluded from the
  *     input schema (`inputParam = None`): the authenticated caller as any type with a
  *     [[McpPrincipalDecoder]] ([[principal]] / [[optionalPrincipal]]), or the whole [[toolInput]].
  */
sealed trait McpParamCodec[A] {

  /** The parameter's contribution to the tool's `inputSchema`, or `None` when injected (not decoded). */
  def inputParam: Option[API.response.ToolInputSchema.Param]

  /** Decode this parameter from the call `arguments` and/or the surrounding [[McpToolInput]]. */
  def decode(args: Json, input: McpToolInput): Either[McpError, A]

}
object McpParamCodec {

  /** A model-supplied parameter: a name-agnostic [[McpFieldParamCodec]] paired with a name (+ `@mcpDoc`). */
  final case class Field[A](name: String, doc: Option[String], field: McpFieldParamCodec[A]) extends McpParamCodec[A] {

    override val inputParam: Option[API.response.ToolInputSchema.Param] =
      Some(API.response.ToolInputSchema.Param(name, field.schema, field.required, doc))

    override def decode(args: Json, input: McpToolInput): Either[McpError, A] =
      field.decodeField(name, argValue(args, name))

  }

  /** An injected parameter — supplied from the [[McpToolInput]], contributing nothing to the input schema. */
  private final class Injected[A](decodeFn: McpToolInput => Either[McpError, A]) extends McpParamCodec[A] {
    override val inputParam: Option[API.response.ToolInputSchema.Param] = None
    override def decode(args: Json, input: McpToolInput): Either[McpError, A] = decodeFn(input)
  }

  /**
    * The authenticated caller as `A`, injected from [[McpToolInput.principal]] through its
    * [[McpPrincipalDecoder]] — requires auth (`401` if absent).
    */
  def principal[A](using decoder: McpPrincipalDecoder[A]): McpParamCodec[A] =
    new Injected[A](_.principal.toRight(McpError.Unauthorized("authentication required")).flatMap(decoder.decode))

  /**
    * The authenticated caller as `A` if present, injected from [[McpToolInput.principal]] through its
    * [[McpPrincipalDecoder]] — does not require auth (`None` when unauthenticated).
    */
  def optionalPrincipal[A](using decoder: McpPrincipalDecoder[A]): McpParamCodec[Option[A]] =
    new Injected[Option[A]](input =>
      input.principal match {
        case None    => Right(None)
        case Some(p) => decoder.decode(p).map(Some(_))
      },
    )

  /** The whole [[McpToolInput]], injected for handlers that need the raw call context. */
  val toolInput: McpParamCodec[McpToolInput] =
    new Injected[McpToolInput](Right(_))

  /** Pull a named field from the args object, defaulting to `null` when absent (so `Option` -> `None`). */
  private def argValue(args: Json, name: String): Json =
    args match {
      case Json.Obj(fields) => fields.collectFirst { case (k, v) if k == name => v }.getOrElse(Json.Null)
      case _                => Json.Null
    }

}
