package oxygen.mcp.domain

import oxygen.json.*
import oxygen.mcp.domain.model.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/**
  * The name-agnostic half of a derived MCP tool's per-parameter codec — the [[oxygen.mcp.generic.McpDerive]]
  * analog of oxygen-http's `PartialPathCodec` (the "partial", un-named codec you then apply a name to,
  * yielding a `RequestCodec.PathLike`). A field codec knows how to describe a single JSON field of type
  * `A` (its [[schema]] + [[required]]-ness) and how to [[decodeField decode]] a `Json` value into `A`, but
  * carries no parameter name; [[named]] pairs it with a name (+ `@mcpDoc`) to produce a full
  * [[McpParamCodec]] (an [[McpParamCodec.Field]]).
  */
sealed trait McpFieldParamCodec[A] {

  /** The JSON schema describing this field's value type — its contribution to the tool's `inputSchema`. */
  def schema: JsonSchema[A]

  /** Whether this field is required in the tool's `inputSchema` (`false` for an `Option` field). */
  def required: Boolean

  /**
    * Decode this field's JSON value (`arguments[name]`, or `Json.Null` when absent) into `A`. A decode
    * failure is a protocol [[McpError.InvalidParams]] naming the parameter (`name`).
    */
  def decodeField(name: String, value: Json): Either[McpError, A]

  /** Pair this field codec with a parameter name (+ `@mcpDoc`), producing the full named [[McpParamCodec]]. */
  final def named(name: String, doc: Option[String] = None): McpParamCodec[A] =
    McpParamCodec.Field(name, doc, this)

}
object McpFieldParamCodec extends McpFieldParamCodecLowPriority.LowPriority1 {

  /** A field codec backed by a `JsonSchema[A]` — the value is decoded straight from its JSON AST. */
  final case class JsonField[A](schema: JsonSchema[A], required: Boolean) extends McpFieldParamCodec[A] {
    override def decodeField(name: String, value: Json): Either[McpError, A] =
      schema.jsonDecoder.decodeJsonAST(value).leftMap(e => McpError.InvalidParams(name, e.toString))
  }

  /** A `JsonSchema`-backed field codec (required — an `Option[A]` field uses [[optional]] instead). */
  def json[A: JsonSchema as schema]: McpFieldParamCodec[A] =
    JsonField(schema, required = true)

  /** An escape hatch: a field codec with an explicit schema / required flag and a custom value decode. */
  def custom[A](schema: JsonSchema[A], required: Boolean)(decodeFn: Json => Either[String, A]): McpFieldParamCodec[A] = {
    val s = schema
    val r = required
    new McpFieldParamCodec[A] {
      override val schema: JsonSchema[A] = s
      override val required: Boolean = r
      override def decodeField(name: String, value: Json): Either[McpError, A] =
        decodeFn(value).leftMap(detail => McpError.InvalidParams(name, detail))
    }
  }

}

/**
  * Priority-tiered givens for [[McpFieldParamCodec]], mirroring oxygen's codec-priority pattern (e.g.
  * `ZipLowPriority`): the more specific `Option[A]`-aware instance ([[LowPriority1.optional]]) resolves
  * ABOVE the generic `Field`-from-`JsonSchema[A]` instance ([[LowPriority2.default]]), which sits at the
  * lowest priority so a custom / special instance always wins.
  */
object McpFieldParamCodecLowPriority {

  trait LowPriority1 extends LowPriority2 {

    /** An `Option[A]` field — not required, and a missing (`null`) value decodes to `None`. */
    given optional: [A] => (schema: JsonSchema[Option[A]]) => McpFieldParamCodec[Option[A]] =
      McpFieldParamCodec.JsonField(schema, required = false)

  }

  trait LowPriority2 {

    /** The default field codec for any `A` with a `JsonSchema` — what [[oxygen.mcp.generic.McpDerive]] summons. */
    given default: [A: JsonSchema as schema] => McpFieldParamCodec[A] =
      McpFieldParamCodec.JsonField(schema, required = true)

  }

}
