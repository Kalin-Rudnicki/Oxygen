package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.schema.JsonSchema

/**
  * The `elicitation/create` request a server sends the client to ask the user for more input (MRTR —
  * the stateless replacement for server call-backs). Two modes, discriminated by `mode`:
  *
  *   - [[Elicitation.Params.Form]] — a restricted form of primitive fields
  *     ([[PrimitiveSchemaDefinition]]); the only mode we intend to support.
  *   - [[Elicitation.Params.Url]] — send the user out-of-band to a URL (e.g. for sensitive data).
  *     Modeled for completeness; not handled.
  *
  * Model-only: nothing dispatches or answers elicitation yet. The client's answer is an
  * [[InputResponse]].
  */
object Elicitation {

  @jsonDiscriminator("mode")
  sealed trait Params derives JsonSchema
  object Params {

    @jsonType("form")
    final case class Form(
        message: String,
        requestedSchema: Elicitation.RequestedSchema,
    ) extends Params derives JsonSchema

    @jsonType("url")
    final case class Url(
        message: String,
        url: String,
    ) extends Params derives JsonSchema

  }

  /**
    * The restricted schema a form requests — top-level primitive properties only. On the wire this is
    * a JSON-Schema object (`type: "object"`); that constant is supplied by the encoder when handling
    * is built, so it is not modeled as a field here.
    */
  final case class RequestedSchema(
      properties: Map[String, PrimitiveSchemaDefinition],
      required: Option[List[String]],
  ) derives JsonSchema

}
