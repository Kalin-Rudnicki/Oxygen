package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/**
  * A client's response to one server-initiated input request (MRTR — the stateless replacement for
  * server call-backs). Modeled as the surviving elicitation-result shape; the schema's `InputResponse`
  * also unions the deprecated sampling (`CreateMessageResult`) and roots (`ListRootsResult`) results.
  */
final case class InputResponse(
    action: InputResponse.Action,
    content: Option[Json.Obj], // present only when action = accept and mode = form
) derives JsonSchema
object InputResponse {
  enum Action(final val value: String) derives StrictEnum {
    case Accept extends Action("accept")
    case Decline extends Action("decline")
    case Cancel extends Action("cancel")

    override final def toString: String = value
  }
}
