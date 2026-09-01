package oxygen.mcp.api.model

import oxygen.predef.core.*

/**
  * The JSON-RPC methods of the MCP `2026-07-28` protocol — models only (no wire parsing / codecs).
  * Each leaf carries its canonical `method` name exactly as it appears on the wire.
  *
  * Split by the JSON-RPC distinction: a [[McpMethod.Request]] expects a response, a
  * [[McpMethod.Notification]] never does.
  */
sealed abstract class McpMethod(final val method: String) derives StrictEnum {
  override final def toString: String = method
}
object McpMethod {

  sealed abstract class Request(method: String) extends McpMethod(method) derives StrictEnum
  sealed abstract class ServerRequest(method: String) extends Request(method) derives StrictEnum
  sealed abstract class ClientRequest(method: String) extends Request(method) derives StrictEnum

  sealed abstract class Notification(method: String) extends McpMethod(method) derives StrictEnum

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Requests — client -> server
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object ServerDiscover extends ServerRequest("server/discover")
  case object ToolsList extends ServerRequest("tools/list")
  case object ToolsCall extends ServerRequest("tools/call")
  case object ResourcesList extends ServerRequest("resources/list")
  case object ResourcesTemplatesList extends ServerRequest("resources/templates/list")
  case object ResourcesRead extends ServerRequest("resources/read")
  case object SubscriptionsListen extends ServerRequest("subscriptions/listen")
  case object PromptsList extends ServerRequest("prompts/list")
  case object PromptsGet extends ServerRequest("prompts/get")
  case object CompletionComplete extends ServerRequest("completion/complete")

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Requests — server -> client
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object SamplingCreateMessage extends ClientRequest("sampling/createMessage")
  case object RootsList extends ClientRequest("roots/list")
  case object ElicitationCreate extends ClientRequest("elicitation/create")

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Notifications
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  case object Cancelled extends Notification("notifications/cancelled")
  case object Progress extends Notification("notifications/progress")
  case object Message extends Notification("notifications/message")
  case object ResourcesUpdated extends Notification("notifications/resources/updated")
  case object ResourcesListChanged extends Notification("notifications/resources/list_changed")
  case object PromptsListChanged extends Notification("notifications/prompts/list_changed")
  case object ToolsListChanged extends Notification("notifications/tools/list_changed")
  case object SubscriptionsAcknowledged extends Notification("notifications/subscriptions/acknowledged")

}
