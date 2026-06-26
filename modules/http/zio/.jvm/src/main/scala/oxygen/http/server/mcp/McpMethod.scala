package oxygen.http.server.mcp

import oxygen.json.Json
import zio.*

enum McpMethod {
  case Initialize
  case ListTools
  case CallTool
  case Notifications(subMethod: String)
}
object McpMethod {
  import McpServer.{asString, field}

  def parse(id: Json, body: Json): IO[McpEndpointResult.Failure, McpMethod] =
    for {
      methodJson <- ZIO.getOrFailWith(McpEndpointResult.InvalidRequest(id, "missing `method`"))(field(body, "method"))
      methodName <- ZIO.getOrFailWith(McpEndpointResult.InvalidRequest(id, "`method` must be a string"))(asString(methodJson))
      method <- methodName match
        case "tools/call"                                 => ZIO.succeed(McpMethod.CallTool)
        case "tools/list"                                 => ZIO.succeed(McpMethod.ListTools)
        case "initialize"                                 => ZIO.succeed(McpMethod.Initialize)
        case _ if methodName.startsWith("notifications/") => ZIO.succeed(McpMethod.Notifications(methodName.stripPrefix("notifications/")))
        case _                                            => ZIO.fail(McpEndpointResult.MethodNotFound(id, methodName))
    } yield method

}
