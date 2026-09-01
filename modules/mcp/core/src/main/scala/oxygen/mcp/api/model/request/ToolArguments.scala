package oxygen.mcp.api.model.request

import oxygen.json.*
import oxygen.schema.JsonSchema

/**
  * The `tools/call` `arguments` object (MCP `2026-07-28`) — the model-supplied, free-form argument bag
  * validated against a tool's [[ToolInputSchema]]. Opaque (`<: Json.Obj`) so it can't be confused with
  * any other json object, yet still usable as a [[oxygen.json.Json.Obj]] without unwrapping.
  */
opaque type ToolArguments <: Json.Obj = Json.Obj
object ToolArguments {

  def wrap(json: Json.Obj): ToolArguments = json

  given JsonSchema[ToolArguments] = JsonSchema.jsonObject.transform(wrap, x => x)

}
