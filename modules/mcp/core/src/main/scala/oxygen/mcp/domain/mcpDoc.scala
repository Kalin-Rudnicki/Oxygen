package oxygen.mcp.domain

import oxygen.meta.*
import scala.annotation.Annotation

/**
  * Documentation for a derived MCP tool ([[oxygen.mcp.generic.McpDerive]]), mirroring oxygen-http's
  * `httpDoc`:
  *   - on a tool method — becomes the tool's `description`.
  *   - on a tool parameter — becomes that parameter's `description` in the tool's `inputSchema`.
  *
  * `derives FromExprT` so the derivation macro can read the annotation's value at compile time via
  * `symbol.annotations.optionalOfValue[mcpDoc]`.
  */
final case class mcpDoc(doc: String) extends Annotation derives FromExprT
