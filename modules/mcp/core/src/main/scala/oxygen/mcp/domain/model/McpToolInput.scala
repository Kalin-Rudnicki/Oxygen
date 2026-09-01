package oxygen.mcp.domain.model

import oxygen.mcp.api.model as API

/**
  * The full context handed to a tool handler for a `tools/call`:
  *   - `arguments` — the raw tool-call arguments object.
  *   - `meta` — the request `_meta` ([[RequestMeta]]): the negotiated `protocolVersion`, the client's
  *     declared capabilities/features (`meta.clientCapabilities`), `clientInfo`, `progressToken`, …
  *     Decode it into the domain view with [[ClientInfo.fromMeta]] when a handler wants concrete flags.
  *   - `inputResponses` / `requestState` — the MRTR carry-through (answers to a prior
  *     `input_required`, and the opaque server state to echo back).
  *   - `principal` — the authenticated caller ([[McpPrincipal]]), if the request was authenticated by
  *     the HTTP layer. The dispatcher never validates tokens; it only threads this through.
  */
final case class McpToolInput(
    arguments: API.request.ToolArguments,
    meta: API.request.RequestMeta,
    inputResponses: Option[Map[String, API.request.InputResponse]],
    requestState: Option[String],
    principal: Option[McpPrincipal],
)
