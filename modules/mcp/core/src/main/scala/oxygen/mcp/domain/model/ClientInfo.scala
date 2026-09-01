package oxygen.mcp.domain.model

import oxygen.mcp.api.model as API

/**
  * A decoded, domain-level view of the client behind a request — parsed from the wire `_meta`
  * ([[RequestMeta]]). The api-layer capability bags are loosely-typed presence flags
  * (`Option[Json.Obj]`); here they are fully decoded into concrete booleans — an absent capability
  * becomes an all-`false` struct, so a handler never hunts through optionals.
  */
final case class ClientInfo(
    protocolVersion: API.ProtocolVersion,
    implementation: Option[API.Implementation], // self-reported identity (name/version); unverified, genuinely absent-able
    capabilities: ClientInfo.Capabilities,
)
object ClientInfo {

  /** What the client declared it supports — every field concrete, absence decoded to `false` / empty. */
  final case class Capabilities(
      sampling: Sampling, // borrow the client's LLM (deprecated 2026-07-28)
      elicitation: Elicitation, // ask the user (the surviving MRTR mechanism)
      roots: Boolean, // client can expose filesystem roots (deprecated)
      experimental: Set[String], // declared non-standard capability keys
      extensions: Set[String], // declared MCP extension ids (e.g. "io.modelcontextprotocol/tasks")
  )

  final case class Sampling(
      tools: Boolean, // supports tool use during sampling
      context: Boolean, // supports context inclusion (deprecated)
  )

  final case class Elicitation(
      form: Boolean, // supports form mode (structured input against a schema)
      url: Boolean, // supports url mode (out-of-band)
  )

  /** Decode a request's `_meta` ([[RequestMeta]]) into the domain client view. */
  def fromMeta(meta: API.request.RequestMeta): ClientInfo =
    ClientInfo(
      protocolVersion = meta.protocolVersion,
      implementation = meta.clientInfo,
      capabilities = fromCapabilities(meta.clientCapabilities),
    )

  /** Decode the loosely-typed capability bags into concrete flags (absent -> all `false`). */
  def fromCapabilities(caps: API.request.ClientCapabilities): Capabilities =
    Capabilities(
      sampling = caps.sampling.fold(Sampling(false, false))(s => Sampling(tools = s.tools.isDefined, context = s.context.isDefined)),
      elicitation = caps.elicitation.fold(Elicitation(false, false))(e => Elicitation(form = e.form.isDefined, url = e.url.isDefined)),
      roots = caps.roots.isDefined,
      experimental = caps.experimental.fold(Set.empty[String])(_.keySet),
      extensions = caps.extensions.fold(Set.empty[String])(_.keySet),
    )

}
