package oxygen.mcp.domain

import oxygen.predef.core.*

/**
  * The api-erased applied form of a tool registry — every [[McpTool]] already bound to its API impl,
  * which is what a [[McpServer]] dispatches over. Mirrors `oxygen-http`'s `AppliedEndpoints`, and is
  * produced from a [[McpTools]] by providing the API implementations to [[McpTools.toLayer]].
  */
final case class AppliedMcpTools(tools: Growable[AppliedMcpTool]) {

  lazy val arraySeq: ArraySeq[AppliedMcpTool] = tools.toArraySeq
  def ++(that: AppliedMcpTools): AppliedMcpTools = AppliedMcpTools(this.tools ++ that.tools)

}
object AppliedMcpTools {

  val empty: AppliedMcpTools = AppliedMcpTools(Growable.empty)

  def flatten(ts: AppliedMcpTools*): AppliedMcpTools =
    AppliedMcpTools(Growable.many(ts).flatMap(_.tools))

}
