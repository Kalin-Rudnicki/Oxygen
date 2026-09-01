package oxygen.mcp.domain

import oxygen.predef.core.*
import zio.*

/**
  * An accumulating registry of the tools a server hosts, parameterized by the set of APIs `Apis` whose
  * implementations still need to be provided. Mirrors `oxygen-http`'s `Endpoints[Apis]`: bind APIs by
  * type ([[add]]), then materialize the api-erased [[AppliedMcpTools]] by providing those API impls to
  * [[toLayer]]:
  *
  * {{{
  * val tools: McpTools[MyApi & BillingApi] =
  *   McpTools.empty.add[MyApi].add[BillingApi]
  *
  * val layer: URLayer[MyApi & BillingApi, AppliedMcpTools] = tools.toLayer
  * }}}
  */
final class McpTools[Apis] private[domain] (private val tools: Growable[McpTools.Tagged[? >: Apis]]) {

  /** Derive + register the tools of `Api` ([[DeriveMcp]]), leaving its impl to be provided later. */
  def add[Api](using derive: DeriveMcp[Api]): McpTools[Apis & Api] = this ++ derive.tools

  def ++[Apis2](that: McpTools[Apis2]): McpTools[Apis & Apis2] = McpTools(this.tools ++ that.tools)

  /** Provide the `Apis` implementations to bind every registered tool, yielding [[AppliedMcpTools]]. */
  def toLayer: URLayer[Apis, AppliedMcpTools] =
    ZLayer.fromZIO { ZIO.environment[Apis].map(env => AppliedMcpTools(this.tools.map(_(env)))) }

}
object McpTools {

  val empty: McpTools[Any] = McpTools[Any](Growable.empty)

  def flatten[Apis](ts: McpTools[? >: Apis]*): McpTools[Apis] = McpTools(Growable.many(ts).flatMap(_.tools))

  final case class Tagged[Api](tag: Tag[Api], tool: McpTool[Api]) {
    def apply(env: ZEnvironment[? <: Api]): AppliedMcpTool = tool.apply(env.get[Api](using tag))
  }

  def of[Api](tag: Tag[Api])(tools: Growable[McpTool[Api]]): McpTools[Api] = new McpTools[Api](tools.map(Tagged(tag, _)))

}
