package oxygen.mcp.domain

/**
  * Typeclass witnessing that the tools of an API trait `Api` can be derived — one [[McpTool]] per
  * abstract method (see `oxygen.mcp.generic.McpDerive`). Mirrors `oxygen-http`'s `DeriveEndpoints`:
  * a user writes `trait MyApi derives DeriveMcp`, then registers it with `McpTools.empty.add[MyApi]`.
  */
trait DeriveMcp[Api] {
  def tools: McpTools[Api]
}
object DeriveMcp {
  inline def derived[Api]: DeriveMcp[Api] = ${ oxygen.mcp.generic.McpDerive.derivedImpl[Api] }
}
