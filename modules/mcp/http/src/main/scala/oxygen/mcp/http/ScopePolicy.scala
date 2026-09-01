package oxygen.mcp.http

/**
  * What OAuth scopes a token must hold to invoke a given tool (the `403` / `insufficient_scope` gate).
  * A trait so a server can vary requirements per tool; the common cases are the two constructors.
  */
trait ScopePolicy {
  def requiredScopes(toolName: String): Set[String]
}
object ScopePolicy {

  /** Every authed tool requires the same set (v1's server-wide model). */
  def uniform(scopes: Set[String]): ScopePolicy = _ => scopes

  /** Any valid token suffices — no scope requirement. */
  val none: ScopePolicy = _ => Set.empty

}
