package oxygen.mcp.api.model.response

import oxygen.predef.core.*

/** Who a piece of content is intended for. */
enum Role(final val value: String) derives StrictEnum {
  case User extends Role("user")
  case Assistant extends Role("assistant")

  override final def toString: String = value
}

/** Cacheability of a result (from `CacheableResult`). */
enum CacheScope(final val value: String) derives StrictEnum {
  case Public extends CacheScope("public")
  case Private extends CacheScope("private")

  override final def toString: String = value
}
