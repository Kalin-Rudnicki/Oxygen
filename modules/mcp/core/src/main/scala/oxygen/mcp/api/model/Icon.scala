package oxygen.mcp.api.model

import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/** A display icon (branding / UI). */
final case class Icon(
    src: String,
    mimeType: Option[String],
    sizes: Option[List[String]],
    theme: Option[Icon.Theme],
) derives JsonSchema
object Icon {
  enum Theme(final val value: String) derives StrictEnum {
    case Light extends Theme("light")
    case Dark extends Theme("dark")

    override final def toString: String = value
  }
}
