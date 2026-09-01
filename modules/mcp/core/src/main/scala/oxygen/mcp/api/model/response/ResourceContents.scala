package oxygen.mcp.api.model.response

import oxygen.schema.JsonSchema

/**
  * The contents of a resource — an untagged union on the wire (distinguished by the presence of
  * `text` vs `blob`); modeled here as a sum for schema purposes.
  */
sealed trait ResourceContents derives JsonSchema {
  val uri: String
  val mimeType: Option[String]
}
object ResourceContents {
  final case class Text(uri: String, mimeType: Option[String], text: String) extends ResourceContents derives JsonSchema
  final case class Blob(uri: String, mimeType: Option[String], blob: String) extends ResourceContents derives JsonSchema
}
