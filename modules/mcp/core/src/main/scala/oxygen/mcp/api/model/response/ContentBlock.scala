package oxygen.mcp.api.model.response

import oxygen.json.*
import oxygen.mcp.api.model.Icon
import oxygen.schema.JsonSchema

/** A block of unstructured tool-call / prompt content. Discriminated by `type`. */
@jsonDiscriminator("type")
sealed trait ContentBlock derives JsonSchema
object ContentBlock {

  @jsonType("text")
  final case class Text(text: String, annotations: Option[Annotations]) extends ContentBlock derives JsonSchema

  @jsonType("image")
  final case class Image(data: String, mimeType: String, annotations: Option[Annotations]) extends ContentBlock derives JsonSchema

  @jsonType("audio")
  final case class Audio(data: String, mimeType: String, annotations: Option[Annotations]) extends ContentBlock derives JsonSchema

  @jsonType("resource_link")
  final case class ResourceLink(
      uri: String,
      name: String,
      title: Option[String],
      description: Option[String],
      mimeType: Option[String],
      annotations: Option[Annotations],
      size: Option[Long],
      icons: Option[List[Icon]],
  ) extends ContentBlock derives JsonSchema

  @jsonType("resource")
  final case class EmbeddedResource(resource: ResourceContents, annotations: Option[Annotations]) extends ContentBlock derives JsonSchema

}
