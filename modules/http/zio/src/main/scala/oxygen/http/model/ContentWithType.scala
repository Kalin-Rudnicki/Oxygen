package oxygen.http.model

import oxygen.predef.core.*
import zio.http.MediaType

final case class ContentWithType(
    body: String,
    contentType: Option[MediaType],
)
object ContentWithType {

  def apply(body: String, contentType: Option[MediaType]): ContentWithType = new ContentWithType(body, contentType)
  def apply(body: String, contentType: MediaType): ContentWithType = new ContentWithType(body, contentType.some)
  def apply(body: String): ContentWithType = new ContentWithType(body, None)

}

final case class ByteContentWithType(
    body: Array[Byte],
    contentType: Option[MediaType],
)
object ByteContentWithType {

  def apply(body: Array[Byte], contentType: Option[MediaType]): ByteContentWithType = new ByteContentWithType(body, contentType)
  def apply(body: Array[Byte], contentType: MediaType): ByteContentWithType = new ByteContentWithType(body, contentType.some)
  def apply(body: Array[Byte]): ByteContentWithType = new ByteContentWithType(body, None)

}
