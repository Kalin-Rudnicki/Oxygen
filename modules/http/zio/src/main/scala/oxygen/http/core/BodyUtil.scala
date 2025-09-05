package oxygen.http.core

import java.nio.charset.Charset
import zio.http.*

object BodyUtil {

  def fromString(text: String, contentType: MediaType = MediaType.text.plain, charset: Charset = Charsets.Http): Body =
    Body.fromArray(text.getBytes(charset)).contentType(contentType)

}
