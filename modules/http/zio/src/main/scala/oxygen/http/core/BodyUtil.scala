package oxygen.http.core

import java.nio.charset.Charset
import oxygen.json.JsonEncoder
import oxygen.predef.core.*
import oxygen.schema.*
import zio.http.*

object BodyUtil {

  def fromString(text: String, contentType: MediaType = MediaType.text.plain, charset: Charset = Charsets.Http): Body =
    Body.fromArray(text.getBytes(charset)).contentType(contentType)

  def usingPlainTextSchema[A: PlainTextSchema as c](value: A): Body =
    BodyUtil.fromString(c.encode(value), contentType = MediaType.text.plain)

  def usingStringEncoder[A: StringEncoder as c](value: A): Body =
    BodyUtil.fromString(c.encode(value), contentType = MediaType.text.plain)

  def usingJsonSchema[A: JsonSchema as c](value: A): Body =
    BodyUtil.fromString(c.jsonEncoder.encodeJsonStringCompact(value), contentType = MediaType.application.json)

  def usingStringEncoder[A: JsonEncoder as c](value: A): Body =
    BodyUtil.fromString(c.encodeJsonStringCompact(value), contentType = MediaType.application.json)

}
