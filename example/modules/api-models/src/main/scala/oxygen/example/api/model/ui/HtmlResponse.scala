package oxygen.example.api.model.ui

import oxygen.http.core.*
import oxygen.http.model.RawResponseText
import oxygen.predef.core.*
import zio.http.{Headers, MediaType, Status}

final case class HtmlResponse(response: String)
object HtmlResponse {

  given ResponseCodec[HtmlResponse] =
    ResponseCodec[RawResponseText].transform(
      raw => HtmlResponse(raw.body),
      response =>
        RawResponseText(
          Status.Ok,
          Headers(
            ("Cache-Control", "no-store, no-cache, must-revalidate"),
            ("Pragma", "no-cache"),
            ("Expires", "0"),
          ),
          response.response,
          MediaType.text.html.some,
        ),
    )

}
