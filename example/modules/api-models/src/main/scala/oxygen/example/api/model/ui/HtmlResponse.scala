package oxygen.example.api.model.ui

import oxygen.http.core.partial.ResponseCodecNoStatus

final case class HtmlResponse(response: String)
object HtmlResponse {

  given ResponseCodecNoStatus[HtmlResponse] =
    ResponseCodecNoStatus.fromBody[String].transform(HtmlResponse(_), _.response) ++
      ResponseCodecNoStatus.SetHeader("Cache-Control", "no-store, no-cache, must-revalidate") ++
      ResponseCodecNoStatus.SetHeader("Pragma", "no-cache") ++
      ResponseCodecNoStatus.SetHeader("Expires", "0")

}
