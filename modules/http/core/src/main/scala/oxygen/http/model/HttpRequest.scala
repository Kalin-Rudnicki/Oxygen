package oxygen.http.model

import oxygen.predef.core.*

final case class HttpRequest(
    method: HttpMethod,
    paths: ArraySeq[String],
    queryParams: QueryParams,
    headers: Headers,
    body: HttpBody,
) {

  def queryParam(key: String): List[String] = queryParams.queryParam(key)
  def header(key: String): List[String] = headers.header(key)

  def singleHeader(key: String): Option[String] = header(key).headOption

  def addHeaders(headers: Headers): HttpRequest =
    copy(headers = this.headers ++ headers)

}
