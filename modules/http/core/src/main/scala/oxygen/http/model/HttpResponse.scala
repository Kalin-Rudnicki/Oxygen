package oxygen.http.model

final case class HttpResponse(
    statusCode: HttpCode,
    statusText: String,
    headers: Headers,
    body: HttpBody,
) {

  def header(key: String): List[String] = headers.header(key)

}
object HttpResponse {

  def apply(statusCode: HttpCode, headers: Headers, body: HttpBody): HttpResponse =
    HttpResponse(statusCode, statusCode.name, headers, body)

  def apply(statusCode: HttpCode): HttpResponse =
    HttpResponse(statusCode, statusCode.name, Headers.empty, HttpBody.plain(statusCode.name))

  def apply(statusCode: HttpCode, body: String): HttpResponse =
    HttpResponse(statusCode, statusCode.name, Headers.empty, HttpBody.plain(body))

}
