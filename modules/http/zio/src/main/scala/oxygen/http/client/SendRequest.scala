package oxygen.http.client

import oxygen.http.core.BodyUtil
import zio.http.*

final case class SendRequest(
    method: Method,
    path: Path,
    queryParams: QueryParams,
    headers: Headers,
    body: Body,
)
object SendRequest {

  def simple(
      method: Method,
      path: String,
      body: String,
  ): SendRequest =
    SendRequest(
      method,
      Path(path),
      QueryParams.empty,
      Headers.empty,
      BodyUtil.fromString(body),
    )

}
