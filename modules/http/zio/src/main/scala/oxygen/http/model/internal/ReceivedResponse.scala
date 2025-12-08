package oxygen.http.model.internal

import oxygen.http.core.ReadOnlyCachedHttpBody
import zio.*
import zio.http.*

final case class ReceivedResponse(
    status: Status,
    headers: Headers,
    body: ReadOnlyCachedHttpBody,
)
object ReceivedResponse {

  def fromResponse(response: Response): UIO[ReceivedResponse] =
    ReadOnlyCachedHttpBody.wrap(response.body).map { body =>
      ReceivedResponse(
        status = response.status,
        headers = response.headers,
        body = body,
      )
    }

}
