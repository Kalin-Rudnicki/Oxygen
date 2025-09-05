package oxygen.http.model

import oxygen.http.core.StatusCodes
import oxygen.http.core.partial.ResponseCodecNoStatus
import oxygen.predef.core.*
import zio.http.*

final case class Redirect(url: URL)
object Redirect {

  def path(p: String): Redirect = Redirect(URL(Path(p)))

  given statusCodes: StatusCodes[Redirect] =
    StatusCodes.Exact(Status.PermanentRedirect)

  given responseCodec: ResponseCodecNoStatus[Redirect] =
    ResponseCodecNoStatus.header.plain
      .required[URL]("location", "url to be redirected to".some)
      .transform(Redirect(_), _.url)

}
