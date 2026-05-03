package oxygen.ui.web

import org.scalajs.dom.window
import zio.*
import zio.http.{Path, QueryParams}
import zio.http.URL

// TODO (KR) : have a way to represent absolute URLs as well
final case class PageURL(
    path: Path,
    queryParams: QueryParams,
) {

  def formatted: String = URL(path = path.addLeadingSlash, queryParams = queryParams).encode

  def addPrefix(prefix: Path): PageURL = copy(path = prefix ++ path)
  def dropPrefix(prefix: Path): PageURL = copy(path = path.unnest(prefix))

}
object PageURL {

  private val fromWindowRaw: Task[PageURL] =
    ZIO.attempt { window.location.href }.flatMap {
      case href if href.startsWith("file://") => ZIO.succeed(PageURL(Path("/page"), QueryParams.empty))
      case href                               =>
        for {
          parsed <- ZIO.fromEither { URL.decode(href) }
        } yield PageURL(parsed.path, parsed.queryParams)
    }

  val fromWindow: UIO[PageURL] =
    fromWindowRaw.tapErrorCause { ZIO.logErrorCause("Error parsing window URL", _) }.orDie

}
