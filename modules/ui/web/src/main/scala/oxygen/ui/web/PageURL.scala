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

  val fromWindow: UIO[PageURL] =
    (for
      path <- ZIO.attempt { window.location.href.stripPrefix("file://") }
      parsed <- ZIO.fromEither { URL.decode(path) }
    yield PageURL(parsed.path, parsed.queryParams)).tapErrorCause { ZIO.logErrorCause("Error parsing window URL", _) }.orDie

}
