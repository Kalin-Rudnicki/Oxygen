package oxygen.ui.web

import org.scalajs.dom.window
import zio.*
import zio.http.{Path, QueryParams}
import zio.http.URL

// TODO (KR) : have a way to represent absolute URLs as well
final case class PageURL(
    path: Path,
    queryParams: QueryParams,
    /** Fragment without leading `#` (`None` = none). */
    fragment: Option[String],
) {

  def formatted: String = {
    val base = URL(path = path.addLeadingSlash, queryParams = queryParams).encode
    fragment.filter(_.nonEmpty).fold(base)(f => s"$base#$f")
  }

  def addPrefix(prefix: Path): PageURL = copy(path = prefix ++ path)
  def dropPrefix(prefix: Path): PageURL = copy(path = path.unnest(prefix))

  def withFragment(frag: String): PageURL =
    withFragment(Some(frag))

  def withFragment(frag: Option[String]): PageURL =
    copy(fragment = frag.map(_.stripPrefix("#")).filter(_.nonEmpty))

  def clearFragment: PageURL = copy(fragment = None)

}
object PageURL {

  private def stripHash(href: String): (String, Option[String]) = {
    val i = href.indexOf('#')
    if i < 0 then (href, None)
    else {
      val frag = href.substring(i + 1)
      (href.substring(0, i), Option.when(frag.nonEmpty)(frag))
    }
  }

  private val fromWindowRaw: Task[PageURL] =
    ZIO.attempt { window.location.href }.flatMap {
      case href if href.startsWith("file://") =>
        val frag = Option(window.location.hash).map(_.stripPrefix("#")).filter(_.nonEmpty)
        ZIO.succeed(PageURL(Path("/page"), QueryParams.empty, frag))
      case href =>
        val (withoutHash, frag) = stripHash(href)
        for {
          parsed <- ZIO.fromEither { URL.decode(withoutHash) }
        } yield PageURL(parsed.path, parsed.queryParams, frag)
    }

  val fromWindow: UIO[PageURL] =
    fromWindowRaw.tapErrorCause { ZIO.logErrorCause("Error parsing window URL", _) }.orDie

  /** Pure helper for tests / callers building URLs. */
  def apply(path: Path, queryParams: QueryParams): PageURL =
    PageURL(path, queryParams, None)

}
