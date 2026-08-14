package oxygen.ui.web.service

import org.scalajs.dom.{document as D, window as W}
import org.scalajs.dom.window
import oxygen.ui.web.PageURL
import oxygen.ui.web.internal.Router
import scala.scalajs.js
import zio.*

object Window {

  def newTab(url: String): UIO[Unit] = ZIO.succeed { W.open(url, "_blank") }
  def newTab(url: PageURL): UIO[Unit] =
    for
      origin <- ZIO.succeed { window.location.origin }
      prefix <- Router.pagePrefixPath
      _ <- newTab(origin + url.addPrefix(prefix).formatted)
    yield ()

  // ideally, this would open a new window, but it does not seem possible to do this in javascript
  def newWindow(url: String): UIO[Unit] = ZIO.succeed { W.open(url, "_blank") }
  def newWindow(url: PageURL): UIO[Unit] = newWindow(url.formatted)

  def setTitle(title: String): UIO[Unit] = ZIO.succeed { D.title = title }

  object location {

    def assign(url: String): UIO[Unit] = ZIO.succeed { W.location.assign(url) }
    def replace(url: String): UIO[Unit] = ZIO.succeed { W.location.replace(url) }

    /**
      * Set / clear the URL fragment (`#id`). Empty clears the hash.
      * Triggers the browser `hashchange` listener (Router → HashScroll).
      */
    def setHash(fragment: String): UIO[Unit] =
      ZIO.succeed {
        val id = fragment.stripPrefix("#").trim
        W.location.hash = if id.isEmpty then "" else id
      }

    def hash: UIO[String] =
      ZIO.succeed { Option(W.location.hash).getOrElse("").stripPrefix("#") }

  }

  object history {

    def push(url: String, title: String): UIO[Unit] = ZIO.succeed { D.title = title; W.history.pushState(null, null, url) }
    def push(url: String): UIO[Unit] = ZIO.succeed { W.history.pushState(null, null, url) }
    def push(url: PageURL, title: String): UIO[Unit] = push(url.formatted, title)
    def push(url: PageURL): UIO[Unit] = push(url.formatted)

    def replace(url: String, title: String): UIO[Unit] = ZIO.succeed { D.title = title; W.history.replaceState(null, null, url) }
    def replace(url: String): UIO[Unit] = ZIO.succeed { W.history.replaceState(null, null, url) }
    def replace(url: PageURL, title: String): UIO[Unit] = replace(url.formatted, title)
    def replace(url: PageURL): UIO[Unit] = replace(url.formatted)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Scroll (W7-T02)
  //
  // Uses document (window) scrolling by default. Nested scroll containers (e.g. HolyGrail center
  // panel with overflow:auto) need the element to be inside that scroller; offsetPx compensates for
  // fixed top bars. For a custom container, scroll its scrollTop yourself or pass offset relative
  // to the document viewport.
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object scroll {

    // TODO (KR): support scrolling to Top / Middle / Bottom of the document (and optionally of a container).

    final case class Options(
        smooth: Boolean = true,
        /** Extra offset from top (e.g. fixed top bar height). */
        offsetPx: Double = 0,
    )

    private def behaviorStr(smooth: Boolean): String =
      if smooth then "smooth" else "instant"

    private def scrollWindowTo(y: Double, smooth: Boolean): Unit =
      // scalajs-dom 2.8 only types scrollTo(x,y); options object via dynamic for smooth + top.
      W.asInstanceOf[js.Dynamic].scrollTo(
        js.Dynamic.literal(
          top = y,
          left = 0.0,
          behavior = behaviorStr(smooth),
        ),
      )

    /** Page-content scroll containers (HolyGrail center pane, reused across page switches). */
    private val scrollContainerSelector: String =
      ".oxy-holy-grail-center, .oxy-holy-grail--center-only"

    private def resetScrollContainers(): Unit = {
      val nodes = D.querySelectorAll(scrollContainerSelector)
      var i = 0
      while i < nodes.length do {
        val el = nodes(i)
        el.scrollTop = 0
        el.scrollLeft = 0
        i += 1
      }
    }

    private def scrollElementIntoView(el: org.scalajs.dom.Element, smooth: Boolean): Unit =
      el.asInstanceOf[js.Dynamic].scrollIntoView(
        js.Dynamic.literal(
          behavior = behaviorStr(smooth),
          block = "start",
        ),
      )

    /** Scroll the document so `#id` is visible. Returns false if element missing. */
    def toId(id: String, options: Options = Options()): UIO[Boolean] =
      ZIO.succeed {
        val el = Option(D.getElementById(id.stripPrefix("#")))
        el.foreach { e =>
          if options.offsetPx == 0 then scrollElementIntoView(e, options.smooth)
          else {
            val top = e.getBoundingClientRect().top + W.scrollY - options.offsetPx
            scrollWindowTo(top, options.smooth)
          }
        }
        el.isDefined
      }

    /** Alias matching task acceptance wording. */
    def scrollToId(id: String, options: Options = Options()): UIO[Boolean] = toId(id, options)

    /** Scroll document to y. */
    def toY(y: Double, smooth: Boolean = true): UIO[Unit] =
      ZIO.succeed { scrollWindowTo(y, smooth) }

    /**
      * Reset scroll to the very top of the page. Resets both the document/window scroll and the
      * reused page-content scroll containers (HolyGrail center pane). Used on page switch so a new
      * page appears at the top instead of inheriting the previous page's scroll position.
      * Instant by default: a fresh page should render at the top, not animate up from mid-page.
      */
    def toTop(smooth: Boolean = false): UIO[Unit] =
      ZIO.succeed {
        scrollWindowTo(0, smooth)
        resetScrollContainers()
      }

  }

}
