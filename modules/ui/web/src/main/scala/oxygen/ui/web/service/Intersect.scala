package oxygen.ui.web.service

import org.scalajs.dom.{document, Element, IntersectionObserver, IntersectionObserverEntry}
import scala.scalajs.js
import zio.*

/**
  * W12-T06: IntersectionObserver helper for infinite scroll / lazy sections.
  */
object Intersect {

  /**
    * Observe element by id. Invokes `onVisible` when intersecting.
    * @param once disconnect after first intersection
    * @param rootMargin CSS margin around root (e.g. `"200px"` fires before the sentinel hits the viewport edge)
    * Returns false if element missing. Callback is synchronous — wrap ZIO via UIRuntime / Runtime.unsafe in apps.
    */
  def observeId(
      id: String,
      once: Boolean = true,
      rootMargin: String = "0px",
  )(onVisible: () => Unit): UIO[Boolean] =
    ZIO.succeed {
      val el = Option(document.getElementById(id.stripPrefix("#")))
      el.foreach { element =>
        observeElement(element, once, rootMargin)(onVisible)
      }
      el.isDefined
    }

  def observeElement(
      element: Element,
      once: Boolean = true,
      rootMargin: String = "0px",
  )(onVisible: () => Unit): IntersectionObserver = {
    val cb: js.Function2[js.Array[IntersectionObserverEntry], IntersectionObserver, Unit] = { (entries, o) =>
      if entries.exists(_.isIntersecting) then {
        onVisible()
        if once then o.disconnect()
      }
    }
    val opts = js.Dynamic.literal(rootMargin = rootMargin).asInstanceOf[js.Object]
    val obs = new IntersectionObserver(cb, opts.asInstanceOf[org.scalajs.dom.IntersectionObserverInit])
    obs.observe(element)
    obs
  }

  /** Sentinel id used by [[oxygen.ui.web.component.InfiniteScroll]] footer. */
  val infiniteScrollSentinelId: String = "oxy-infinite-scroll-sentinel"

}
