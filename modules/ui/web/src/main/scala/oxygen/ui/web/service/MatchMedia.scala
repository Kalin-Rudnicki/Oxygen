package oxygen.ui.web.service

import org.scalajs.dom.window
import zio.*

/**
  * W12-T05: shared matchMedia helper for color mode, reduced motion, breakpoints.
  */
object MatchMedia {

  def matches(query: String): Boolean =
    window.matchMedia(query).matches

  def matchesZIO(query: String): UIO[Boolean] =
    ZIO.succeed(matches(query))

  def prefersDark: Boolean =
    matches("(prefers-color-scheme: dark)")

  def prefersDarkZIO: UIO[Boolean] =
    matchesZIO("(prefers-color-scheme: dark)")

  def prefersReducedMotion: Boolean =
    matches("(prefers-reduced-motion: reduce)")

  def prefersReducedMotionZIO: UIO[Boolean] =
    matchesZIO("(prefers-reduced-motion: reduce)")

  def minWidth(px: Int): Boolean =
    matches(s"(min-width: ${px}px)")

  def maxWidth(px: Int): Boolean =
    matches(s"(max-width: ${px}px)")

  /**
    * Subscribe to media query changes for the lifetime of the surrounding [[Scope]].
    * Listener is invoked on the browser event thread; wrap with UIRuntime if you need ZIO.
    */
  def onChange(query: String)(listener: Boolean => Unit): URIO[Scope, Unit] =
    ZIO
      .acquireRelease {
        ZIO.succeed {
          val mql = window.matchMedia(query)
          val handler: org.scalajs.dom.Event => Unit = _ => listener(mql.matches)
          mql.addEventListener("change", handler)
          (mql, handler)
        }
      } { case (mql, handler) =>
        ZIO.succeed(mql.removeEventListener("change", handler))
      }
      .unit

}
