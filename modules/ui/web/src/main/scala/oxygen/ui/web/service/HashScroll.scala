package oxygen.ui.web.service

import oxygen.ui.web.PageURL
import zio.*

/**
  * W7-T03: scroll to `#fragment` after DOM is available.
  * Retries briefly so async-mounted content can appear.
  */
object HashScroll {

  private val defaultAttempts: Int = 12
  private val defaultDelay: Duration = 40.millis

  /** Scroll when fragment non-empty; no-op for empty/`None`. Returns true if element found. */
  def toFragment(
      fragment: Option[String],
      options: Window.scroll.Options = Window.scroll.Options(),
      attempts: Int = defaultAttempts,
      delay: Duration = defaultDelay,
  ): UIO[Boolean] =
    fragment.map(_.stripPrefix("#")).filter(_.nonEmpty) match {
      case None     => ZIO.succeed(false)
      case Some(id) =>
        Window.scroll.toId(id, options).flatMap {
          case true                   => ZIO.succeed(true)
          case false if attempts <= 1 => ZIO.succeed(false)
          case false                  => ZIO.sleep(delay) *> toFragment(Some(id), options, attempts - 1, delay)
        }
    }

  def toFragment(
      fragment: String,
      options: Window.scroll.Options,
      attempts: Int,
      delay: Duration,
  ): UIO[Boolean] =
    toFragment(Option(fragment).filter(_.nonEmpty), options, attempts, delay)

  def toFragment(fragment: String): UIO[Boolean] =
    toFragment(Option(fragment).filter(_.nonEmpty))

  /** Read current window hash and scroll (with retry). */
  def toWindowFragment(
      options: Window.scroll.Options = Window.scroll.Options(),
      attempts: Int = defaultAttempts,
      delay: Duration = defaultDelay,
  ): UIO[Boolean] =
    PageURL.fromWindow.flatMap(u => toFragment(u.fragment, options, attempts, delay))

}
