package oxygen.ui.web.service

import zio.*

/**
  * Scroll behavior for a page switch (OXY-155).
  *
  * When navigating to a new page: scroll to the `#anchor` when the URL has one, otherwise reset to
  * the top of the page. Without this, a client-side page switch inherits the previous page's scroll
  * position (the HolyGrail center pane is reused), so the new page "lands in the middle".
  */
object PageScroll {

  /** Where a navigation should scroll to. */
  enum Target {
    case Top
    case Fragment(id: String)
  }

  /** Pure decision: blank/empty fragment ⇒ [[Target.Top]]; otherwise scroll to the cleaned id. */
  def targetFor(fragment: Option[String]): Target =
    fragment.map(_.trim.stripPrefix("#").trim).filter(_.nonEmpty) match {
      case Some(id) => Target.Fragment(id)
      case None     => Target.Top
    }

  /** Apply the scroll behavior for a page switch, given the navigating URL's fragment. */
  def onNavigate(fragment: Option[String]): UIO[Unit] =
    targetFor(fragment) match {
      case Target.Top          => Window.scroll.toTop()
      case Target.Fragment(id) => HashScroll.toFragment(Some(id)).unit
    }

}
