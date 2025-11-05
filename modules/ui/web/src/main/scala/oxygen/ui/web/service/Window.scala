package oxygen.ui.web.service

import org.scalajs.dom.{document as D, window as W}
import org.scalajs.dom.window
import oxygen.ui.web.PageURL
import oxygen.ui.web.internal.Router
import zio.*

object Window {

  def newTab(url: String): UIO[Unit] = ZIO.succeed { W.open(url, "_blank") }
  def newTab(url: PageURL): UIO[Unit] =
    for {
      origin <- ZIO.succeed { window.location.origin }
      prefix <- Router.pagePrefixPath
      _ <- newTab(origin + url.addPrefix(prefix).formatted)
    } yield ()

  // ideally, this would open a new window, but it does not seem possible to do this in javascript
  def newWindow(url: String): UIO[Unit] = ZIO.succeed { W.open(url, "_blank") }
  def newWindow(url: PageURL): UIO[Unit] = newWindow(url.formatted)

  def setTitle(title: String): UIO[Unit] = ZIO.succeed { D.title = title }

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

}
