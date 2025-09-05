package oxygen.ui.web

import org.scalajs.dom.document
import oxygen.predef.core.*
import oxygen.ui.web.create.StyleSheet
import oxygen.ui.web.internal.{DefaultPages, NavigationEvent, RootErrorHandler, Router}
import oxygen.zio.logging.LogConfig
import zio.*

abstract class PageApp[Env: HasNoScope] extends ZIOAppDefault {

  val defaultPages: DefaultPages[Env] = DefaultPages.default

  val pages: ArraySeq[RoutablePage[Env]]

  val styleSheets: ArraySeq[StyleSheet]

  def layer: TaskLayer[Env]

  val logLevel: LogLevel = LogLevel.Info

  private def addStyleSheet(sheet: StyleSheet): Task[Unit] =
    ZIO.attempt {
      val styleElement = document.createElement("style")
      styleElement.id = sheet.styleSheetId
      styleElement.innerHTML = sheet.innerHTML
      document.head.append(styleElement)
    }

  private def effect: RIO[Env, Unit] =
    for {
      _ <- ZIO.logInfo("Welcome to Oxygen Web UI!")
      router <- Router.init[Env](pages, RootErrorHandler.Default(defaultPages))

      _ <- ZIO.foreachDiscard(styleSheets)(addStyleSheet)
      _ <- router.route(NavigationEvent.renderPage(defaultPages.initial)(()), 0)
      _ <- router.routeWindowURL
    } yield ()

  override def run: ZIO[Any, Any, Unit] =
    LogConfig.usingConfig(LogConfig.oxygenDefault(logLevel)).set *>
      effect.provide(layer)

}
