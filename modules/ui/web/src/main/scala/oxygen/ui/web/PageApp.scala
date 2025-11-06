package oxygen.ui.web

import org.scalajs.dom.document
import oxygen.predef.core.*
import oxygen.ui.web.create.StyleSheet
import oxygen.ui.web.internal.{DefaultPages, NavigationEvent, RootErrorHandler, Router}
import oxygen.zio.logging.{LogConfig, RichLogLevel}
import zio.*

abstract class PageApp[Env: HasNoScope] extends ZIOAppDefault {

  val defaultPages: DefaultPages[Env] = DefaultPages.default

  val pagePrefix: ArraySeq[String] = ArraySeq("page")

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
      pageUrl <- PageURL.fromWindow
      logLevel = pageUrl.queryParams.queryParams("oxygen-log-level").headOption.flatMap(RichLogLevel.strictEnum.decodeOption).getOrElse(RichLogLevel.Info)
      _ <- LogConfig.usingConfig(LogConfig.oxygenDefault(logLevel.level)).set

      _ <- ZIO.logInfo("Welcome to Oxygen Web UI!")
      router <- Router.init[Env](pages, pagePrefix, RootErrorHandler.Default(defaultPages))

      _ <- ZIO.foreachDiscard(styleSheets)(addStyleSheet)
      _ <- router.route(NavigationEvent.renderPage(defaultPages.initial)(()), 0)

      _ <- router.route(NavigationEvent.browserLoad(pageUrl), 0)
    } yield ()

  override def run: ZIO[Any, Any, Unit] =
    effect.provide(layer)

}
