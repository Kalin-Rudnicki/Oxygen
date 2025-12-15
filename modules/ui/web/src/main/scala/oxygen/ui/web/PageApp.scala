package oxygen.ui.web

import org.scalajs.dom.document
import oxygen.predef.core.*
import oxygen.ui.web.create.StyleSheet
import oxygen.ui.web.internal.{ActivityWatchdog, DefaultPages, NavigationEvent, RootErrorHandler, Router}
import oxygen.zio.logging.{LogConfig, RichLogLevel}
import zio.*

abstract class PageApp[Env: {HasNoScope, EnvironmentTag}] extends ZIOAppDefault {

  val defaultPages: DefaultPages[Env] = DefaultPages.default

  val pagePrefix: ArraySeq[String] = ArraySeq("page")

  val pages: ArraySeq[RoutablePage[Env]]

  val styleSheets: ArraySeq[StyleSheet]

  def layer: TaskLayer[Env]

  val logLevel: LogLevel = LogLevel.Info

  val jobs: Seq[GlobalJob[Env]] = Nil

  private def addStyleSheet(sheet: StyleSheet): Task[Unit] =
    ZIO.attempt {
      val styleElement = document.createElement("style")
      styleElement.id = sheet.styleSheetId
      styleElement.innerHTML = sheet.innerHTML
      document.head.append(styleElement)
    }

  protected def prePageLoad: RIO[Env & Scope, Unit] = ZIO.unit

  protected def postPageLoad: RIO[Env & Scope, Unit] = ZIO.unit

  private def effect: RIO[Env & Scope, Unit] =
    for {
      pageUrl <- PageURL.fromWindow
      logLevel = pageUrl.queryParams.queryParams("oxygen-log-level").headOption.flatMap(RichLogLevel.strictEnum.decodeOption).fold(this.logLevel)(_.level)
      _ <- LogConfig.usingConfig(LogConfig.oxygenDefault(logLevel)).set

      _ <- ZIO.logInfo("Welcome to Oxygen Web UI!")
      watchdog <- ActivityWatchdog.make
      _ <- prePageLoad
      router <- Router.init[Env](pages, pagePrefix, RootErrorHandler.Default(defaultPages), watchdog)

      _ <- ZIO.foreachDiscard(styleSheets)(addStyleSheet)
      _ <- router.route(NavigationEvent.renderPage(defaultPages.initial)(()), 0)

      _ <- router.route(NavigationEvent.browserLoad(pageUrl), 0)
      // TODO (KR) : do better than dying
      _ <- ZIO.foreachDiscard(jobs) { job => watchdog.scheduleEffect(job.name, job.timeout.getOrElse(PageJob.defaultTimeout), job.effect.orDie) }
      _ <- postPageLoad
    } yield ()

  override def run: ZIO[Scope, Any, Unit] =
    (effect *> ZIO.never) // the `ZIO.never` here stops the app from completing, and closing the scope
      .provideSomeLayer[Scope](layer)

}
