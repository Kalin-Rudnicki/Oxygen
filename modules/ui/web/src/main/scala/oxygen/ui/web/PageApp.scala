package oxygen.ui.web

import oxygen.predef.core.*
import oxygen.ui.web.internal.{DefaultPages, NavigationEvent, RootErrorHandler, Router}
import oxygen.zio.logging.LogConfig
import zio.*

abstract class PageApp[Env: HasNoScope] extends ZIOAppDefault {

  val defaultPages: DefaultPages[Env] = DefaultPages.default

  // TODO (KR) : styles

  val pages: ArraySeq[RoutablePage[Env]]

  def layer: TaskLayer[Env]

  val logLevel: LogLevel = LogLevel.Info

  private def effect: RIO[Env, Unit] =
    for {
      _ <- LogConfig.usingConfig(LogConfig.oxygenDefault(logLevel)).set
      _ <- ZIO.logInfo("Welcome to Oxygen Web UI!")
      router <- Router.init[Env](pages, RootErrorHandler.Default(defaultPages))

      _ <- router.route(NavigationEvent.renderPage(defaultPages.initial)(()), 0)
      _ <- router.routeWindowURL
    } yield ()

  override def run: ZIO[Any, Any, Unit] =
    effect.provide(layer)

}
