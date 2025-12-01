package oxygen.ui.web.internal

import org.scalajs.dom.window
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.PageCodec.ParseResult
import scala.annotation.tailrec
import zio.*
import zio.http.Path

trait Router {

  def route(navEvent: NavigationEvent, redirectNo: Int): UIO[Unit]

  val pagePrefixPath: Path

  private[web] def routeWindowURL: UIO[Unit] =
    PageURL.fromWindow.flatMap { url => route(NavigationEvent.browserLoad(url), 0) }

}
object Router {

  private val currentRouterRef: Ref[Option[Router]] =
    Unsafe.unsafely { Ref.unsafe.make(None) }

  def route(navEvent: NavigationEvent): UIO[Unit] = route(navEvent, 0)
  def route(navEvent: NavigationEvent, redirectNo: Int): UIO[Unit] =
    currentRouterRef.get
      .someOrElseZIO { ZIO.dieMessage("No router set") }
      .flatMap(_.route(navEvent, redirectNo))

  def pagePrefixPath: UIO[Path] =
    currentRouterRef.get
      .someOrElseZIO { ZIO.dieMessage("No router set") }
      .map(_.pagePrefixPath)

  private final class Inst[Env: HasNoScope] private[Router] (
      pages: ArraySeq[RoutablePage[Env]],
      val pagePrefixPath: Path,
      pageManager: PageManager,
      uiRuntime: UIRuntime[Env],
      rootErrorHandler: RootErrorHandler,
  ) extends Router {

    override def route(navEvent: NavigationEvent, redirectNo: Int): UIO[Unit] =
      ZIO.logTrace(s"Attempting to route: navEvent = $navEvent, redirectNo = $redirectNo") *>
        ZIO.dieMessage("Infinite page redirect detected").whenDiscard(redirectNo >= 25) *>
        uiRuntime.execute {
          pageManager.currentErrorLocation
            .flatMap(RootErrorHandler.rootError(_) { findPage(navEvent.target) })
            .flatMap(pageManager.loadPage[Env](_, navEvent.navType, uiRuntime))
            .catchAll {
              case RootErrorHandler.ErrorWithLocation(loc, error) => rootErrorHandler.handleErrorCause(loc, error)
              case UIError.Redirect(navEvent)                     => route(navEvent, redirectNo + 1)
            }
        }

    private def findPage(target: NavigationEvent.Target): IO[UIError.Internal, NavigationEvent.Target.PageWithParams[Env]] =
      target match {
        case NavigationEvent.Target.Url(tmpUrl) =>
          val url = tmpUrl.dropPrefix(pagePrefixPath)

          @tailrec
          def loop(idx: Int): IO[UIError.Internal, NavigationEvent.Target.PageWithParams[Env]] =
            if idx < pages.length then {
              val page: RoutablePage[Env] = pages(idx)

              page.paramCodec.decode(url) match {
                case ParseResult.Success(params) => ZIO.succeed(NavigationEvent.Target.RoutablePageWithParams[Env](page)(params))
                case ParseResult.Error(error)    => ZIO.fail(UIError.Internal.PageParamDecodingFailure(error))
                case ParseResult.InvalidPath     => loop(idx + 1)
              }
            } else ZIO.fail(UIError.Internal.UrlNotFound(url))

          loop(0)
        case _pageWithParams: NavigationEvent.Target.PageWithParams[_] =>
          val pageWithParams: NavigationEvent.Target.PageWithParams[Env] = _pageWithParams.asInstanceOf[NavigationEvent.Target.PageWithParams[Env]]
          pageWithParams match {
            case pageWithParams: NavigationEvent.Target.RoutablePageWithParams[Env] if !pages.contains(pageWithParams.page) =>
              ZIO.fail(UIError.Internal.DirectPageNavigationToPageNotInRouter(pageWithParams.routablePage))
            case pageWithParams: NavigationEvent.Target.RoutablePageWithParams[Env]    => ZIO.succeed(pageWithParams)
            case pageWithParams: NavigationEvent.Target.NonRoutablePageWithParams[Env] => ZIO.succeed(pageWithParams)
          }
      }

  }

  def init[Env: HasNoScope](
      pages: ArraySeq[RoutablePage[Env]],
      pagePrefix: ArraySeq[String],
      rootErrorHandler: RootErrorHandler,
  ): URIO[Env, Router] = {
    val pagePrefixPath: Path = pagePrefix.foldLeft(Path.root)(_ / _)

    for
      pageManager <- PageManager.make(rootErrorHandler, pagePrefixPath)
      uiRuntime <- UIRuntime.make[Env]
      router = Inst(pages, pagePrefixPath, pageManager, uiRuntime, rootErrorHandler)
      _ <- currentRouterRef.set(router.some)
      _ <- ZIO.succeed { window.onpopstate = { _ => uiRuntime.unsafeExecute { router.routeWindowURL } } }
    yield router
  }

}
