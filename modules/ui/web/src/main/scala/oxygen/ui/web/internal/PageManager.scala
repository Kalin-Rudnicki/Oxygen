package oxygen.ui.web.internal

import oxygen.predef.core.*
import oxygen.ui.web.{NonRoutablePage, Page, RoutablePage, UIError}
import oxygen.ui.web.internal.NavigationEvent.Target
import zio.*

trait PageManager {

  def currentPageRef: UIO[PageReference]

  def currentErrorLocation: UIO[RootErrorHandler.ErrorLocation]

  def loadPage[Env: HasNoScope](
      target: NavigationEvent.Target.PageWithParams[Env],
      navType: NavigationEvent.NavType,
      uiRuntime: UIRuntime[Env],
  ): ZIO[Env, RootErrorHandler.RootError, Unit]

  def reRenderCurrentPage: UIO[Unit]

}
object PageManager {

  private final case class Impl(
      currentRef: Ref[Option[PageInstance.Untyped]],
      errorHandler: RootErrorHandler,
  ) extends PageManager {

    override def currentPageRef: UIO[PageReference] =
      currentRef.get.someOrElseZIO { ZIO.dieMessage("No current page...") }.map(_.pageReference)

    override def currentErrorLocation: UIO[RootErrorHandler.ErrorLocation] =
      currentRef.get.map {
        case Some(current) => RootErrorHandler.ErrorLocation.NavigationAttempt(current.pageReference)
        case None          => RootErrorHandler.ErrorLocation.BrowserLoad
      }

    private def makeRaw[Env: HasNoScope, Params, State](
        scope: Scope.Closeable,
        uiRuntime: UIRuntime[Env],
        page: Page.AuxE[Env, Params, State],
    ): UIO[PageInstance.TypedEnv[Env, Params, State]] =
      page match {
        case page: RoutablePage.AuxE[Env @unchecked, Params, State] =>
          for {
            pageId <- Random.nextUUID
            stateRef <- Ref.make(PageInstance.ActiveState.Active(scope))
            lastEvalRef <- Ref.make(Option.empty[PageInstance.Routable.LastEval[Params, State]])
          } yield PageInstance.Routable[Env, page.Params, page.State](
            pageId = pageId,
            errorHandler = errorHandler,
            activeRef = stateRef,
            lastEvalRef = lastEvalRef,
            page = page,
            uiRuntime = uiRuntime,
          )
        case page: NonRoutablePage.AuxE[Env @unchecked, Params, State] =>
          for {
            pageId <- Random.nextUUID
            stateRef <- Ref.make(PageInstance.ActiveState.Active(scope))
            lastEvalRef <- Ref.make(Option.empty[PageInstance.NonRoutable.LastEval[State]])
          } yield PageInstance.NonRoutable[Env, page.Params, page.State](
            pageId = pageId,
            errorHandler = errorHandler,
            activeRef = stateRef,
            lastEvalRef = lastEvalRef,
            page = page,
            uiRuntime = uiRuntime,
          )
      }

    override def loadPage[Env: HasNoScope](
        target: Target.PageWithParams[Env],
        navType: NavigationEvent.NavType,
        uiRuntime: UIRuntime[Env],
    ): ZIO[Env, RootErrorHandler.RootError, Unit] =
      (for {
        _ <- ZIO.logTrace(s"----- Loading Page - ${target.page} ---")
        currentPageInstance <- currentRef.get
        loc = currentPageInstance match
          case Some(currentPageInstance) => RootErrorHandler.ErrorLocation.NavigationAttempt(currentPageInstance.pageReference)
          case None                      => RootErrorHandler.ErrorLocation.BrowserLoad

        newScope <- Scope.make
        _ <- newScope.addFinalizer { ZIO.logTrace(s"----- Closing Scope - ${target.page} ---") }

        _ <- ZIO.logTrace("Loading state")
        initialState <- RootErrorHandler.rootError(loc) {
          target.page
            .initialLoad(target.params)
            .provideSomeEnvironment[Env](_.add(newScope))
            .tapErrorCause { cause => newScope.close(Exit.Failure(cause)) }
        }

        _ <- (for {
          // TODO (KR) : probably need a way to mark that a page is attempting to schedule itself.
          // TODO (KR) : what happens if one page takes 5s to load, and in that time, you try to load another page

          _ <- ZIO.logTrace("Creating page instance")
          pageInstance <- makeRaw[Env, target.page.Params, target.page.State](newScope, uiRuntime, target.page) // TODO (KR) : thread Params and State into `makeRaw`
          _ <- ZIO.succeed { pageInstance.pageState.set(initialState) }

          _ <- ZIO.logTrace("Switching current and closing previous")
          _ <- currentRef.set(pageInstance.some)
          _ <- ZIO.foreachDiscard(currentPageInstance)(_.close)

          _ <- pageInstance.reRender

          _ <- ZIO.logTrace("PageManager.postLoad.start")
          _ <-
            RootErrorHandler
              .rootError(RootErrorHandler.ErrorLocation.NavigationPagePostLoad(pageInstance.pageReference)) {
                newScope.extend[Env] { target.page.postLoad(initialState, pageInstance.raiseHandler.eraseEnv) }
              }
              .catchAll {
                case RootErrorHandler.ErrorWithLocation(loc, error) => errorHandler.handleErrorCause(loc, error)
                case UIError.Redirect(navEvent)                     => Router.route(navEvent)
              }
          _ <- ZIO.logTrace("PageManager.postLoad.end")
        } yield ()).forkIn(newScope)
      } yield ()) @@ ZIOAspect.annotated("page", target.page.toString)

    override def reRenderCurrentPage: UIO[Unit] =
      currentRef.get.flatMap {
        case Some(current) => current.reRender
        case None          => ZIO.logWarning("No current page to render")
      }

  }

  private val currentPageManagerRef: Ref[Option[PageManager]] =
    Unsafe.unsafely { Ref.unsafe.make(None) }

  private val currentPage: UIO[PageManager] =
    currentPageManagerRef.get.someOrElseZIO { ZIO.dieMessage("No current PageManager") }

  private[web] def currentPageRef: UIO[PageReference] =
    currentPage.flatMap(_.currentPageRef)

  private[web] def reRenderCurrentPage: UIO[Unit] =
    currentPage.flatMap(_.reRenderCurrentPage)

  def make(errorHandler: RootErrorHandler): UIO[PageManager] =
    for {
      runningRef <- Ref.make(Option.empty[PageInstance.Untyped])
      pageManager = Impl(runningRef, errorHandler)
      _ <- currentPageManagerRef.set(pageManager.some)
    } yield pageManager

}
