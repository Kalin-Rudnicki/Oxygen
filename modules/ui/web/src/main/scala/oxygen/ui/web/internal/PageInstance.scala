package oxygen.ui.web.internal

import java.util.UUID
import org.scalajs.dom.{document, window}
import oxygen.predef.core.*
import oxygen.ui.web.{NonRoutablePage, Page, PageURL, RaiseHandler, RoutablePage, UIError}
import zio.*

trait PageInstance[Params, State] {

  private[web] val pageId: UUID
  protected val page: Page.Aux[Params, State]
  protected val errorHandler: RootErrorHandler

  private[web] final lazy val pageReference: PageReference = PageReference(pageId, page.getClass.getSimpleName.stripSuffix("$"))

  private[web] final lazy val pageState: GlobalStateManager.Value[State] = page.InternalPageState.getValue(pageReference)

  private[web] def recordNewValues(state: State, navType: NavigationEvent.NavType): UIO[Unit]

  private[web] def reRender: UIO[Unit]

  private[web] def close: UIO[Unit]

  def runForked[Env: HasNoScope](effect: URIO[Env & Scope, Unit]): URIO[Env, Unit]

  final def runForkedHandleErrors[Env: HasNoScope](toLoc: PageReference => RootErrorHandler.ErrorLocation)(effect: ZIO[Env & Scope, UIError, Unit]): URIO[Env, Unit] =
    runForked {
      RootErrorHandler.rootError(toLoc(pageReference))(effect).catchAll {
        case RootErrorHandler.ErrorWithLocation(loc, error) => errorHandler.handleErrorCause(loc, error)
        case UIError.Redirect(navEvent)                     => Router.route(navEvent)
      }
    }

}
object PageInstance {

  type Untyped = PageInstance[?, ?]

  enum ActiveState {
    case Active(scope: Scope.Closeable)
    case Closed
  }

  trait TypedEnv[Env: HasNoScope, Params, State] extends PageInstance[Params, State] {

    val activeRef: Ref[ActiveState]

    override protected val page: Page.AuxE[Env, Params, State]
    protected val uiRuntime: UIRuntime[Env]

    private[web] final lazy val raiseHandler: RaiseHandler.Root[Env, Params, State] =
      RaiseHandler.Root[Env, Params, State](page, this, uiRuntime)

    override private[web] final def reRender: UIO[Unit] =
      activeRef.get.flatMap {
        case ActiveState.Active(scope) => uiRuntime.execute(scope.extend[Env](page.render[Env](raiseHandler, NavigationEvent.NavType.Replace)))
        case ActiveState.Closed        => ZIO.logWarning(s"[PageExecutor.runForked] pageId=$pageId is closed")
      }

    // deliberately does not use `mutex`, as this is called from the parent, when the parent is within the mutex
    override private[web] final def close: UIO[Unit] =
      activeRef.get.flatMap {
        case ActiveState.Active(scope) =>
          for {
            fiberId <- ZIO.fiberId
            _ <- scope.close(Exit.interrupt(fiberId))
            _ <- activeRef.set(ActiveState.Closed)

            // TODO (KR) : it might be possible in the future to have "go back a page" resume from an existing page state
            _ <- ZIO.succeed { GlobalStateManager.releasePageLocal(pageReference) }
          } yield ()
        case ActiveState.Closed =>
          ZIO.logWarning(s"[PageExecutor.close] pageId=$pageId is already closed")
      }.uninterruptible

    override final def runForked[Env2: HasNoScope](effect: URIO[Env2 & Scope, Unit]): URIO[Env2, Unit] =
      activeRef.get.flatMap {
        case ActiveState.Active(scope) => scope.extend[Env2](effect).forkIn(scope).unit
        case ActiveState.Closed        => ZIO.logWarning(s"[PageExecutor.runForked] pageId=$pageId is closed")
      }

  }

  private def setTitle(title: String): UIO[Unit] =
    ZIO.succeed { document.title = title }

  private def nav(url: PageURL, title: String, navType: NavigationEvent.NavType): UIO[Unit] =
    navType match
      case NavigationEvent.NavType.Push    => ZIO.succeed { window.history.pushState(null, title, url.formatted) }
      case NavigationEvent.NavType.Replace => ZIO.succeed { window.history.replaceState(null, title, url.formatted) }
      case NavigationEvent.NavType.None    => setTitle(title)

  final case class Routable[Env: HasNoScope, Params, State](
      pageId: UUID,
      errorHandler: RootErrorHandler,
      activeRef: Ref[ActiveState],
      lastEvalRef: Ref[Option[Routable.LastEval[Params, State]]],
      page: RoutablePage.AuxE[Env, Params, State],
      uiRuntime: UIRuntime[Env],
  ) extends PageInstance.TypedEnv[Env, Params, State] {

    override private[web] def recordNewValues(state: State, navType: NavigationEvent.NavType): UIO[Unit] = {
      val params = page.paramsFromState(state)
      val title = page.title(state)

      lastEvalRef.get.flatMap {
        case Some(lastEval) if lastEval.params == params =>
          setTitle(title).whenDiscard(title != lastEval.title) *>
            lastEvalRef.set(Routable.LastEval(params, state, lastEval.url, title).some)
        case _ =>
          val newUrl = page.paramCodec.encode(params)
          nav(newUrl, title, navType) *>
            lastEvalRef.set(Routable.LastEval(params, state, newUrl, title).some)
      }
    }

  }
  object Routable {

    final case class LastEval[Params, State](
        params: Params,
        state: State,
        url: PageURL,
        title: String,
    )

  }

  final case class NonRoutable[Env: HasNoScope, Params, State](
      pageId: UUID,
      errorHandler: RootErrorHandler,
      activeRef: Ref[ActiveState],
      lastEvalRef: Ref[Option[NonRoutable.LastEval[State]]],
      page: NonRoutablePage.AuxE[Env, Params, State],
      uiRuntime: UIRuntime[Env],
  ) extends PageInstance.TypedEnv[Env, Params, State] {

    override private[web] def recordNewValues(state: State, navType: NavigationEvent.NavType): UIO[Unit] = {
      val title = page.title(state)

      lastEvalRef.get.flatMap {
        case Some(lastEval) =>
          setTitle(title).whenDiscard(title != lastEval.title) *>
            lastEvalRef.set(NonRoutable.LastEval(state, title).some)
        case None =>
          setTitle(title) *>
            lastEvalRef.set(NonRoutable.LastEval(state, title).some)
      }
    }

  }
  object NonRoutable {

    final case class LastEval[State](
        state: State,
        title: String,
    )

  }

}
