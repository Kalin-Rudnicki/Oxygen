package oxygen.ui.web

import oxygen.ui.web.create.*
import oxygen.ui.web.internal.*
import oxygen.ui.web.service.Window
import zio.*

// TODO (KR) : try to eliminate contravariance from Env. potentially add `.widen` and implicit conversion for `.widen`
sealed trait Page[-Env] { page =>

  ///////  ///////////////////////////////////////////////////////////////

  type PageParams
  type PageState

  final type PageWidgetState = WidgetState[PageState]

  ///////  ///////////////////////////////////////////////////////////////

  def title(state: PageState): String

  def initialLoad(params: PageParams): ZIO[Env & Scope, UIError, PageState]
  def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Env & Scope, UIError, Unit]

  protected final val job: PageJob.WithState[PageState] = PageJob.withState[PageState]

  // TODO (KR) : read this and schedule jobs once the page is loaded
  protected val jobs: Seq[PageJob[Env, PageState]] = Nil
  private[ui] final def internalJobs: Seq[PageJob[Env, PageState]] = jobs

  ///////  ///////////////////////////////////////////////////////////////

  private[web] object InternalPageState extends PageLocalState[PageState](s"InternalPageState[$pageName]")(null.asInstanceOf[PageState])

  protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[Env, PageState]

  private[web] final def render[Env2 <: Env: HasNoScope](
      pageInstance: PageInstance.TypedEnv[Env2, PageParams, PageState],
      navType: NavigationEvent.NavType,
  ): URIO[Env2 & Scope, Unit] = {
    val state: WidgetState[PageState] = pageInstance.pageState
    ZIO.logTrace(s"rendering page $page") *>
      pageInstance.recordNewValues(state.renderTimeValue, navType) *>
      Renderer.renderBody(component(state, state.renderTimeValue).build[Env2](state, RaiseHandler.Empty, pageInstance, pageInstance.uiRuntime))
  }

  final def pageName: String = getClass.getSimpleName.stripSuffix("$")

  override def toString: String = s"Page[$pageName]"

}
object Page {

  type AuxE[E, P, S] = Page[E] { type PageParams = P; type PageState = S }
  type Aux[P, S] = Page[?] { type PageParams = P; type PageState = S }

}

trait NonRoutablePage[-Env] extends Page[Env] { page =>

  override def initialLoad(params: PageParams): ZIO[Env & Scope, Nothing, PageState]

  object navigate { // Env is enforced by this effect returning Env in the R type
    def render(params: PageParams): ZIO[Env, UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.renderPage(page)(params)))
  }

}
object NonRoutablePage {

  trait StateSameAsParams[-Env] extends NonRoutablePage[Env] {

    override final type PageState = PageParams

    override def initialLoad(params: PageParams): ZIO[Env & Scope, Nothing, PageParams] = ZIO.succeed(params)
    override def postLoad(state: WidgetState[PageParams], initialState: PageState): ZIO[Env & Scope, UIError, Unit] = ZIO.unit

  }

  type AuxE[E, P, S] = NonRoutablePage[E] { type PageParams = P; type PageState = S }
  type AuxEP[E, P] = NonRoutablePage[E] { type PageParams = P }
  type Aux[P, S] = NonRoutablePage[?] { type PageParams = P; type PageState = S }

}

trait RoutablePage[-Env] extends Page[Env] { page =>

  lazy val paramCodec: PageCodec[PageParams]
  def paramsFromState(state: PageState): PageParams

  object navigate { // Env is enforced by checking that the page exists in the typed page router
    def apply(params: PageParams): RoutablePage.Navigate = new RoutablePage.Navigate(page)(params)
    def push(params: PageParams): IO[UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.pushPage(page)(params)))
    def replace(params: PageParams): IO[UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.replacePage(page)(params)))
    def openInNewTab(params: PageParams): UIO[Unit] = Window.newTab(paramCodec.encode(params))
  }

}
object RoutablePage {

  final class Navigate(private val page: RoutablePage[?])(private val params: page.PageParams) {
    def push: IO[UIError.Redirect, Nothing] = page.navigate.push(params)
    def replace: IO[UIError.Redirect, Nothing] = page.navigate.replace(params)
    def openInNewTab: UIO[Unit] = page.navigate.openInNewTab(params)
  }

  type AuxE[E, P, S] = RoutablePage[E] { type PageParams = P; type PageState = S }
  type Aux[P, S] = RoutablePage[?] { type PageParams = P; type PageState = S }

  trait NoParams[-Env] extends RoutablePage[Env] {

    val path: Seq[String]

    override final type PageParams = Unit
    override final lazy val paramCodec: PageCodec[Unit] = PageCodec.ConstPaths(path.toList)
    override final def paramsFromState(state: PageState): Unit = ()

  }

}
