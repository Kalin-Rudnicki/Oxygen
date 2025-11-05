package oxygen.ui.web

import oxygen.ui.web.create.*
import oxygen.ui.web.internal.*
import oxygen.ui.web.service.Window
import zio.*

// TODO (KR) : try to eliminate contravariance from Env. potentially add `.widen` and implicit conversion for `.widen`
sealed trait Page[-Env] { page =>

  ///////  ///////////////////////////////////////////////////////////////

  type Params
  type State

  final type PageWidgetState = WidgetState[State]

  ///////  ///////////////////////////////////////////////////////////////

  def title(state: State): String

  def initialLoad(params: Params): ZIO[Env & Scope, UIError, State]
  def postLoad(state: WidgetState[State]): ZIO[Env & Scope, UIError, Unit]

  ///////  ///////////////////////////////////////////////////////////////

  private[web] object InternalPageState extends PageLocalState[State](s"InternalPageState[$pageName]")(null.asInstanceOf[State])

  protected def component(state: State): WidgetES[Env, State]

  private[web] final def render[Env2 <: Env: HasNoScope](
      pageInstance: PageInstance.TypedEnv[Env2, Params, State],
      navType: NavigationEvent.NavType,
  ): URIO[Env2 & Scope, Unit] = {
    val state: WidgetState[State] = pageInstance.pageState
    ZIO.logTrace(s"rendering page $page") *>
      pageInstance.recordNewValues(state.renderTimeValue, navType) *>
      Renderer.renderBody(component(state.renderTimeValue).build[Env2](state, RaiseHandler.Empty, pageInstance, pageInstance.uiRuntime))
  }

  final def pageName: String = getClass.getSimpleName.stripSuffix("$")

  override def toString: String = s"Page[$pageName]"

}
object Page {

  type AuxE[E, P, S] = Page[E] { type Params = P; type State = S }
  type Aux[P, S] = Page[?] { type Params = P; type State = S }

}

trait NonRoutablePage[-Env] extends Page[Env] { page =>

  override def initialLoad(params: Params): ZIO[Env & Scope, Nothing, State]

  object navigate { // Env is enforced by this effect returning Env in the R type
    def render(params: Params): ZIO[Env, UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.renderPage(page)(params)))
  }

}
object NonRoutablePage {

  trait StateSameAsParams[-Env] extends NonRoutablePage[Env] {

    override final type State = Params

    override def initialLoad(params: Params): ZIO[Env & Scope, Nothing, Params] = ZIO.succeed(params)
    override def postLoad(state: WidgetState[Params]): ZIO[Env & Scope, UIError, Unit] = ZIO.unit

  }

  type AuxE[E, P, S] = NonRoutablePage[E] { type Params = P; type State = S }
  type AuxEP[E, P] = NonRoutablePage[E] { type Params = P }
  type Aux[P, S] = NonRoutablePage[?] { type Params = P; type State = S }

}

trait RoutablePage[-Env] extends Page[Env] { page =>

  lazy val paramCodec: PageCodec[Params]
  def paramsFromState(state: State): Params

  object navigate { // Env is enforced by checking that the page exists in the typed page router
    def push(params: Params): IO[UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.pushPage(page)(params)))
    def replace(params: Params): IO[UIError.Redirect, Nothing] = ZIO.fail(UIError.Redirect(NavigationEvent.replacePage(page)(params)))
    def openInNewTab(params: Params): UIO[Unit] = Window.newTab(paramCodec.encode(params))
  }

}
object RoutablePage {

  type AuxE[E, P, S] = RoutablePage[E] { type Params = P; type State = S }
  type Aux[P, S] = RoutablePage[?] { type Params = P; type State = S }

  trait NoParams[-Env] extends RoutablePage[Env] {

    val path: Seq[String]

    override final type Params = Unit
    override final lazy val paramCodec: PageCodec[Unit] = PageCodec.ConstPaths(path.toList)
    override final def paramsFromState(state: State): Unit = ()

  }

}
