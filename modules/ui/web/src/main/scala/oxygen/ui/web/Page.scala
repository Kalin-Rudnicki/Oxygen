package oxygen.ui.web

import oxygen.ui.web.internal.*
import zio.*

sealed trait Page[-Env] { page =>

  ///////  ///////////////////////////////////////////////////////////////

  type Params
  type State

  ///////  ///////////////////////////////////////////////////////////////

  def title(state: State): String

  def initialLoad(params: Params): ZIO[Env & Scope, UIError, State]
  def postLoad(state: State, rh: RaiseHandler.Stateful[Nothing, State]): ZIO[Env & Scope, UIError, Unit]

  ///////  ///////////////////////////////////////////////////////////////

  private[web] object InternalPageState extends PageLocalState[State](s"InternalPageState[$pageName]")(null.asInstanceOf[State])

  protected def component(state: State): Widget.Stateful[Env, Nothing, State]

  private[web] final def render[Env2 <: Env: HasNoScope](rh: RaiseHandler.Root[Env2, Params, State], navType: NavigationEvent.NavType): URIO[Env2 & Scope, Unit] = {
    val state: State = InternalPageState.getValue(rh.pageInstance.pageReference).get()
    ZIO.logTrace(s"rendering page $page") *>
      rh.pageInstance.recordNewValues(state, navType) *>
      Renderer.renderBody(component(state).build[Env2](state, rh, rh.pageInstance, rh.uiRuntime))
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
    override final def initialLoad(params: Params): ZIO[Env & Scope, Nothing, Params] = ZIO.succeed(params)

    override def postLoad(state: Params, rh: RaiseHandler.Stateful[Nothing, Params]): ZIO[Env & Scope, UIError, Unit] = ZIO.unit

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
