package oxygen.ui.web

import monocle.Lens
import oxygen.ui.web.internal.*
import zio.*

trait RaiseHandler[-Env: HasNoScope, -Action, +StateGet, -StateSet <: StateGet] {

  def handleRaise(raise: Raise[Action, StateGet, StateSet]): ZIO[Env & Scope, UIError, Unit]

  // WARNING! Use with caution! This should be used in as few places as possible!
  private[web] final def eraseEnv: RaiseHandler[Any, Action, StateGet, StateSet] = this.asInstanceOf[RaiseHandler[Any, Action, StateGet, StateSet]]

}
object RaiseHandler {

  type Stateless[-Action] = RaiseHandler[Any, Action, Any, Nothing]
  type Stateful[-Action, State] = RaiseHandler[Any, Action, State, State]

  type StatelessE[-Env, -Action] = RaiseHandler[Env, Action, Any, Nothing]
  type StatefulE[-Env, -Action, State] = RaiseHandler[Env, Action, State, State]

  extension [Env, Action](self: RaiseHandler.StatelessE[Env, Action]) {
    def raiseAction(action: Action): ZIO[Env & Scope, UIError, Unit] = self.handleRaise(Raise.Action(action))
  }

  extension [Env, Action, StateSet](self: RaiseHandler.StatefulE[Env, Action, StateSet]) {
    def setState(value: StateSet): ZIO[Env & Scope, UIError, Unit] = self.handleRaise(Raise.SetState(value, true))
    def updateState(value: StateSet => StateSet): ZIO[Env & Scope, UIError, Unit] = self.handleRaise(Raise.UpdateState(value, true))
  }

  final case class ZoomIn[Env: HasNoScope, Action, InnerState, OuterState](
      underlying: RaiseHandler.StatefulE[Env, Action, OuterState],
      lens: Lens[OuterState, InnerState],
  ) extends RaiseHandler.StatefulE[Env, Action, InnerState] {

    override def handleRaise(raise: Raise.Stateful[Action, InnerState]): ZIO[Env & Scope, UIError, Unit] =
      underlying.handleRaise(raise.zoomOut(lens))

  }

  final case class WithPageLocalState[Env: HasNoScope, Action, State](
      underlying: RaiseHandler.StatelessE[Env, Action],
      pageLocalState: PageLocalState[State],
      pageReference: PageReference,
  ) extends RaiseHandler.StatefulE[Env, Action, State] {

    override def handleRaise(raise: Raise.Stateful[Action, State]): ZIO[Env & Scope, UIError, Unit] =
      raise match {
        case stateless: Raise.WithoutState[Action] => underlying.handleRaise(stateless)
        case Raise.UpdateState(update, true)       => ZIO.succeed(pageLocalState.getValue(pageReference).modify(update)) *> underlying.handleRaise(Raise.ReRender)
        case Raise.SetState(value, true)           => ZIO.succeed(pageLocalState.getValue(pageReference).set(value)) *> underlying.handleRaise(Raise.ReRender)
        case Raise.UpdateState(update, false)      => ZIO.succeed(pageLocalState.getValue(pageReference).modify(update))
        case Raise.SetState(value, false)          => ZIO.succeed(pageLocalState.getValue(pageReference).set(value))
      }

  }

  final case class WithGlobalState[Env: HasNoScope, Action, State](
      underlying: RaiseHandler.StatelessE[Env, Action],
      globalState: GlobalState[State],
  ) extends RaiseHandler.StatefulE[Env, Action, State] {

    override def handleRaise(raise: Raise.Stateful[Action, State]): ZIO[Env & Scope, UIError, Unit] =
      raise match {
        case stateless: Raise.WithoutState[Action] => underlying.handleRaise(stateless)
        case Raise.UpdateState(update, true)       => ZIO.succeed(globalState.getValue().modify(update)) *> underlying.handleRaise(Raise.ReRender)
        case Raise.SetState(value, true)           => ZIO.succeed(globalState.getValue().set(value)) *> underlying.handleRaise(Raise.ReRender)
        case Raise.UpdateState(update, false)      => ZIO.succeed(globalState.getValue().modify(update))
        case Raise.SetState(value, false)          => ZIO.succeed(globalState.getValue().set(value))
      }

  }

  final case class ConvertRaise[Env: HasNoScope, Action1, Action2, StateGet, StateSet <: StateGet](
      underlying: RaiseHandler[Env, Action2, StateGet, StateSet],
      f: Action1 => Raise[Action2, StateGet, StateSet],
  ) extends RaiseHandler[Env, Action1, StateGet, StateSet] {

    override def handleRaise(raise: Raise[Action1, StateGet, StateSet]): ZIO[Env & Scope, UIError, Unit] =
      raise match
        case Raise.Action(action)                           => underlying.handleRaise(f(action))
        case raise: Raise.WithoutAction[StateGet, StateSet] => underlying.handleRaise(raise)

  }

  final case class HandleRaise[Env: HasNoScope, Action1, Action2, StateGet, StateSet <: StateGet](
      underlying: RaiseHandler[Any, Action2, StateGet, StateSet],
      state: StateGet,
      f: (Action1, StateGet, RaiseHandler[Any, Action2, StateGet, StateSet]) => ZIO[Env & Scope, UIError, Unit],
  ) extends RaiseHandler[Env, Action1, StateGet, StateSet] {

    override def handleRaise(raise: Raise[Action1, StateGet, StateSet]): ZIO[Env & Scope, UIError, Unit] =
      raise match
        case Raise.Action(action)                           => f(action, state, underlying)
        case raise: Raise.WithoutAction[StateGet, StateSet] => underlying.handleRaise(raise)

  }

  private[web] final case class Root[Env: HasNoScope, P, S](
      page: Page.AuxE[Env, P, S],
      pageInstance: PageInstance[P, S],
      uiRuntime: UIRuntime[Env],
  ) extends RaiseHandler.StatefulE[Env, Nothing, S] {

    override def handleRaise(raise: Raise.Stateful[Nothing, S]): ZIO[Env & Scope, UIError, Unit] =
      raise match {
        case Raise.UpdateState(value, true)  => ZIO.succeed { page.InternalPageState.getValue(pageInstance.pageReference).modify(value) } *> page.render(this, NavigationEvent.NavType.Replace)
        case Raise.SetState(value, true)     => ZIO.succeed { page.InternalPageState.getValue(pageInstance.pageReference).set(value) } *> page.render(this, NavigationEvent.NavType.Replace)
        case Raise.UpdateState(value, false) => ZIO.succeed { page.InternalPageState.getValue(pageInstance.pageReference).modify(value) }
        case Raise.SetState(value, false)    => ZIO.succeed { page.InternalPageState.getValue(pageInstance.pageReference).set(value) }

        case Raise.ReRender => page.render(this, NavigationEvent.NavType.Replace)

        case Raise.Action(_) => throw new RuntimeException("not possible...")
      }

  }

}
