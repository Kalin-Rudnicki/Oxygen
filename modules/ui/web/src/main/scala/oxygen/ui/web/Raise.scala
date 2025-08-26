package oxygen.ui.web

import monocle.Lens

sealed trait Raise[+Action, -StateGet, +StateSet <: StateGet]
object Raise {

  def setState[State](state: State): Raise.Stateful[Nothing, State] = Raise.SetState(state, true)
  def updateState[State](state: State => State): Raise.Stateful[Nothing, State] = Raise.UpdateState(state, true)

  /////// Types ///////////////////////////////////////////////////////////////

  type Stateless[+Action] = Raise[Action, Any, Nothing]
  type Stateful[+Action, State] = Raise[Action, State, State]

  /////// Categories ///////////////////////////////////////////////////////////////

  sealed trait WithoutAction[-StateGet, +StateSet <: StateGet] extends Raise[Nothing, StateGet, StateSet]
  sealed trait WithoutState[+Action] extends Raise[Action, Any, Nothing]
  sealed trait WithState[S] extends Raise[Nothing, S, S], WithoutAction[S, S]

  ///////  ///////////////////////////////////////////////////////////////

  final case class Action[+A](action: A) extends WithoutState[A]

  final case class SetState[S](value: S, reRender: Boolean) extends WithState[S]
  final case class UpdateState[S](value: S => S, reRender: Boolean) extends WithState[S]

  sealed trait Invariant extends WithoutState[Nothing], WithoutAction[Any, Nothing]

  case object ReRender extends Raise.Invariant

  ///////  ///////////////////////////////////////////////////////////////

  extension [A, InnerState](raise: Raise.Stateful[A, InnerState])
    def zoomOut[OuterState](lens: Lens[OuterState, InnerState]): Raise.Stateful[A, OuterState] =
      raise match
        case action: Raise.Action[A]         => action
        case SetState(value, reRender)       => UpdateState(lens.replace(value), reRender)
        case action: UpdateState[InnerState] => UpdateState(lens.modify(action.value), action.reRender)
        case action: Invariant               => action

}
