package oxygen.ui.web

trait WidgetTypes[F[-Env, +Action, -StateGet, +StateSet <: StateGet] <: PWidget[Env, Action, StateGet, StateSet]] {

  final type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet] = F[Env, Action, StateGet, StateSet]
  final type Stateful[-Env, +Action, State] = F[Env, Action, State, State]
  final type Stateless[-Env, +Action] = F[Env, Action, Any, Nothing]

  final type EAS[-Env, +Action, State] = F[Env, Action, State, State]
  final type ES[-Env, State] = F[Env, Nothing, State, State]
  final type AS[+Action, State] = F[Any, Action, State, State]
  final type S[State] = F[Any, Nothing, State, State]

  final type EA[-Env, +Action] = F[Env, Action, Any, Nothing]
  final type E[-Env] = F[Env, Nothing, Any, Nothing]
  final type A[+Action] = F[Any, Action, Any, Nothing]
  final type Const = F[Any, Nothing, Any, Nothing]

}
