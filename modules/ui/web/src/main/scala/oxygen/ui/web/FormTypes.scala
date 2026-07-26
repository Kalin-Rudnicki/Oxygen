package oxygen.ui.web

trait FormTypes[F[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] <: PForm[Env, Action, StateGet, StateSet, Value]] {

  final type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = F[Env, Action, StateGet, StateSet, Value]
  final type Stateful[-Env, +Action, State, +Value] = F[Env, Action, State, State, Value]
  final type Stateless[-Env, +Action, +Value] = F[Env, Action, Any, Nothing, Value]

  final type EAS[-Env, +Action, State, +Value] = F[Env, Action, State, State, Value]
  final type ES[-Env, State, +Value] = F[Env, Nothing, State, State, Value]
  final type AS[+Action, State, +Value] = F[Any, Action, State, State, Value]
  final type S[State, +Value] = F[Any, Nothing, State, State, Value]

  final type EA[-Env, +Action, +Value] = F[Env, Action, Any, Nothing, Value]
  final type E[-Env, +Value] = F[Env, Nothing, Any, Nothing, Value]
  final type A[+Action, +Value] = F[Any, Action, Any, Nothing, Value]
  final type Const[+Value] = F[Any, Nothing, Any, Nothing, Value]

}
