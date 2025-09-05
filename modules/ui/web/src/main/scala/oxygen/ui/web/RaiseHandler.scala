package oxygen.ui.web

import zio.*

trait RaiseHandler[-Env: HasNoScope, -Action] {

  def raiseAction(raise: Action): ZIO[Env & Scope, UIError, Unit]

  // WARNING! Use with caution! This should be used in as few places as possible!
  private[web] final def eraseEnv: RaiseHandler[Any, Action] = this.asInstanceOf[RaiseHandler[Any, Action]]

}
object RaiseHandler {

  case object Empty extends RaiseHandler[Any, Nothing] {
    override def raiseAction(raise: Nothing): ZIO[Scope, UIError, Unit] = ZIO.dieMessage("empty raise handler")
  }

  final case class FromFunction[Env: HasNoScope, Action](f: Action => ZIO[Env & Scope, UIError, Unit]) extends RaiseHandler[Env, Action] {
    override def raiseAction(raise: Action): ZIO[Env & Scope, UIError, Unit] = f(raise)
  }

  final case class HandleAction[Env: HasNoScope, Action, Action2](
      underlying: RaiseHandler[Any, Action2],
      f: (RaiseHandler[Any, Action2], Action) => ZIO[Env & Scope, UIError, Unit],
  ) extends RaiseHandler[Env, Action] {
    override def raiseAction(raise: Action): ZIO[Env & Scope, UIError, Unit] = f(underlying, raise)
  }

}
