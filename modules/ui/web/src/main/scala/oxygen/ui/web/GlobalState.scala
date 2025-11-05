package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.create.*
import oxygen.ui.web.internal.*
import zio.*

/**
  * Global state that is shared by all pages.
  */
abstract class GlobalState[S](final val name: String)(initial: => S) {
  private val stateId: UUID = PlatformCompat.randomUUID()
  private[web] val stateReference: StateReference = StateReference(stateId, name)

  private[web] final def getValue(): GlobalStateManager.Value[S] =
    GlobalStateManager.getGlobal(stateReference, initial)

  def attach[Env, Action](f: WidgetState[S] => WidgetEAS[Env, Action, S]): WidgetEA[Env, Action] =
    Widget.state[S].attach(this)(f)
  def detach[Env, Action, InnerStateGet, InnerStateSet <: InnerStateGet](
      f: WidgetState[S] => Widget.Polymorphic[Env, Action, InnerStateGet, InnerStateSet],
  ): Widget.Polymorphic[Env, Action, InnerStateGet, InnerStateSet] =
    Widget.state[S].detach(this)(f)

  final def toState(using pageInstance: PageInstance.Untyped): WidgetState[S] = WidgetState.GlobalValue.fromGlobalState(this, pageInstance)

  final def get: UIO[S] = ZIO.succeed { getValue().get() }
  final def set(value: S): UIO[Unit] = PageManager.currentPageInstance.flatMap { pageInstance => toState(using pageInstance).set(value) }
  final def update(f: S => S): UIO[Unit] = PageManager.currentPageInstance.flatMap { pageInstance => toState(using pageInstance).update(f) }

}
