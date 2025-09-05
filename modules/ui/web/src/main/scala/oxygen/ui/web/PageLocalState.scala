package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.create.*
import oxygen.ui.web.internal.*
import zio.*

/**
  * Global state that is scoped to a single page instance.
  */
abstract class PageLocalState[S](final val name: String)(initial: => S) {
  private val stateId: UUID = PlatformCompat.randomUUID()
  private[web] final val stateReference: StateReference = StateReference(stateId, name)

  private[web] final def getValue(pageReference: PageReference): GlobalStateManager.Value[S] =
    GlobalStateManager.getPageLocal(pageReference).getValue(stateReference, initial)

  def attach[Env, Action](f: WidgetState[S] => WidgetEAS[Env, Action, S]): WidgetEA[Env, Action] =
    Widget.state[S].attach(this)(f)
  def detach[Env, Action, InnerStateGet, InnerStateSet <: InnerStateGet](
      f: WidgetState[S] => Widget.Polymorphic[Env, Action, InnerStateGet, InnerStateSet],
  ): Widget.Polymorphic[Env, Action, InnerStateGet, InnerStateSet] =
    Widget.state[S].detach(this)(f)

  final def get(pageInstance: PageInstance.Untyped): UIO[S] = ZIO.succeed { getValue(pageInstance.pageReference).get() }
  final def set(pageInstance: PageInstance.Untyped)(value: S): UIO[Unit] = WidgetState.GlobalValue.fromPageLocalState(this, pageInstance).set(value)
  final def update(pageInstance: PageInstance.Untyped)(f: S => S): UIO[Unit] = WidgetState.GlobalValue.fromPageLocalState(this, pageInstance).update(f)

}
