package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.internal.{GlobalStateManager, PageManager, StateReference}
import zio.*

/**
  * Global state that is shared by all pages.
  */
abstract class GlobalState[S](final val name: String)(initial: => S) {
  private val stateId: UUID = PlatformCompat.randomUUID()

  private val stateReference: StateReference = StateReference(stateId, name)

  private[web] final def getValue(): GlobalStateManager.Value[S] =
    GlobalStateManager.getGlobal(stateReference, initial)

  final def get: UIO[S] = ZIO.succeed { unsafeGet } <* PageManager.reRenderCurrentPage
  final def set(value: S): UIO[Unit] = ZIO.succeed { unsafeSet(value) } <* PageManager.reRenderCurrentPage
  final def update(f: S => S): UIO[Unit] = ZIO.succeed { unsafeUpdate(f) } <* PageManager.reRenderCurrentPage

  final def unsafeGet(using Unsafe): S = getValue().get()
  final def unsafeSet(value: S)(using Unsafe): Unit = getValue().set(value)
  final def unsafeUpdate(f: S => S)(using Unsafe): Unit = getValue().modify(f)

}
