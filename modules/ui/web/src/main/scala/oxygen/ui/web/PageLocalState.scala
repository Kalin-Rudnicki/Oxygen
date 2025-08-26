package oxygen.ui.web

import java.util.UUID
import oxygen.predef.core.*
import oxygen.ui.web.internal.{GlobalStateManager, PageManager, PageReference, StateReference}
import zio.*

/**
  * Global state that is scoped to a single page instance.
  */
abstract class PageLocalState[S](final val name: String)(initial: => S) {
  private val stateId: UUID = PlatformCompat.randomUUID()

  private[web] final val stateReference: StateReference = StateReference(stateId, name)

  private[web] final def getValue(pageReference: PageReference): GlobalStateManager.Value[S] =
    GlobalStateManager.getPageLocal(pageReference).getValue(stateReference, initial)

  final def get(pageReference: PageReference): UIO[S] = ZIO.succeed { unsafeGet(pageReference) } <* PageManager.reRenderCurrentPage
  final def set(pageReference: PageReference)(value: S): UIO[Unit] = ZIO.succeed { unsafeSet(pageReference)(value) } <* PageManager.reRenderCurrentPage
  final def update(pageReference: PageReference)(f: S => S): UIO[Unit] = ZIO.succeed { unsafeUpdate(pageReference)(f) } <* PageManager.reRenderCurrentPage

  final def getCurrentPage: UIO[S] = PageManager.currentPageRef.flatMap(get(_))
  final def setCurrentPage(value: S): UIO[Unit] = PageManager.currentPageRef.flatMap(set(_)(value))
  final def updateCurrentPage(f: S => S): UIO[Unit] = PageManager.currentPageRef.flatMap(update(_)(f))

  final def unsafeGet(pageReference: PageReference)(using Unsafe): S = getValue(pageReference).get()
  final def unsafeSet(pageReference: PageReference)(value: S)(using Unsafe): Unit = getValue(pageReference).set(value)
  final def unsafeUpdate(pageReference: PageReference)(f: S => S)(using Unsafe): Unit = getValue(pageReference).modify(f)

}
