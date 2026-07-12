package oxygen.ui.web.internal

import java.util.UUID
import org.scalajs.dom
import oxygen.core.PlatformCompat
import scala.annotation.implicitNotFound
import zio.*

abstract class ForeignElement(@scala.annotation.unused reg: ForeignElement.Register) {

  final val instanceId: UUID = PlatformCompat.randomUUID()
  val name: String

  private var _state: ForeignElement.State = ForeignElement.State.Unmounted

  final def mount(stableId: String, element: dom.Element): Unit =
    _state match {
      case ForeignElement.State.Mounted(`stableId`) =>
        ()
      case ForeignElement.State.Unmounted =>
        this.mountInternal(stableId, element)
        this._state = ForeignElement.State.Mounted(stableId)
      case ForeignElement.State.Mounted(_) =>
        this.unmountInternal()
        this.mountInternal(stableId, element)
        this._state = ForeignElement.State.Mounted(stableId)
      case ForeignElement.State.Destroyed =>
        throw new RuntimeException(s"Attempted to mount destroyed ForeignElement($name)")
    }

  final def unmount(): Unit =
    _state match {
      case ForeignElement.State.Unmounted =>
        ()
      case ForeignElement.State.Mounted(_) =>
        this.unmountInternal()
        this._state = ForeignElement.State.Unmounted
      case ForeignElement.State.Destroyed =>
        throw new RuntimeException(s"Attempted to unmount destroyed ForeignElement($name)")
    }

  final def destroy(): Unit =
    _state match {
      case ForeignElement.State.Unmounted | ForeignElement.State.Mounted(_) =>
        this.destroyInternal()
        this._state = ForeignElement.State.Destroyed
      case ForeignElement.State.Destroyed =>
        ()
    }

  protected def mountInternal(stableId: String, element: dom.Element): Unit
  protected def unmountInternal(): Unit
  protected def destroyInternal(): Unit

}
object ForeignElement {

  enum State {
    case Unmounted
    case Mounted(stableId: String)
    case Destroyed
  }

  @implicitNotFound("Use `ForeignElement.register`")
  sealed trait Register
  object Register {
    private[ui] case object Instance extends Register
  }

  /** Allows for creation of a [[ForeignElement]]. Element will de [[ForeignElement.destroy]]ed upon [[Scope]] close. */
  def register(f: Register ?=> ForeignElement): URIO[Scope, ForeignElement] =
    ZIO.succeed { f(using Register.Instance) }.withFinalizer { elem => ZIO.succeed { elem.destroy() } }

}
