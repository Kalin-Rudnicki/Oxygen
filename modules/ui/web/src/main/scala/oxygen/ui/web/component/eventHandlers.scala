package oxygen.ui.web.component

import org.scalajs.dom.*
import oxygen.ui.web.{RaiseHandler, UIError, Widget}
import zio.*

object onClick extends EventHandlerBuilder[MouseEvent]("onclick")
object onDblClick extends EventHandlerBuilder[MouseEvent]("ondblclick")
object onContextMenu extends EventHandlerBuilder[MouseEvent]("oncontextmenu")
object onMouseDown extends EventHandlerBuilder[MouseEvent]("onmousedown")
object onMouseUp extends EventHandlerBuilder[MouseEvent]("onmouseup")
object onMouseOver extends EventHandlerBuilder[MouseEvent]("onmouseover")
object onMouseMove extends EventHandlerBuilder[MouseEvent]("onmousemove")
object onMouseOut extends EventHandlerBuilder[MouseEvent]("onmouseout")
object onMouseEnter extends EventHandlerBuilder[MouseEvent]("onmouseenter")
object onMouseLeave extends EventHandlerBuilder[MouseEvent]("onmouseleave")

object onKeyDown extends EventHandlerBuilder[KeyboardEvent]("onkeydown")
object onKeyUp extends EventHandlerBuilder[KeyboardEvent]("onkeyup")
object onKeyPress extends EventHandlerBuilder[KeyboardEvent]("onkeypress")

object onChange extends EventHandlerBuilder[Event]("onchange")
object onSubmit extends EventHandlerBuilder[Event]("onsubmit")
object onReset extends EventHandlerBuilder[Event]("onreset")

object onFocus extends EventHandlerBuilder[FocusEvent]("onfocus")
object onBlur extends EventHandlerBuilder[FocusEvent]("onblur")

object onDrag extends EventHandlerBuilder[DragEvent]("ondrag")
object onDragStart extends EventHandlerBuilder[DragEvent]("ondragstart")
object onDragEnd extends EventHandlerBuilder[DragEvent]("ondragend")
object onDragEnter extends EventHandlerBuilder[DragEvent]("ondragenter")
object onDragLeave extends EventHandlerBuilder[DragEvent]("ondragleave")
object onDragOver extends EventHandlerBuilder[DragEvent]("ondragover")
object onDrop extends EventHandlerBuilder[DragEvent]("ondrop")

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builder(s)
//////////////////////////////////////////////////////////////////////////////////////////////////////

sealed abstract class EventHandlerBuilder[Event](key: String) {

  final def :=[Env](f: => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Nothing] = Widget.StatelessEventHandler[Env, Nothing, Event](key, (_, _) => f)
  final def handle[Env](f: => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Nothing] = Widget.StatelessEventHandler[Env, Nothing, Event](key, (_, _) => f)

  final def a[Action]: EventHandlerBuilder.A[Event, Action] = EventHandlerBuilder.A(key)
  final def s[State]: EventHandlerBuilder.S[Event, State] = EventHandlerBuilder.S(key)
  final def as[Action, State]: EventHandlerBuilder.AS[Event, Action, State] = EventHandlerBuilder.AS(key)

  final def r: EventHandlerBuilder.A[Event, Nothing] = EventHandlerBuilder.A(key)
  final def rs[State]: EventHandlerBuilder.AS[Event, Nothing, State] = EventHandlerBuilder.AS(key)

  final def e: EventHandlerBuilder.E[Event] = EventHandlerBuilder.E(key)
  final def ea[Action]: EventHandlerBuilder.EA[Event, Action] = EventHandlerBuilder.EA(key)
  final def es[State]: EventHandlerBuilder.ES[Event, State] = EventHandlerBuilder.ES(key)
  final def eas[Action, State]: EventHandlerBuilder.EAS[Event, Action, State] = EventHandlerBuilder.EAS(key)

  final def er: EventHandlerBuilder.EA[Event, Nothing] = EventHandlerBuilder.EA(key)
  final def ers[State]: EventHandlerBuilder.EAS[Event, Nothing, State] = EventHandlerBuilder.EAS(key)

  final def action[Action](act: Action): Widget.Stateless[Any, Action] = a[Action].action(act)

}
object EventHandlerBuilder {

  // format: off

  /////// Without Event ///////////////////////////////////////////////////////////////

  final class A[Event, Action](key: String) {
    def :=[Env](f: RaiseHandler.Stateless[Action] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Action] = Widget.StatelessEventHandler[Env, Action, Event](key, (rh, _) => f(rh))
    def handle[Env](f: RaiseHandler.Stateless[Action] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Action] = Widget.StatelessEventHandler[Env, Action, Event](key, (rh, _) => f(rh))

    def action(act: Action): Widget.Stateless[Any, Action] = handle(_.raiseAction(act))
  }

  final class S[Event, State](key: String) {
    def :=[Env](f: RaiseHandler.Stateful[Nothing, State] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Nothing, State] = Widget.StatefulEventHandler[Env, Nothing, State, Event](key, (rh, _) => f(rh))
    def handle[Env](f: RaiseHandler.Stateful[Nothing, State] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Nothing, State] = Widget.StatefulEventHandler[Env, Nothing, State, Event](key, (rh, _) => f(rh))

    def setState(s: State): Widget.Stateful[Any, Nothing, State] = handle(_.setState(s))
    def updateState(s: State => State): Widget.Stateful[Any, Nothing, State] = handle(_.updateState(s))
  }

  final class AS[Event, Action, State](key: String) {
    def :=[Env](f: RaiseHandler.Stateful[Action, State] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Action, State] = Widget.StatefulEventHandler[Env, Action, State, Event](key, (rh, _) => f(rh))
    def handle[Env](f: RaiseHandler.Stateful[Action, State] => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Action, State] = Widget.StatefulEventHandler[Env, Action, State, Event](key, (rh, _) => f(rh))
  }

  /////// With Event ///////////////////////////////////////////////////////////////

  final class E[Event](key: String) {
    def :=[Env](f: Event => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Nothing] = Widget.StatelessEventHandler[Env, Nothing, Event](key, (_, e) => f(e))
    def handle[Env](f: Event => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Nothing] = Widget.StatelessEventHandler[Env, Nothing, Event](key, (_, e) => f(e))
  }

  final class EA[Event, Action](key: String) {
    def :=[Env](f: (RaiseHandler.Stateless[Action], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Action] = Widget.StatelessEventHandler[Env, Action, Event](key, f)
    def handle[Env](f: (RaiseHandler.Stateless[Action], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateless[Env, Action] = Widget.StatelessEventHandler[Env, Action, Event](key, f)
  }

  final class ES[Event, State](key: String) {
    def :=[Env](f: (RaiseHandler.Stateful[Nothing, State], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Nothing, State] = Widget.StatefulEventHandler[Env, Nothing, State, Event](key, f)
    def handle[Env](f: (RaiseHandler.Stateful[Nothing, State], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Nothing, State] = Widget.StatefulEventHandler[Env, Nothing, State, Event](key, f)
  }

  final class EAS[Event, Action, State](key: String) {
    def :=[Env](f: (RaiseHandler.Stateful[Action, State], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Action, State] = Widget.StatefulEventHandler[Env, Action, State, Event](key, f)
    def handle[Env](f: (RaiseHandler.Stateful[Action, State], Event) => ZIO[Env & Scope, UIError, Unit]): Widget.Stateful[Env, Action, State] = Widget.StatefulEventHandler[Env, Action, State, Event](key, f)
  }

  // format: on

}
