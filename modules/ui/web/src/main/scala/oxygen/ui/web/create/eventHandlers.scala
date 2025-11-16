package oxygen.ui.web.create

import org.scalajs.dom.{Window as _, *}
import oxygen.ui.web.{NonRoutablePage, PageURL, PWidget, RaiseHandler, RoutablePage, UIError, WidgetState}
import oxygen.ui.web.service.Window
import zio.*

object onClick extends EventHandlerBuilder[MouseEvent]("onclick") {

  def push(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Widget =
    push(page)(ev(()))
  def push(page: RoutablePage[?])(params: page.PageParams): Widget = {
    def url: PageURL = page.paramCodec.encode(params)

    fragment(
      this.e.handle { e =>
        if (e.ctrlKey || e.metaKey || e.button == 1) Window.newTab(url)
        else if (e.shiftKey) Window.newWindow(url) // unfortunately seems no way to force this, so will behave the same as `newTab`
        else page.navigate.push(params)
      },
      onAuxClick.handle {
        Window.newTab(url)
      },
    )
  }

  def replace(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Widget =
    replace(page)(ev(()))
  def replace(page: RoutablePage[?])(params: page.PageParams): Widget = {
    def url: PageURL = page.paramCodec.encode(params)

    fragment(
      this.e.handle { e =>
        if (e.ctrlKey || e.metaKey || e.button == 1) Window.newTab(url)
        else if (e.shiftKey) Window.newWindow(url) // unfortunately seems no way to force this, so will behave the same as `newTab`
        else page.navigate.replace(params)
      },
      onAuxClick.handle {
        Window.newTab(url)
      },
    )
  }

  def render[Env](page: NonRoutablePage[Env])(using ev: Unit <:< page.PageParams): WidgetE[Env] =
    render(page)(ev(()))
  def render[Env](page: NonRoutablePage[Env])(params: page.PageParams): WidgetE[Env] =
    this := page.navigate.render(params)

}

object onContextMenu extends EventHandlerBuilder[MouseEvent]("oncontextmenu")

object onAuxClick extends EventHandlerBuilder[MouseEvent]("onauxclick")

object onDblClick extends EventHandlerBuilder[MouseEvent]("ondblclick")
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
object onInput extends EventHandlerBuilder[Event]("oninput")
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

  final def :=[Env](f: => ZIO[Env & Scope, UIError, Unit]): WidgetE[Env] = PWidget.StatelessEventHandler[Env, Nothing, Event](key, (_, _) => f)
  final def handle[Env](f: => ZIO[Env & Scope, UIError, Unit]): WidgetE[Env] = PWidget.StatelessEventHandler[Env, Nothing, Event](key, (_, _) => f)

  final def a[Action]: EventHandlerBuilder.A[Event, Action] = EventHandlerBuilder.A(key)
  final def s[State]: EventHandlerBuilder.S[Event, State] = EventHandlerBuilder.S(key)
  final def as[Action, State]: EventHandlerBuilder.AS[Event, Action, State] = EventHandlerBuilder.AS(key)

  final def e: EventHandlerBuilder.E[Event] = EventHandlerBuilder.E(key)
  final def ea[Action]: EventHandlerBuilder.EA[Event, Action] = EventHandlerBuilder.EA(key)
  final def es[State]: EventHandlerBuilder.ES[Event, State] = EventHandlerBuilder.ES(key)
  final def eas[Action, State]: EventHandlerBuilder.EAS[Event, Action, State] = EventHandlerBuilder.EAS(key)

  // format: off

  final def action[Action](       a:   Action           ): WidgetA[ Action] =        this.a[Action].action(a)
  final def setState[State](      s:   State            ): WidgetS[State] = this.s[State].setState(s)
  final def updateState[State](   s:   State => State   ): WidgetS[State] = this.s[State].updateState(s)

  // format: on

}
object EventHandlerBuilder {

  // format: off

  /////// Without Event ///////////////////////////////////////////////////////////////

  final class A[Event, Action](key: String) {
    def :=[Env](       f:   RaiseHandler[Any, Action]   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEA[Env, Action] = PWidget.StatelessEventHandler[Env, Action, Event](key, (rh, _) => f(rh))
    def handle[Env](   f:   RaiseHandler[Any, Action]   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEA[Env, Action] = PWidget.StatelessEventHandler[Env, Action, Event](key, (rh, _) => f(rh))

    def action(   act:   Action   ): WidgetA[Action] = handle(_.raiseAction(act))
  }

  final class S[Event, State](key: String) {
    def :=[Env](       f:   WidgetState[State]   => ZIO[Env & Scope, UIError, Unit]   ): WidgetES[Env, State] = PWidget.StatefulEventHandler[Env, Nothing, State, Event](key, (s, _, _) => f(s))
    def handle[Env](   f:   WidgetState[State]   => ZIO[Env & Scope, UIError, Unit]   ): WidgetES[Env, State] = PWidget.StatefulEventHandler[Env, Nothing, State, Event](key, (s, _, _) => f(s))

    def setState(      s:   State            ): WidgetS[State] = handle(_.set(s))
    def updateState(   s:   State => State   ): WidgetS[State] = handle(_.update(s))
  }

  final class AS[Event, Action, State](key: String) {
    def :=[Env](       f:   (WidgetState[State], RaiseHandler[Any, Action])   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEAS[Env, Action, State] = PWidget.StatefulEventHandler[Env, Action, State, Event](key, (s, rh, _) => f(s, rh))
    def handle[Env](   f:   (WidgetState[State], RaiseHandler[Any, Action])   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEAS[Env, Action, State] = PWidget.StatefulEventHandler[Env, Action, State, Event](key, (s, rh, _) => f(s, rh))
  }

  /////// With Event ///////////////////////////////////////////////////////////////

  final class E[Event](key: String) {
    def :=[Env](       f:   Event   => ZIO[Env & Scope, UIError, Unit]   ): WidgetE[Env] = PWidget.StatelessEventHandler[Env, Nothing, Event](key, (_, e) => f(e))
    def handle[Env](   f:   Event   => ZIO[Env & Scope, UIError, Unit]   ): WidgetE[Env] = PWidget.StatelessEventHandler[Env, Nothing, Event](key, (_, e) => f(e))
  }

  final class EA[Event, Action](key: String) {
    def :=[Env](       f:   (RaiseHandler[Any, Action], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEA[Env, Action] = PWidget.StatelessEventHandler[Env, Action, Event](key, f)
    def handle[Env](   f:   (RaiseHandler[Any, Action], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEA[Env, Action] = PWidget.StatelessEventHandler[Env, Action, Event](key, f)
  }

  final class ES[Event, State](key: String) {
    def :=[Env](       f:   (WidgetState[State], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetES[Env, State] = PWidget.StatefulEventHandler[Env, Nothing, State, Event](key, (s, _, e) => f(s, e))
    def handle[Env](   f:   (WidgetState[State], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetES[Env, State] = PWidget.StatefulEventHandler[Env, Nothing, State, Event](key, (s, _, e) => f(s, e))
  }

  final class EAS[Event, Action, State](key: String) {
    def :=[Env](       f:   (WidgetState[State], RaiseHandler[Any, Action], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEAS[Env, Action, State] = PWidget.StatefulEventHandler[Env, Action, State, Event](key, f)
    def handle[Env](   f:   (WidgetState[State], RaiseHandler[Any, Action], Event)   => ZIO[Env & Scope, UIError, Unit]   ): WidgetEAS[Env, Action, State] = PWidget.StatefulEventHandler[Env, Action, State, Event](key, f)
  }

  // format: on

}
