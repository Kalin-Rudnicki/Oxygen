package oxygen.ui.web

import monocle.Lens
import monocle.macros.GenLens
import org.scalajs.dom.CanvasRenderingContext2D
import oxygen.predef.core.*
import oxygen.ui.web.component.Component
import oxygen.ui.web.internal.*
import scala.scalajs.js
import zio.*

sealed trait Widget[-Env, +Action, -StateGet, +StateSet <: StateGet] {

  private[web] def build[Env2 <: Env: HasNoScope](
      state: StateGet,
      rh: RaiseHandler[Env2, Action, StateGet, StateSet],
      pageExecutor: PageInstance.Untyped,
      uiRuntime: UIRuntime[Env2],
  ): Growable[DOMElement]

}
object Widget {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  inline def text(value: String): Widget.Const = Raw.text(value)

  inline def css(key: String, value: String): Widget.Const = Raw.css(key, value)
  inline def attr(key: String, value: String): Widget.Const = Raw.attr(key, value)
  inline def objectAttr(key: String, value: js.Any): Widget.Const = Raw.objectAttr(key, value)

  def foreach[S[_]: SeqRead, I, Env, Action, StateGet, StateSet <: StateGet](input: S[I])(
      f: I => Widget[Env, Action, StateGet, StateSet],
  ): Widget[Env, Action, StateGet, StateSet] =
    Foreach(input, f)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [Env, Action](self: Widget.Stateless[Env, Action]) {

    def withState[State]: Widget.Stateful[Env, Action, State] = self

    def handleAction: StatelessHandleActionBuilder[Env, Action, Nothing] = new StatelessHandleActionBuilder(self)
    def handleActionA[Action2]: StatelessHandleActionBuilder[Env, Action, Action2] = new StatelessHandleActionBuilder(self)

  }

  extension [Env, Action, State](self: Widget.Stateful[Env, Action, State]) {

    inline def zoomOut[OuterState](inline f: OuterState => State): Widget.Stateful[Env, Action, OuterState] =
      ZoomOut(self, GenLens[OuterState].apply(f).asInstanceOf[Lens[OuterState, State]])

    def handleActionS: StatefulHandleActionBuilder[Env, Action, State, Nothing] = new StatefulHandleActionBuilder(self)
    def handleActionAS[Action2]: StatefulHandleActionBuilder[Env, Action, State, Action2] = new StatefulHandleActionBuilder(self)

  }

  extension [Env, Action, StateGet, StateSet <: StateGet](self: Widget[Env, Action, StateGet, StateSet]) {

    def convertAction[Action2, StateGet2 >: StateSet <: StateGet, StateSet2 >: StateSet <: StateGet2](
        f: Action => Raise[Action2, StateGet2, StateSet2],
    ): Widget[Env, Action2, StateGet2, StateSet2] =
      Widget.ConvertAction(self, f)

  }

  final class StatelessHandleActionBuilder[Env, Action, Action2](self: Widget.Stateless[Env, Action]) {

    def apply[Env2 <: Env: HasNoScope](
        f: (Action, RaiseHandler.StatelessE[Env, Action2]) => ZIO[Env2 & Scope, UIError, Unit],
    ): Widget.Stateless[Env2, Action2] =
      Widget.HandleAction[Env2, Action, Any, Nothing, Action2](self, (a, _, rh) => f(a, rh))

  }

  final class StatefulHandleActionBuilder[Env, Action, State, Action2](self: Widget.Stateful[Env, Action, State]) {

    def apply[Env2 <: Env: HasNoScope](
        f: (Action, State, RaiseHandler.StatefulE[Env, Action2, State]) => ZIO[Env2 & Scope, UIError, Unit],
    ): Widget.Stateful[Env2, Action2, State] =
      Widget.HandleAction[Env2, Action, State, State, Action2](self, f)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  type Const = Stateless[Any, Nothing]
  type Stateless[-Env, +Action] = Widget[Env, Action, Any, Nothing]
  type StatefulReadOnly[-Env, +Action, -StateGet] = Widget[Env, Action, StateGet, Nothing]
  type Stateful[-Env, +Action, State] = Widget[Env, Action, State, State]

  private given unsafe: Unsafe = Unsafe.unsafe(identity)

  final case class ComponentWithProps[-Env, +Action, -StateGet, +StateSet <: StateGet, P](
      component: Component.WithProps[Env, Action, StateGet, StateSet] { type Props = P },
      props: P,
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      component.componentInternal(props, state).build(state, rh, pageExecutor, uiRuntime)

  }

  final case class ComponentWithoutProps[-Env, +Action, -StateGet, +StateSet <: StateGet](
      component: Component.WithoutProps[Env, Action, StateGet, StateSet],
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      component.componentInternal(state).build(state, rh, pageExecutor, uiRuntime)

  }

  final case class Foreach[S[_]: SeqRead, I, Env, Action, StateGet, StateSet <: StateGet](
      input: S[I],
      f: I => Widget[Env, Action, StateGet, StateSet],
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.many(input).flatMap(f(_).build[Env2](state, rh, pageExecutor, uiRuntime))

  }

  final case class Node[-Env, +Action, -StateGet, +StateSet <: StateGet](
      tag: String,
      children: Growable[Widget[Env, Action, StateGet, StateSet]],
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.Node(tag, children.flatMap(_.build(state, rh, pageExecutor, uiRuntime)).toArraySeq))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: Widget[Env2, Action2, StateGet2, StateSet2]*,
    ): Node[Env2, Action2, StateGet2, StateSet2] =
      if (addChildren.isEmpty) this
      else Node(tag, children ++ Growable.many(addChildren))

  }
  object Node {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Node[Env, Action, Any, Nothing]
    type StatefulReadOnly[-Env, +Action, -StateGet] = Node[Env, Action, StateGet, Nothing]
    type Stateful[-Env, +Action, State] = Node[Env, Action, State, State]

    def empty(tag: String): Node.Const = Node(tag, Growable.empty)

  }

  final case class Canvas[-Env, +Action, -StateGet, +StateSet <: StateGet](
      draw: CanvasRenderingContext2D => Unit,
      children: Growable[Widget[Env, Action, StateGet, StateSet]],
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.Canvas(draw, children.flatMap(_.build(state, rh, pageExecutor, uiRuntime)).toArraySeq))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: Widget[Env2, Action2, StateGet2, StateSet2]*,
    ): Canvas[Env2, Action2, StateGet2, StateSet2] =
      if (addChildren.isEmpty) this
      else Canvas(draw, children ++ Growable.many(addChildren))

  }
  object Canvas {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Canvas[Env, Action, Any, Nothing]
    type StatefulReadOnly[-Env, +Action, -StateGet] = Canvas[Env, Action, StateGet, Nothing]
    type Stateful[-Env, +Action, State] = Canvas[Env, Action, State, State]

  }

  final case class Fragment[-Env, +Action, -StateGet, +StateSet <: StateGet](
      elements: Growable[Widget[Env, Action, StateGet, StateSet]],
  ) extends Widget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      elements.flatMap(_.build(state, rh, pageExecutor, uiRuntime))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addElements: Widget[Env2, Action2, StateGet2, StateSet2]*,
    ): Fragment[Env2, Action2, StateGet2, StateSet2] =
      if (addElements.isEmpty) this
      else Fragment(elements ++ Growable.many(addElements))

  }
  object Fragment {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Fragment[Env, Action, Any, Nothing]
    type StatefulReadOnly[-Env, +Action, -StateGet] = Fragment[Env, Action, StateGet, Nothing]
    type Stateful[-Env, +Action, State] = Fragment[Env, Action, State, State]

    def empty: Fragment.Const = Fragment(Growable.empty)

  }

  // TODO (KR) : explore whether this can work
  /*
  final case class AppliedConstState[-Env, +Action, StateGet](
      underlying: Widget.StatefulReadOnly[Env, Action, StateGet],
      state: StateGet,
  ) extends Widget.Stateless[Env, Action] {

    override private[web] def build(state: Any, rh: RaiseHandler.Stateless[Action], runner: Runner[Env]): Growable[DOMElement] =
      underlying.build(this.state, rh, runner)

  }
   */

  final case class WithPageLocalState[-Env, +Action, State](
      underlying: Widget.Stateful[Env, Action, State],
      pageLocalState: PageLocalState[State],
  ) extends Widget.Stateless[Env, Action] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: Any,
        rh: RaiseHandler.StatelessE[Env2, Action],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val getState = pageLocalState.getValue(pageExecutor.pageReference).get()
      underlying.build(getState, RaiseHandler.WithPageLocalState(rh, pageLocalState, pageExecutor.pageReference), pageExecutor, uiRuntime)
    }

  }

  final case class WithGlobalState[-Env, +Action, State](
      underlying: Widget.Stateful[Env, Action, State],
      globalState: GlobalState[State],
  ) extends Widget.Stateless[Env, Action] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: Any,
        rh: RaiseHandler.StatelessE[Env2, Action],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying.build(globalState.getValue().get(), RaiseHandler.WithGlobalState(rh, globalState), pageExecutor, uiRuntime)

  }

  final case class Raw[E <: DOMElement](raw: E) extends Widget.Const {

    private val rawGrowable: Growable[E] = Growable.single(raw)

    override private[web] def build[Env2 <: Any: HasNoScope](
        state: Any,
        rh: RaiseHandler[Env2, Nothing, Any, Nothing],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      rawGrowable

  }
  object Raw {

    type Text = Widget.Raw[DOMElement.Text]
    type CSSAttr = Widget.Raw[DOMElement.CSSAttr]
    type HtmlAttr = Widget.Raw[DOMElement.HtmlAttr]
    type ObjectAttr = Widget.Raw[DOMElement.ObjectAttr]

    def text(value: String): Widget.Raw.Text = Raw(DOMElement.Text(value))

    def css(key: String, value: String): Widget.Raw.CSSAttr = Raw(DOMElement.CSSAttr(key, value))
    def attr(key: String, value: String): Widget.Raw.HtmlAttr = Raw(DOMElement.HtmlAttr(key, value))
    def objectAttr(key: String, value: js.Any): Widget.Raw.ObjectAttr = Raw(DOMElement.ObjectAttr(key, value))

  }

  final case class StatelessEventHandler[Env, Action, Event](
      key: String,
      handler: (RaiseHandler.Stateless[Action], Event) => ZIO[Env & Scope, UIError, Unit],
  ) extends Widget.Stateless[Env, Action] {

    private def handleEvent[Env2 <: Env: HasNoScope](
        rh: RaiseHandler.StatelessE[Env2, Action],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    )(e: Event): Unit =
      uiRuntime.unsafeExecute { pageExecutor.runForkedHandleErrors[Env2](RootErrorHandler.ErrorLocation.PageEvent(_)) { handler(rh.eraseEnv, e) } }

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: Any,
        rh: RaiseHandler.StatelessE[Env2, Action],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.ObjectAttr(key, (e: Event) => handleEvent[Env2](rh, pageExecutor, uiRuntime)(e)))

  }

  // TODO (KR) : may be worth exposing state in `handler:`
  final case class StatefulEventHandler[Env, Action, State, Event](
      key: String,
      handler: (RaiseHandler.Stateful[Action, State], Event) => ZIO[Env & Scope, UIError, Unit],
  ) extends Widget.Stateful[Env, Action, State] {

    private def handleEvent[Env2 <: Env: HasNoScope](
        rh: RaiseHandler.StatefulE[Env2, Action, State],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    )(e: Event): Unit =
      uiRuntime.unsafeExecute { pageExecutor.runForkedHandleErrors[Env2](RootErrorHandler.ErrorLocation.PageEvent(_)) { handler(rh.eraseEnv, e) } }

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: State,
        rh: RaiseHandler.StatefulE[Env2, Action, State],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.ObjectAttr(key, (e: Event) => handleEvent[Env2](rh, pageExecutor, uiRuntime)(e)))

  }

  final case class ZoomOut[Env, Action, InnerState, OuterState](
      underlying: Widget.Stateful[Env, Action, InnerState],
      lens: Lens[OuterState, InnerState],
  ) extends Widget.Stateful[Env, Action, OuterState] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: OuterState,
        rh: RaiseHandler[Env2, Action, OuterState, OuterState],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying.build(lens.get(state), RaiseHandler.ZoomIn(rh, lens), pageExecutor, uiRuntime)

  }

  final case class ConvertAction[Env, Action1, StateGet1, StateSet1 <: StateGet1, Action2, StateGet2 >: StateSet1 <: StateGet1, StateSet2 >: StateSet1 <: StateGet2](
      underlying: Widget[Env, Action1, StateGet1, StateSet1],
      f: Action1 => Raise[Action2, StateGet2, StateSet2],
  ) extends Widget[Env, Action2, StateGet2, StateSet2] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet2,
        rh: RaiseHandler[Env2, Action2, StateGet2, StateSet2],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying.build(state, RaiseHandler.ConvertRaise(rh, f), pageExecutor, uiRuntime)

  }

  final case class HandleAction[Env: HasNoScope, Action1, StateGet, StateSet <: StateGet, Action2](
      underlying: Widget[Env, Action1, StateGet, StateSet],
      f: (Action1, StateGet, RaiseHandler[Any, Action2, StateGet, StateSet]) => ZIO[Env & Scope, UIError, Unit],
  ) extends Widget[Env, Action2, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: StateGet,
        rh: RaiseHandler[Env2, Action2, StateGet, StateSet],
        pageExecutor: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying.build(state, RaiseHandler.HandleRaise[Env, Action1, Action2, StateGet, StateSet](rh.eraseEnv, state, f), pageExecutor, uiRuntime)

  }

}
