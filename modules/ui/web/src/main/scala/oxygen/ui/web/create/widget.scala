package oxygen.ui.web.create

import org.scalajs.dom.CanvasRenderingContext2D
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.internal.{DOMElement, PageInstance}
import scalajs.js

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Types
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Widget.Stateful
type WidgetEAS[-Env, +Action, State] = Widget.Stateful[Env, Action, State]
type WidgetES[-Env, State] = Widget.Stateful[Env, Nothing, State]
type WidgetAS[+Action, State] = Widget.Stateful[Any, Action, State]
type WidgetS[State] = Widget.Stateful[Any, Nothing, State]

// Widget.Stateless
type WidgetEA[-Env, +Action] = Widget.Stateless[Env, Action]
type WidgetE[-Env] = Widget.Stateless[Env, Nothing]
type WidgetA[+Action] = Widget.Stateless[Any, Action]
type Widget = Widget.Stateless[Any, Nothing]

// Widget.Fragment.Stateful
type FragmentEAS[-Env, +Action, State] = Fragment.Stateful[Env, Action, State]
type FragmentES[-Env, State] = Fragment.Stateful[Env, Nothing, State]
type FragmentAS[+Action, State] = Fragment.Stateful[Any, Action, State]
type FragmentS[State] = Fragment.Stateful[Any, Nothing, State]

// Widget.Fragment.Stateless
type FragmentEA[-Env, +Action] = Fragment.Stateless[Env, Action]
type FragmentE[-Env] = Fragment.Stateless[Env, Nothing]
type FragmentA[+Action] = Fragment.Stateless[Any, Action]
type Fragment = Fragment.Stateless[Any, Nothing]

// Widget.Node.Stateful
type NodeEAS[-Env, +Action, State] = Node.Stateful[Env, Action, State]
type NodeES[-Env, State] = Node.Stateful[Env, Nothing, State]
type NodeAS[+Action, State] = Node.Stateful[Any, Action, State]
type NodeS[State] = Node.Stateful[Any, Nothing, State]

// Widget.Node.Stateless
type NodeEA[-Env, +Action] = Node.Stateless[Env, Action]
type NodeE[-Env] = Node.Stateless[Env, Nothing]
type NodeA[+Action] = Node.Stateless[Any, Action]
type Node = Node.Stateless[Any, Nothing]

// Widget.Canvas.Stateful
type CanvasEAS[-Env, +Action, State] = Canvas.Stateful[Env, Action, State]
type CanvasES[-Env, State] = Canvas.Stateful[Env, Nothing, State]
type CanvasAS[+Action, State] = Canvas.Stateful[Any, Action, State]
type CanvasS[State] = Canvas.Stateful[Any, Nothing, State]

// Widget.Canvas.Stateless
type CanvasEA[-Env, +Action] = Canvas.Stateless[Env, Action]
type CanvasE[-Env] = Canvas.Stateless[Env, Nothing]
type CanvasA[+Action] = Canvas.Stateless[Any, Action]
type Canvas = Canvas.Stateless[Any, Nothing]

// Raw
type RawWidget[E <: DOMElement] = PWidget.Raw[E]
type Text = RawWidget[DOMElement.Text]
type CSSAttr = RawWidget[DOMElement.CSSAttr]
type HtmlAttr = RawWidget[DOMElement.HtmlAttr]
type ObjectAttr = RawWidget[DOMElement.ObjectAttr]
type ClassAttr = RawWidget[DOMElement.ClassAttr]

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builders
//////////////////////////////////////////////////////////////////////////////////////////////////////

object Widget {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet] = PWidget[Env, Action, StateGet, StateSet]
  type Stateful[-Env, +Action, State] = Widget.Polymorphic[Env, Action, State, State]
  type Stateless[-Env, +Action] = Widget.Polymorphic[Env, Action, Any, Nothing]
  type Const = Widget.Polymorphic[Any, Nothing, Any, Nothing]

  def text(value: String): Widget = Widget.raw.text(value)
  def css(key: String, value: String): Widget = Widget.raw.css(key, value)
  def htmlAttr(key: String, value: String): Widget = Widget.raw.htmlAttr(key, value)
  def objectAttr(key: String, value: js.Any): Widget = Widget.raw.objectAttr(key, value)
  def `class`(classes: String*): Widget = Widget.raw.`class`(classes*)
  def `class`(classes: Set[String]): Widget = Widget.raw.`class`(classes)

  val empty: Widget = PWidget.Empty

  val fragment: Fragment = PWidget.Fragment.empty

  def node(xmlns: String, tag: String): Node = PWidget.Node.empty(xmlns, tag)
  def node(tag: String): Node = PWidget.Node.empty(tag)

  def canvas(draw: CanvasRenderingContext2D => Unit): Canvas = Canvas(draw)

  def when[Env, Action, StateGet, StateSet <: StateGet](cond: Boolean)(widget: Widget.Polymorphic[Env, Action, StateGet, StateSet]): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    if (cond) widget
    else Widget.empty
  def unless[Env, Action, StateGet, StateSet <: StateGet](cond: Boolean)(widget: Widget.Polymorphic[Env, Action, StateGet, StateSet]): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    if (cond) Widget.empty
    else widget

  def fragment[S[_]: SeqRead, Env, Action, StateGet, StateSet <: StateGet](widgets: S[Widget.Polymorphic[Env, Action, StateGet, StateSet]]): Fragment.Polymorphic[Env, Action, StateGet, StateSet] =
    PWidget.Fragment(Growable.many(widgets))

  def intersperse[S[_]: SeqOps, Env, Action, StateGet, StateSet <: StateGet](
      widgets: S[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
      join: Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Fragment.Polymorphic[Env, Action, StateGet, StateSet] =
    Widget.fragment(widgets.intersperse(join))

  def foreach[S[_]: SeqRead, I, Env, Action, StateGet, StateSet <: StateGet](input: S[I])(
      f: I => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    PWidget.Foreach(input, f)

  def foreach[I, Env, Action, StateGet, StateSet <: StateGet](input0: I, input1: I, inputN: I*)(
      f: I => Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    foreach(input0 :: input1 :: inputN.toList)(f)

  def state[S]: widgetBuilders.WidgetStateBuilder[S] = widgetBuilders.WidgetStateBuilder[S]()

  def seq[F[_]: PWidget.Sequence.Ops as ops]: widgetBuilders.SequenceBuilder[F] = widgetBuilders.SequenceBuilder(ops)

  object sum {

    def build[State]: PWidget.SumCases.ParentBuilder[State] = new PWidget.SumCases.ParentBuilder

    def option[Env, Action, State](
        some: PWidget.Stateful[Env, Action, State],
        none: PWidget.Stateless[Env, Action],
    ): WidgetEAS[Env, Action, Option[State]] =
      sum.build[Option[State]](
        _.subType[Some[State]].zoomIn(_.value)(some),
        _.subType[None.type](none),
      )

    def option[Env, Action, State](
        some: PWidget.Stateful[Env, Action, State],
    ): WidgetEAS[Env, Action, Option[State]] =
      option(some, PWidget.Empty)

    def either[Env, Action, L, R](
        left: PWidget.Stateful[Env, Action, L],
        right: PWidget.Stateful[Env, Action, R],
    ): WidgetEAS[Env, Action, Either[L, R]] =
      sum.build[Either[L, R]](
        _.subType[Left[L, R]].zoomIn(_.value)(left),
        _.subType[Right[L, R]].zoomIn(_.value)(right),
      )

  }

  object raw {

    def apply[E <: DOMElement](value: E): RawWidget[E] = PWidget.Raw(value)

    def text(value: String): Text = Widget.raw(DOMElement.Text(value))
    def css(key: String, value: String): CSSAttr = Widget.raw(DOMElement.CSSAttr(key, value))
    def htmlAttr(key: String, value: String): HtmlAttr = Widget.raw(DOMElement.HtmlAttr(key, value))
    def objectAttr(key: String, value: js.Any): ObjectAttr = Widget.raw(DOMElement.ObjectAttr(key, value))
    def `class`(classes: String*): ClassAttr = Widget.raw(DOMElement.ClassAttr(classes.toSet))
    def `class`(classes: Set[String]): ClassAttr = Widget.raw(DOMElement.ClassAttr(classes))

  }

  def withPageInstance[Env, Action, StateGet, StateSet <: StateGet](
      make: PageInstance.Untyped ?=> Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    PWidget.WithPageInstance(pi => make(using pi))

}

object Fragment {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet] = PWidget.Fragment[Env, Action, StateGet, StateSet]
  type Stateful[-Env, +Action, State] = Fragment.Polymorphic[Env, Action, State, State]
  type Stateless[-Env, +Action] = Fragment.Polymorphic[Env, Action, Any, Nothing]
  type Const = Fragment.Polymorphic[Any, Nothing, Any, Nothing]

  val empty: Fragment = PWidget.Fragment.empty

}

object Node {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet] = PWidget.Node[Env, Action, StateGet, StateSet]
  type Stateful[-Env, +Action, State] = Node.Polymorphic[Env, Action, State, State]
  type Stateless[-Env, +Action] = Node.Polymorphic[Env, Action, Any, Nothing]
  type Const = Node.Polymorphic[Any, Nothing, Any, Nothing]

  def apply(xmlns: String, tag: String): Node = PWidget.Node.empty(xmlns, tag)
  def apply(tag: String): Node = PWidget.Node.empty(tag)

}

object Canvas {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet] = PWidget.Canvas[Env, Action, StateGet, StateSet]
  type Stateful[-Env, +Action, State] = Canvas.Polymorphic[Env, Action, State, State]
  type Stateless[-Env, +Action] = Canvas.Polymorphic[Env, Action, Any, Nothing]
  type Const = Canvas.Polymorphic[Any, Nothing, Any, Nothing]

  def apply(draw: CanvasRenderingContext2D => Unit): Canvas = PWidget.Canvas(draw, Growable.empty)

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Builders - Internal
//////////////////////////////////////////////////////////////////////////////////////////////////////

object widgetBuilders {

  final class WidgetStateBuilder[State] {

    def fix[Env, Action](f: WidgetState[State] => WidgetEAS[Env, Action, State]): WidgetEAS[Env, Action, State] = PWidget.FixState(f)
    def get[Env, Action](f: State => WidgetEAS[Env, Action, State]): WidgetEAS[Env, Action, State] =
      fix { state => f(state.renderTimeValue) }
    def fixGet[Env, Action](f: (WidgetState[State], State) => WidgetEAS[Env, Action, State]): WidgetEAS[Env, Action, State] =
      fix { state => f(state, state.renderTimeValue) }

    def attach[Env, Action](b: PageLocalState[State])(f: WidgetState[State] => WidgetEAS[Env, Action, State]): WidgetEA[Env, Action] = fix(f).attach(b)
    def attach[Env, Action](b: GlobalState[State])(f: WidgetState[State] => WidgetEAS[Env, Action, State]): WidgetEA[Env, Action] = fix(f).attach(b)

    def detach[Env, Action, StateGet, StateSet <: StateGet](b: PageLocalState[State])(
        f: WidgetState[State] => Widget.Polymorphic[Env, Action, StateGet, StateSet],
    ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      PWidget.DetachPageLocalState(f, b)
    def detach[Env, Action, StateGet, StateSet <: StateGet](b: GlobalState[State])(
        f: WidgetState[State] => Widget.Polymorphic[Env, Action, StateGet, StateSet],
    ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      PWidget.DetachGlobalState(f, b)

  }

  final class SequenceBuilder[F[_]](ops: PWidget.Sequence.Ops[F]) {

    def apply[Env, Action, State](elemWidget: WidgetEAS[Env, Action, State]): WidgetEAS[Env, Action, F[State]] =
      PWidget.Sequence(_ => elemWidget, ops)

    def withIndex[Env, Action, State](elemWidget: Int => WidgetEAS[Env, Action, State]): WidgetEAS[Env, Action, F[State]] =
      PWidget.Sequence(elemWidget, ops)

  }

}
