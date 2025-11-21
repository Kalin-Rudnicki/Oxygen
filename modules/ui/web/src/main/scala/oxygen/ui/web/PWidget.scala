package oxygen.ui.web

import monocle.Lens
import org.scalajs.dom.CanvasRenderingContext2D
import oxygen.meta.typing.UnionRemoving
import oxygen.predef.core.*
import oxygen.ui.web.component.Form
import oxygen.ui.web.create.{Component, NodeModifier}
import oxygen.ui.web.internal.*
import scala.annotation.tailrec
import scala.reflect.TypeTest
import scala.scalajs.js
import zio.{RuntimeFlags as _, *}

sealed trait PWidget[-Env, +Action, -StateGet, +StateSet <: StateGet] {

  private[web] def build[Env2 <: Env: HasNoScope](
      state: PWidgetState[StateGet, StateSet],
      rh: RaiseHandler[Env2, Action],
      pageInstance: PageInstance.Untyped,
      uiRuntime: UIRuntime[Env2],
  ): Growable[DOMElement]

}
object PWidget {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// State ///////////////////////////////////////////////////////////////

  extension [Env, Action](self: PWidget.Stateless[Env, Action]) {

    def fixState[State]: PWidget.Stateful[Env, Action, State] = self

  }

  extension [Env, Action, State](self: PWidget.Stateful[Env, Action, State]) {

    inline def zoomOut[OuterState](inline f: OuterState => State): PWidget.Stateful[Env, Action, OuterState] =
      ZoomOut(self, LensUtil.genLens(f))

    def zoomOutLens[OuterState](lens: Lens[OuterState, State]): PWidget.Stateful[Env, Action, OuterState] =
      ZoomOut(self, lens)

    def attach(v: WidgetState[State]): PWidget.Stateless[Env, Action] = PWidget.AttachWidgetState(self, v)
    def attach(v: GlobalState[State]): PWidget.Stateless[Env, Action] = PWidget.AttachGlobalState(self, v)
    def attach(v: PageLocalState[State]): PWidget.Stateless[Env, Action] = PWidget.AttachPageLocalState(self, v)

  }

  /////// Action ///////////////////////////////////////////////////////////////

  extension [Env, Action, StateGet, StateSet <: StateGet](self: PWidget[Env, Action, StateGet, StateSet]) {

    def discardAction(using HasNoScope[Env]): PWidget[Env, Nothing, StateGet, StateSet] =
      PWidget.StatelessHandleAction[Env, Action, StateGet, StateSet](self, _ => ZIO.unit)

    def mapAction[Action2](f: Action => Action2)(using HasNoScope[Env]): PWidget[Env, Action2, StateGet, StateSet] =
      PWidget.StatelessHandleActionA[Env, Action, Action2, StateGet, StateSet](self, (rh, a) => rh.raiseAction(f(a)))

    def mapActionZIO[Env2 <: Env: HasNoScope, Action2](f: Action => ZIO[Env2 & Scope, UIError, Action2]): PWidget[Env2, Action2, StateGet, StateSet] =
      PWidget.StatelessHandleActionA[Env2, Action, Action2, StateGet, StateSet](self, (rh, a) => f(a).flatMap(rh.raiseAction))

  }

  extension [Env, Action](self: PWidget.Stateless[Env, Action]) {
    def handleActionStateless: HandleActionBuilders.Stateless[Env, Action] = new HandleActionBuilders.Stateless(self)
  }

  extension [Env, Action, State](self: PWidget.Stateful[Env, Action, State]) {
    def handleActionStateful: HandleActionBuilders.Stateful[Env, Action, State] = new HandleActionBuilders.Stateful(self)
  }

  /////// Form ///////////////////////////////////////////////////////////////

  extension [Env, Action, StateGet, StateSet <: StateGet](self: PWidget[Env, Action, StateGet, StateSet]) {

    def asConstForm[Value](value: Value): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
      Form.const(self, value)

    def asUnitForm: Form.Polymorphic[Env, Action, StateGet, StateSet, Unit] =
      Form.const(self, ())

  }

  extension [Env, Action, State](self: PWidget.Stateful[Env, Action, State]) {

    def asStateForm(fields: List[String]): Form.Stateful[Env, Action, State, State] =
      Form.makeWith(fields, self)(identity)

    def asStateForm(field: String): Form.Stateful[Env, Action, State, State] =
      Form.makeWith(field, self)(identity)

    def asStateForm: Form.Stateful[Env, Action, State, State] =
      Form.makeWith(self)(identity)

    def asFormWith[Value](fields: List[String])(f: State => Value): Form.Stateful[Env, Action, State, Value] =
      Form.makeWith(fields, self)(f)

    def asFormWith[Value](field: String)(f: State => Value): Form.Stateful[Env, Action, State, Value] =
      Form.makeWith(field, self)(f)

    def asFormWith[Value](f: State => Value): Form.Stateful[Env, Action, State, Value] =
      Form.makeWith(self)(f)

    def asFormWithValidation[Value](fields: List[String])(f: State => PForm.EitherMessage[Value]): Form.Stateful[Env, Action, State, Value] =
      Form.makeWithValidation(fields, self)(f)

    def asFormWithValidation[Value](field: String)(f: State => PForm.EitherMessage[Value]): Form.Stateful[Env, Action, State, Value] =
      Form.makeWithValidation(field, self)(f)

    def asFormWithValidation[Value](f: State => PForm.EitherMessage[Value]): Form.Stateful[Env, Action, State, Value] =
      Form.makeWithValidation(self)(f)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builder Classes
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object HandleActionBuilders {

    // =====| Stateless |=====

    final class Stateless[Env, Action](widget: PWidget.Stateless[Env, Action]) {

      def apply[Env2 <: Env: HasNoScope](f: Action => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateless[Env2, Nothing] =
        PWidget.StatelessHandleAction(widget, f)

      def a[Action2]: StatelessA[Env, Action, Action2] = new StatelessA(widget)
      def s[State]: StatefulS[Env, Action, State] = new StatefulS(widget)
      def as[Action2, State]: StatefulAS[Env, Action, State, Action2] = new StatefulAS(widget)

      def map[Action2](f: Action => Action2)(using HasNoScope[Env]): PWidget.Stateless[Env, Action2] =
        this.a[Action2] { (rh, a) => rh.raiseAction(f(a)) }
      def mapZIO[Env2 <: Env: HasNoScope, Action2](f: Action => ZIO[Env2 & Scope, UIError, Action2]): PWidget.Stateless[Env2, Action2] =
        this.a[Action2] { (rh, a) => f(a).flatMap(rh.raiseAction) }

    }

    final class StatelessA[Env, Action, Action2](widget: PWidget.Stateless[Env, Action]) {
      def apply[Env2 <: Env: HasNoScope](f: (RaiseHandler[Any, Action2], Action) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateless[Env2, Action2] =
        PWidget.StatelessHandleActionA(widget, f)
    }

    // =====| Stateful |=====

    final class Stateful[Env, Action, State](widget: PWidget.Stateful[Env, Action, State]) {

      def apply[Env2 <: Env: HasNoScope](f: Action => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, Nothing, State] =
        PWidget.StatefulHandleAction(widget, (_, a) => f(a))

      def a[Action2]: StatefulA[Env, Action, State, Action2] = new StatefulA(widget)
      def s: StatefulS[Env, Action, State] = new StatefulS(widget)
      def as[Action2]: StatefulAS[Env, Action, State, Action2] = new StatefulAS(widget)

      // handle part of the action, and allow the rest to pass through
      def ps[HandleA](using
          ev: UnionRemoving[Action, HandleA],
      ): StatefulPS[Env, Action, State, HandleA, ev.Remaining] = new StatefulPS(widget, ev.apply)

      def map[Action2](f: Action => Action2)(using HasNoScope[Env]): PWidget.Stateful[Env, Action2, State] =
        this.a[Action2] { (rh, a) => rh.raiseAction(f(a)) }
      def mapZIO[Env2 <: Env: HasNoScope, Action2](f: Action => ZIO[Env2 & Scope, UIError, Action2]): PWidget.Stateful[Env2, Action2, State] =
        this.a[Action2] { (rh, a) => f(a).flatMap(rh.raiseAction) }

    }

    final class StatefulA[Env, Action, State, Action2](widget: PWidget.Stateful[Env, Action, State]) {
      def apply[Env2 <: Env: HasNoScope](f: (RaiseHandler[Any, Action2], Action) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, Action2, State] =
        PWidget.StatefulHandleActionA(widget, (_, rh, a) => f(rh, a))
    }

    final class StatefulS[Env, Action, State](widget: PWidget.Stateful[Env, Action, State]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], Action) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, Nothing, State] =
        PWidget.StatefulHandleAction(widget, f)
    }

    final class StatefulAS[Env, Action, State, Action2](widget: PWidget.Stateful[Env, Action, State]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, Action2], Action) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, Action2, State] =
        PWidget.StatefulHandleActionA(widget, f)
    }

    final class StatefulPS[Env, Action, State, HandledA, UnhandledA](widget: PWidget.Stateful[Env, Action, State], toEither: Action => Either[HandledA, UnhandledA]) {

      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], HandledA) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, UnhandledA, State] =
        StatefulAS[Env, Action, State, UnhandledA](widget).apply[Env2] { (s, rh, a) =>
          toEither(a) match
            case Left(handle)     => f(s, handle)
            case Right(unhandled) => rh.raiseAction(unhandled)
        }

      def a[UnhandledA2 >: UnhandledA]: StatefulPAS[Env, Action, State, HandledA, UnhandledA2] = new StatefulPAS(widget, toEither)

    }

    final class StatefulPAS[Env, Action, State, HandledA, UnhandledA](widget: PWidget.Stateful[Env, Action, State], toEither: Action => Either[HandledA, UnhandledA]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, UnhandledA], HandledA) => ZIO[Env2 & Scope, UIError, Unit]): PWidget.Stateful[Env2, UnhandledA, State] =
        StatefulAS[Env, Action, State, UnhandledA](widget).apply[Env2] { (s, rh, a) =>
          toEither(a) match
            case Left(handle)     => f(s, rh, handle)
            case Right(unhandled) => rh.raiseAction(unhandled)
        }
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  type Const = Stateless[Any, Nothing]
  type Stateless[-Env, +Action] = PWidget[Env, Action, Any, Nothing]
  type StateOnly[State] = Stateful[Any, Nothing, State]
  type Stateful[-Env, +Action, State] = PWidget[Env, Action, State, State]

  private given unsafe: Unsafe = Unsafe.unsafe(identity)

  /////// State Related ///////////////////////////////////////////////////////////////

  final case class FixState[Env, Action, State](
      withState: WidgetState[State] => PWidget.Stateful[Env, Action, State],
  ) extends PWidget.Stateful[Env, Action, State] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[State],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      withState(state.fix).build[Env2](state, rh, pageInstance, uiRuntime)

  }

  final case class AttachPageLocalState[-Env, +Action, State](
      underlying: PWidget.Stateful[Env, Action, State],
      pageLocalState: PageLocalState[State],
  ) extends PWidget.Stateless[Env, Action] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Anything,
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val createdState: WidgetState[State] = WidgetState.GlobalValue.fromPageLocalState(pageLocalState, pageInstance)
      underlying.build[Env2](createdState, rh, pageInstance, uiRuntime)
    }

  }

  final case class AttachWidgetState[-Env, +Action, State](
      underlying: PWidget.Stateful[Env, Action, State],
      widgetState: WidgetState[State],
  ) extends PWidget.Stateless[Env, Action] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Anything,
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying.build[Env2](widgetState, rh, pageInstance, uiRuntime)

  }

  final case class AttachGlobalState[-Env, +Action, State](
      underlying: PWidget.Stateful[Env, Action, State],
      globalState: GlobalState[State],
  ) extends PWidget.Stateless[Env, Action] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Anything,
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val createdState: WidgetState[State] = WidgetState.GlobalValue.fromGlobalState(globalState, pageInstance)
      underlying.build[Env2](createdState, rh, pageInstance, uiRuntime)
    }

  }

  final case class DetachPageLocalState[Env, Action, DetachedState, InnerStateGet, InnerStateSet <: InnerStateGet](
      withState: WidgetState[DetachedState] => PWidget[Env, Action, InnerStateGet, InnerStateSet],
      pageLocalState: PageLocalState[DetachedState],
  ) extends PWidget[Env, Action, InnerStateGet, InnerStateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[InnerStateGet, InnerStateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val createdState: WidgetState[DetachedState] = WidgetState.GlobalValue.fromPageLocalState(pageLocalState, pageInstance)
      withState(createdState).build[Env2](state, rh, pageInstance, uiRuntime)
    }

  }

  final case class DetachGlobalState[Env, Action, DetachedState, InnerStateGet, InnerStateSet <: InnerStateGet](
      withState: WidgetState[DetachedState] => PWidget[Env, Action, InnerStateGet, InnerStateSet],
      globalState: GlobalState[DetachedState],
  ) extends PWidget[Env, Action, InnerStateGet, InnerStateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[InnerStateGet, InnerStateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val createdState: WidgetState[DetachedState] = WidgetState.GlobalValue.fromGlobalState(globalState, pageInstance)
      withState(createdState).build[Env2](state, rh, pageInstance, uiRuntime)
    }

  }

  final case class SumCases[Env, Action, State](
      cases: List[SumCases.Case[Env, Action, State, ? <: State]],
  ) extends PWidget.Stateful[Env, Action, State] {

    @tailrec
    private def loop[Env2 <: Env: HasNoScope](
        cases: List[SumCases.Case[Env, Action, State, ? <: State]],
        state: WidgetState[State],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      cases match {
        case cHead :: _ if cHead.pf.isDefinedAt(state.renderTimeValue) =>
          val innerState: WidgetState[cHead.State] = WidgetState.SumCase(state, cHead.pf)
          cHead.widget.build[Env2](innerState, rh, pageInstance, uiRuntime)
        case _ :: cTail =>
          loop(cTail, state, rh, pageInstance, uiRuntime)
        case Nil =>
          throw new RuntimeException(s"No SumTypeSplit.Case can handle value: ${state.renderTimeValue}")
      }

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[State],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      loop[Env2](cases, state.fix, rh, pageInstance, uiRuntime)

  }
  object SumCases {

    def fromCases[Env, Action, State](
        cases: Case[Env, Action, State, ? <: State]*,
    ): PWidget.Stateful[Env, Action, State] =
      SumCases(cases.toList)

    def build[State]: ParentBuilder[State] = new ParentBuilder

    final case class Case[-Env, +Action, OuterState, InnerState <: OuterState](
        widget: PWidget.Stateful[Env, Action, InnerState],
        pf: PartialFunction[OuterState, InnerState],
    ) {
      type State = InnerState
    }
    object Case {

      def make[OuterState, InnerState <: OuterState](using tt: TypeTest[OuterState, InnerState]): CaseBuilder[OuterState, InnerState] =
        CaseBuilder { case tt(inner) => inner }

    }

    final class ParentBuilder[OuterState] {

      private val parentCaseBuilder: ParentCaseBuilder[OuterState] = new ParentCaseBuilder

      def apply[Env, Action](
          cases: (ParentCaseBuilder[OuterState] => Case[Env, Action, OuterState, ? <: OuterState])*,
      ): PWidget.Stateful[Env, Action, OuterState] =
        SumCases(cases.map(_(parentCaseBuilder)).toList)

    }

    final class ParentCaseBuilder[OuterState] {

      def subType[InnerState <: OuterState](using tt: TypeTest[OuterState, InnerState]): CaseBuilder[OuterState, InnerState] =
        Case.make[OuterState, InnerState]

    }

    final class CaseBuilder[OuterState, InnerState <: OuterState](pf: PartialFunction[OuterState, InnerState]) {

      inline def zoomIn[State2](inline f: InnerState => State2): ZoomedCaseBuilder[OuterState, InnerState, State2] = {
        val newLens: Lens[InnerState, State2] = LensUtil.genLens(f)
        ZoomedCaseBuilder(pf, newLens)
      }

      def apply[Env, Action](widget: PWidget.Stateful[Env, Action, InnerState]): Case[Env, Action, OuterState, InnerState] =
        Case(widget, pf)

    }

    final class ZoomedCaseBuilder[OuterState, InnerState <: OuterState, State](pf: PartialFunction[OuterState, InnerState], lens: Lens[InnerState, State]) {

      inline def zoomIn[State2](inline f: State => State2): ZoomedCaseBuilder[OuterState, InnerState, State2] = {
        val newLens: Lens[State, State2] = LensUtil.genLens(f)
        ZoomedCaseBuilder(pf, lens.andThen(newLens))
      }

      def apply[Env, Action](widget: PWidget.Stateful[Env, Action, State]): Case[Env, Action, OuterState, InnerState] =
        Case(PWidget.ZoomOut(widget, lens), pf)

    }

  }

  final case class Sequence[Env, Action, F[_], State](
      elemWidget: Int => PWidget.Stateful[Env, Action, State],
      ops: Sequence.Ops[F],
  ) extends PWidget.Stateful[Env, Action, F[State]] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[F[State]],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      ops.childStates[State](state.fix).flatMap { case (idx, idxState) => elemWidget(idx).build[Env2](idxState, rh, pageInstance, uiRuntime) }

  }
  object Sequence {

    trait Ops[F[_]] {
      def childStates[A](state: WidgetState[F[A]]): Growable[(Int, WidgetState[A])]
    }
    object Ops {

      trait FromSizeAndLens[F[_]] extends Ops[F] {

        protected def size[A](fValue: F[A]): Int
        protected def get[A](fValue: F[A], idx: Int): A
        protected def update[A](fValue: F[A], idx: Int, value: A): F[A]

        private def lens[A](idx: Int): Lens[F[A], A] =
          Lens[F[A], A] { fValue => get(fValue, idx) } { value => fValue => update(fValue, idx, value) }

        override final def childStates[A](state: WidgetState[F[A]]): Growable[(Int, WidgetState[A])] =
          Growable.fillWithIndex(size(state.renderTimeValue)) { idx =>
            (idx, WidgetState.ZoomIn(state, lens[A](idx)))
          }

      }

      given arraySeq: Ops[ArraySeq] =
        new FromSizeAndLens[ArraySeq] {
          override protected def size[A](value: ArraySeq[A]): Int = value.length
          override protected def get[A](fValue: ArraySeq[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: ArraySeq[A], idx: Int, value: A): ArraySeq[A] = fValue.updated(idx, value)
        }

      given seq: Ops[Seq] =
        new FromSizeAndLens[Seq] {
          override protected def size[A](value: Seq[A]): Int = value.length
          override protected def get[A](fValue: Seq[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: Seq[A], idx: Int, value: A): Seq[A] = fValue.updated(idx, value)
        }

      given list: Ops[List] =
        new FromSizeAndLens[List] {
          override protected def size[A](value: List[A]): Int = value.length
          override protected def get[A](fValue: List[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: List[A], idx: Int, value: A): List[A] = fValue.updated(idx, value)
        }

      given vector: Ops[Vector] =
        new FromSizeAndLens[Vector] {
          override protected def size[A](value: Vector[A]): Int = value.length
          override protected def get[A](fValue: Vector[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: Vector[A], idx: Int, value: A): Vector[A] = fValue.updated(idx, value)
        }

      given indexedSeq: Ops[IndexedSeq] =
        new FromSizeAndLens[IndexedSeq] {
          override protected def size[A](value: IndexedSeq[A]): Int = value.length
          override protected def get[A](fValue: IndexedSeq[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: IndexedSeq[A], idx: Int, value: A): IndexedSeq[A] = fValue.updated(idx, value)
        }

      given chunk: Ops[Chunk] =
        new FromSizeAndLens[Chunk] {
          override protected def size[A](value: Chunk[A]): Int = value.length
          override protected def get[A](fValue: Chunk[A], idx: Int): A = fValue(idx)
          override protected def update[A](fValue: Chunk[A], idx: Int, value: A): Chunk[A] = fValue.updated(idx, value)
        }

    }

  }

  /////// Action Related ///////////////////////////////////////////////////////////////

  final case class StatelessHandleAction[Env: HasNoScope, Action, StateGet, StateSet <: StateGet](
      underlying: PWidget[Env, Action, StateGet, StateSet],
      f: Action => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget[Env, Nothing, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Nothing],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val myRH: RaiseHandler[Env, Action] = RaiseHandler.FromFunction(f)
      underlying.build[Env](state, myRH, pageInstance, uiRuntime)
    }

  }

  final case class StatelessHandleActionA[Env: HasNoScope, Action, Action2, StateGet, StateSet <: StateGet](
      underlying: PWidget[Env, Action, StateGet, StateSet],
      f: (RaiseHandler[Any, Action2], Action) => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget[Env, Action2, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action2],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val myRH: RaiseHandler[Env2, Action] = RaiseHandler.HandleAction(rh.eraseEnv, f)
      underlying.build[Env2](state, myRH, pageInstance, uiRuntime)
    }

  }

  final case class StatefulHandleAction[Env: HasNoScope, Action, State](
      underlying: PWidget.Stateful[Env, Action, State],
      f: (WidgetState[State], Action) => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget.Stateful[Env, Nothing, State] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[State],
        rh: RaiseHandler[Env2, Nothing],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val f2: Action => ZIO[Env & Scope, UIError, Unit] = f(state.fix, _)
      val myRH: RaiseHandler[Env, Action] = RaiseHandler.FromFunction(f2)
      underlying.build[Env](state, myRH, pageInstance, uiRuntime)
    }

  }

  final case class StatefulHandleActionA[Env: HasNoScope, Action, State, Action2](
      underlying: PWidget.Stateful[Env, Action, State],
      f: (WidgetState[State], RaiseHandler[Any, Action2], Action) => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget.Stateful[Env, Action2, State] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[State],
        rh: RaiseHandler[Env2, Action2],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val f2: (RaiseHandler[Any, Action2], Action) => ZIO[Env & Scope, UIError, Unit] = f(state.fix, _, _)
      val myRH: RaiseHandler[Env, Action] = RaiseHandler.HandleAction(rh.eraseEnv, f2)
      underlying.build[Env](state, myRH, pageInstance, uiRuntime)
    }

  }

  final case class ZoomOut[Env, Action, InnerState, OuterState](
      underlying: PWidget.Stateful[Env, Action, InnerState],
      lens: Lens[OuterState, InnerState],
  ) extends PWidget.Stateful[Env, Action, OuterState] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[OuterState],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] = {
      val newState: WidgetState[InnerState] = WidgetState.ZoomIn(state.fix, lens)
      underlying.build(newState, rh, pageInstance, uiRuntime)
    }

  }

  /////// Root Elements ///////////////////////////////////////////////////////////////

  final case class Node[-Env, +Action, -StateGet, +StateSet <: StateGet](
      xmlns: Option[String],
      tag: String,
      children: Growable[PWidget[Env, Action, StateGet, StateSet]],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.Node(xmlns, tag, children.flatMap(_.build(state, rh, pageInstance, uiRuntime)).toArraySeq))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
    ): Node[Env2, Action2, StateGet2, StateSet2] =
      if (addChildren.isEmpty) this
      else Node(xmlns, tag, children ++ Growable.many(addChildren))

    def apply(mod: NodeModifier): PWidget.Node[Env, Action, StateGet, StateSet] =
      PWidget.Node(
        xmlns,
        tag,
        Growable.single(mod.before) ++ children ++ Growable.single(mod.after),
      )

  }
  object Node {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Node[Env, Action, Any, Nothing]
    type Stateful[-Env, +Action, State] = Node[Env, Action, State, State]

    def empty(xmlns: String, tag: String): Node.Const = Node(xmlns.some, tag, Growable.empty)
    def empty(tag: String): Node.Const = Node(None, tag, Growable.empty)

  }

  final case class Canvas[-Env, +Action, -StateGet, +StateSet <: StateGet](
      draw: CanvasRenderingContext2D => Unit,
      children: Growable[PWidget[Env, Action, StateGet, StateSet]],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.Canvas(draw, children.flatMap(_.build(state, rh, pageInstance, uiRuntime)).toArraySeq))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
    ): Canvas[Env2, Action2, StateGet2, StateSet2] =
      if (addChildren.isEmpty) this
      else Canvas(draw, children ++ Growable.many(addChildren))

  }
  object Canvas {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Canvas[Env, Action, Any, Nothing]
    type Stateful[-Env, +Action, State] = Canvas[Env, Action, State, State]

  }

  final case class Fragment[-Env, +Action, -StateGet, +StateSet <: StateGet](
      elements: Growable[PWidget[Env, Action, StateGet, StateSet]],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      elements.flatMap(_.build(state, rh, pageInstance, uiRuntime))

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addElements: PWidget[Env2, Action2, StateGet2, StateSet2]*,
    ): Fragment[Env2, Action2, StateGet2, StateSet2] =
      if (addElements.isEmpty) this
      else Fragment(elements ++ Growable.many(addElements))

    def apply(mod: NodeModifier): PWidget.Fragment[Env, Action, StateGet, StateSet] =
      PWidget.Fragment(
        Growable.single(mod.before) ++ elements ++ Growable.single(mod.after),
      )

  }
  object Fragment {

    type Const = Stateless[Any, Nothing]
    type Stateless[-Env, +Action] = Fragment[Env, Action, Any, Nothing]
    type Stateful[-Env, +Action, State] = Fragment[Env, Action, State, State]

    val empty: Fragment.Const = Fragment(Growable.empty)

  }

  final case class Raw[E <: DOMElement](raw: E) extends PWidget.Const {

    private val rawGrowable: Growable[E] = Growable.single(raw)

    override private[web] def build[Env2 <: Any: HasNoScope](
        state: PWidgetState.Anything,
        rh: RaiseHandler[Env2, Nothing],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      rawGrowable

  }
  object Raw {

    type Text = PWidget.Raw[DOMElement.Text]
    type CSSAttr = PWidget.Raw[DOMElement.CSSAttr]
    type HtmlAttr = PWidget.Raw[DOMElement.HtmlAttr]
    type ObjectAttr = PWidget.Raw[DOMElement.ObjectAttr]
    type ClassAttr = PWidget.Raw[DOMElement.ClassAttr]

    def text(value: String): PWidget.Raw.Text = Raw(DOMElement.Text(value))

    def css(key: String, value: => String): PWidget.Raw.CSSAttr = Raw(DOMElement.CSSAttr(key, Lazy { value }))
    def attr(key: String, value: String): PWidget.Raw.HtmlAttr = Raw(DOMElement.HtmlAttr(key, value))
    def objectAttr(key: String, value: js.Any): PWidget.Raw.ObjectAttr = Raw(DOMElement.ObjectAttr(key, value))
    def `class`(classes: String*): PWidget.Raw.ClassAttr = Raw(DOMElement.ClassAttr(classes.toSet))
    def `class`(classes: Set[String]): PWidget.Raw.ClassAttr = Raw(DOMElement.ClassAttr(classes))

  }

  /////// Component ///////////////////////////////////////////////////////////////

  final case class ComponentWithProps[-Env, +Action, -StateGet, +StateSet <: StateGet, P](
      component: Component.WithProps[Env, Action, StateGet, StateSet] { type Props = P },
      props: P,
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      component.componentInternal(props, state.renderTimeValue).build(state, rh, pageInstance, uiRuntime)

  }

  final case class ComponentWithoutProps[-Env, +Action, -StateGet, +StateSet <: StateGet](
      component: Component.WithoutProps[Env, Action, StateGet, StateSet],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      component.componentInternal(state.renderTimeValue).build(state, rh, pageInstance, uiRuntime)

  }

  /////// Misc ///////////////////////////////////////////////////////////////

  case object Empty extends PWidget.Const {

    override private[web] def build[Env2 <: Any: HasNoScope](
        state: PWidgetState[Any, Nothing],
        rh: RaiseHandler[Env2, Nothing],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.empty

  }

  final case class WithPageInstance[Env, Action, StateGet, StateSet <: StateGet](
      underlying: PageInstance.Untyped => PWidget[Env, Action, StateGet, StateSet],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying(pageInstance).build(state, rh, pageInstance, uiRuntime)

  }

  final case class Optional[Env, Action, StateGet, StateSet <: StateGet](
      underlying: Option[PWidget[Env, Action, StateGet, StateSet]],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      underlying match
        case Some(underlying) => underlying.build[Env2](state, rh, pageInstance, uiRuntime)
        case None             => Growable.empty

  }

  final case class Foreach[S[_]: SeqRead, I, Env, Action, StateGet, StateSet <: StateGet](
      input: S[I],
      f: I => PWidget[Env, Action, StateGet, StateSet],
  ) extends PWidget[Env, Action, StateGet, StateSet] {

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState[StateGet, StateSet],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.many(input).flatMap(f(_).build[Env2](state, rh, pageInstance, uiRuntime))

  }

  final case class StatelessEventHandler[Env, Action, Event](
      key: String,
      handler: (RaiseHandler[Any, Action], Event) => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget.Stateless[Env, Action] {

    private def handleEvent[Env2 <: Env: HasNoScope](
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    )(e: Event): Unit =
      uiRuntime.unsafeExecute { pageInstance.runForkedHandleErrors[Env2](RootErrorHandler.ErrorLocation.PageEvent(_)) { handler(rh.eraseEnv, e) } }

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Anything,
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.ObjectAttr(key, (e: Event) => handleEvent[Env2](rh, pageInstance, uiRuntime)(e)))

  }

  final case class StatefulEventHandler[Env, Action, State, Event](
      key: String,
      handler: (WidgetState[State], RaiseHandler[Any, Action], Event) => ZIO[Env & Scope, UIError, Unit],
  ) extends PWidget.Stateful[Env, Action, State] {

    private def handleEvent[Env2 <: Env: HasNoScope](
        state: WidgetState[State],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    )(e: Event): Unit =
      uiRuntime.unsafeExecute { pageInstance.runForkedHandleErrors[Env2](RootErrorHandler.ErrorLocation.PageEvent(_)) { handler(state, rh.eraseEnv, e) } }

    override private[web] def build[Env2 <: Env: HasNoScope](
        state: PWidgetState.Fixed[State],
        rh: RaiseHandler[Env2, Action],
        pageInstance: PageInstance.Untyped,
        uiRuntime: UIRuntime[Env2],
    ): Growable[DOMElement] =
      Growable.single(DOMElement.ObjectAttr(key, (e: Event) => handleEvent[Env2](state.fix, rh, pageInstance, uiRuntime)(e)))

  }

}
