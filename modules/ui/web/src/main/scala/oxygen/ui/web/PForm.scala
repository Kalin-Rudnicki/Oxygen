package oxygen.ui.web

import monocle.Lens
import oxygen.meta.typing.UnionRemoving
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.{Form, Wizard}
import oxygen.ui.web.create.*
import oxygen.ui.web.internal.LensUtil
import scala.annotation.targetName
import zio.*

// TODO (KR) : be able to attach a "FormLock", which disables submission while an existing effect is already running
sealed trait PForm[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] {

  private[PForm] def buildInternal[State2 >: StateSet <: StateGet](state: WidgetState[State2]): (Widget.Stateful[Env, Action, State2], FormValue[State2, Value])

  def mapValue[Value2](f: Value => Value2): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm.TransformValue(this, _.mapValue(f))
  def mapValueEitherMessage[Value2](f: Value => PForm.EitherMessage[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm.TransformValue(this, _.mapValueEitherMessage(f))
  def mapValueEitherMessages[Value2](f: Value => PForm.EitherMessages[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm.TransformValue(this, _.mapValueEitherMessages(f))
  def mapValueEitherError[Value2](f: Value => PForm.EitherError[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm.TransformValue(this, _.mapValueEitherError(f))

  def mapOrFail[Value2](f: Value => PForm.EitherMessage[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] = this.mapValueEitherMessage(f)

  def as[Value2](f: => Value2): PForm[Env, Action, StateGet, StateSet, Value2] = this.mapValue(_ => f)
  def unit: PForm[Env, Action, StateGet, StateSet, Unit] = this.mapValue(_ => ())

  def result: PForm[Env, Action, StateGet, StateSet, PForm.Result[Value]] =
    PForm.TransformValue(this, _.result)

  @targetName("validateValue_single")
  def validateValue(f: Value => PForm.EitherMessage[Unit]): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm.TransformValue(this, _.validateValue(f))

  @targetName("validateValue_many")
  def validateValue(f: Value => PForm.EitherMessages[Unit]): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm.TransformValue(this, _.validateValue(f))

}
object PForm {

  def apply[Env, Action, StateGet, StateSet <: StateGet, Value](
      widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
      fields: List[String],
      valueResult: StateGet => Result[Value],
  ): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm.Basic(widget, FormValue(fields, valueResult))

  def state[S]: StateBuilder[S, S] = StateBuilder(identity)

  type Result[+Value] = FormResult[Value]
  type EitherResult[+Value] = Either[UIError.ClientSide.FormValidationErrors, Value]
  type EitherMessage[+Value] = Either[String, Value]
  type EitherMessages[+Value] = EitherNel[String, Value]
  type EitherError[+Value] = Either[UIError.ClientSide.FormValidationErrors.ErrorType, Value]

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = PForm[Env, Action, StateGet, StateSet, Value]
  type Stateless[-Env, +Action, +Value] = PForm[Env, Action, Any, Nothing, Value]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Impls
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait StatefulImpl[-Env, +Action, State, +Value] extends PForm[Env, Action, State, State, Value] {

    private[PForm] final def buildInternal[State2 >: State <: State](state: WidgetState[State2]): (Widget.Stateful[Env, Action, State2], FormValue[State2, Value]) =
      buildInternalStateful(state.asInstanceOf[WidgetState[State]]).asInstanceOf[(Widget.Stateful[Env, Action, State2], FormValue[State2, Value])]

    private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env, Action, State], FormValue[State, Value])

  }

  type Stateful[-Env, +Action, State, +Value] = PForm[Env, Action, State, State, Value]

  final case class Basic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value](
      widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
      value: FormValue[StateGet, Value],
  ) extends PForm[Env, Action, StateGet, StateSet, Value] {

    override private[PForm] def buildInternal[State2 >: StateSet <: StateGet](state: WidgetState[State2]): (Widget.Stateful[Env, Action, State2], FormValue[State2, Value]) =
      (widget, value)

  }

  final case class TransformValue[Env, Action, StateGet, StateSet <: StateGet, Value1, Value2](
      inner: PForm[Env, Action, StateGet, StateSet, Value1],
      f: FormValue[StateGet, Value1] => FormValue[StateGet, Value2],
  ) extends PForm[Env, Action, StateGet, StateSet, Value2] {

    override private[PForm] def buildInternal[State2 >: StateSet <: StateGet](state: WidgetState[State2]): (Widget.Stateful[Env, Action, State2], FormValue[State2, Value2]) = {
      val (w, fv) = inner.buildInternal[State2](state)
      (w, f.asInstanceOf[FormValue[State2, Value1] => FormValue[State2, Value2]].apply(fv))
    }

  }

  final case class ZoomOut[Env, Action, InnerState, OuterState, Value](
      inner: PForm.Stateful[Env, Action, InnerState, Value],
      lens: Lens[OuterState, InnerState],
  ) extends PForm.StatefulImpl[Env, Action, OuterState, Value] {

    override private[PForm] def buildInternalStateful(state: WidgetState[OuterState]): (Widget.Stateful[Env, Action, OuterState], FormValue[OuterState, Value]) = {
      val (w, fv) = inner.buildInternal[InnerState](state.zoomInLens(lens))
      (w.zoomOutLens(lens), fv.zoomOutLens(lens))
    }

  }

  final case class ZipWith[Env, Action, State, Value1, Value2, Value3](
      a: PForm.Stateful[Env, Action, State, Value1],
      b: PForm.Stateful[Env, Action, State, Value2],
      f: (Value1, Value2) => Value3,
  ) extends PForm.StatefulImpl[Env, Action, State, Value3] {

    override private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env, Action, State], FormValue[State, Value3]) = {
      val (wA, fvA) = a.buildInternal[State](state)
      val (wB, fvB) = b.buildInternal[State](state)
      (fragment(wA, wB), fvA.zipWith(fvB)(f))
    }

  }

  final case class Mapped[Env, Action, Env2 <: Env, Action2 >: Action, State, Value1, Value2](
      inner: PForm.Stateful[Env, Action, State, Value1],
      f: (Widget.Stateful[Env, Action, State], FormValue[State, Value1]) => (Widget.Stateful[Env2, Action2, State], FormValue[State, Value2]),
  ) extends PForm.StatefulImpl[Env2, Action2, State, Value2] {

    override private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env2, Action2, State], FormValue[State, Value2]) = {
      val (w, fv) = inner.buildInternal[State](state)
      f(w, fv)
    }

  }

  final case class FlatMapped[Env, Action, Env2 <: Env, Action2 >: Action, State, Value1, Value2](
      inner: PForm.Stateful[Env, Action, State, Value1],
      f: (Widget.Stateful[Env, Action, State], FormValue[State, Value1]) => PForm.Stateful[Env2, Action2, State, Value2],
  ) extends PForm.StatefulImpl[Env2, Action2, State, Value2] {

    override private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env2, Action2, State], FormValue[State, Value2]) = {
      val (w, fv) = inner.buildInternal[State](state)
      f(w, fv).buildInternal[State](state)
    }

  }

  final case class FlatMapWizard[Env, Action, State, Value1, Out, Value2](
      inner: Wizard[State, Value1, Out],
      f: Out => PForm.Stateful[Env, Action, State, Value2],
  ) extends PForm.StatefulImpl[Env, Action, State, Value2] {

    override private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env, Action, State], FormValue[State, Value2]) =
      f(inner.toOut(inner.formValue.valueResult(state.renderTimeValue))).buildInternal[State](state)

  }

  final case class Sequence[Env, Action, State, Value, F[_]](
      inner: PForm.Stateful[Env, Action, State, Value],
      ops: PWidget.Sequence.Ops[F],
  ) extends PForm.StatefulImpl[Env, Action, F[State], F[Value]] {

    override private[PForm] def buildInternalStateful(state: WidgetState[F[State]]): (Widget.Stateful[Env, Action, F[State]], FormValue[F[State], F[Value]]) = {
      val lenses: ArraySeq[(Int, Lens[F[State], State])] =
        ops.lenses(state).toArraySeq
      val pairs: ArraySeq[(Widget.Stateful[Env, Action, F[State]], FormValue[F[State], Value])] =
        lenses.map { case (_, lens) => inner.zoomOutLens(lens).buildInternal[F[State]](state) }
      val formValues: ArraySeq[FormValue[F[State], Value]] =
        pairs.map(_._2)

      (
        Widget.foreach(pairs)(_._1),
        FormValue[F[State], F[Value]](
          formValues.flatMap(_.fields).toList,
          { s =>
            formValues.parTraverse { _.valueResult(s).toEither.leftMap(_.errors) } match {
              case Right(value) => FormResult.Success(ops.wrap(value))
              case Left(errors) => FormResult.Error(UIError.ClientSide.FormValidationErrors(errors))
            }
          },
        ),
      )
    }

  }

  final case class WithState[Env, Action, State, StateIn, Value](
      toIn: State => StateIn,
      f: StateIn => PForm.Stateful[Env, Action, State, Value],
  ) extends PForm.StatefulImpl[Env, Action, State, Value] {

    override private[PForm] def buildInternalStateful(state: WidgetState[State]): (Widget.Stateful[Env, Action, State], FormValue[State, Value]) =
      f(toIn(state.renderTimeValue)).buildInternal[State](state)

  }

  final class StateBuilder[State, StateIn](toIn: State => StateIn) {

    def zoomIn[StateIn2](f: StateIn => StateIn2): StateBuilder[State, StateIn2] = StateBuilder(s => f(toIn(s)))

    def flatMap[Env, Action, Value](f: StateIn => Form.Stateful[Env, Action, State, Value]): Form.Stateful[Env, Action, State, Value] =
      PForm.WithState(toIn, f)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [Env, Action, State, Value](self: PForm.Stateful[Env, Action, State, Value]) {

    def <*>[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2])(using zip: Zip[Value, Value2]): PForm.Stateful[Env2, Action2, State, zip.Out] =
      PForm.ZipWith(self, that, zip.zip)

    def <*[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2]): PForm.Stateful[Env2, Action2, State, Value] =
      PForm.ZipWith(self, that, (a, _) => a)

    def *>[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2]): PForm.Stateful[Env2, Action2, State, Value2] =
      PForm.ZipWith(self, that, (_, b) => b)

    inline def zoomOut[OuterState](inline f: OuterState => State): PForm.Stateful[Env, Action, OuterState, Value] =
      PForm.ZoomOut(self, LensUtil.genLens(f))

    def zoomOutLens[OuterState](lens: Lens[OuterState, State]): PForm.Stateful[Env, Action, OuterState, Value] =
      PForm.ZoomOut(self, lens)

    def handleActionStateful: HandleActionBuilders.Stateful[Env, Action, State, Value] = HandleActionBuilders.Stateful(self)

    def map[Env2 <: Env, Action2 >: Action, Value2](
        f: (Widget.Stateful[Env, Action, State], FormValue[State, Value]) => (Widget.Stateful[Env2, Action2, State], FormValue[State, Value2]),
    ): PForm.Stateful[Env2, Action2, State, Value2] =
      PForm.Mapped(self, f)

    def flatMap[Env2 <: Env, Action2 >: Action, Value2](
        f: (Widget.Stateful[Env, Action, State], FormValue[State, Value]) => PForm.Stateful[Env2, Action2, State, Value2],
    ): PForm.Stateful[Env2, Action2, State, Value2] =
      PForm.FlatMapped(self, f)

    def widget: Widget.Stateful[Env, Action, State] =
      Widget.state[State].fix { self.buildInternal[State](_)._1 }

    // TODO (KR) : need some way to allow the `Value` of one form to control how other forms are created.
    //           : (tmp).wizard { (tmpWidget, tmpValue, optValue) => ??? } could be an optional, required, or either value, applying the value function to current state
    /*
    def foldValue[Env2 <: Env, Action2 >: Action, Value2](
        withValue: (PForm.Stateful[Env, Action, State, Value], Value) => PForm.Stateful[Env2, Action2, State, Value2],
    ): PForm.Stateful[Env2, Action2, State, Value] =
     */

  }

  extension [Env, State, Value](self: PForm.Stateful[Env, Form.Submit, State, Value]) {
    // TODO (KR) : return a more specialized builder that ignores useless `Submit` action?
    def onSubmit: HandleActionBuilders.Stateful[Env, Form.Submit, State, Value] = HandleActionBuilders.Stateful(self)
  }

  extension [Env, Action >: Form.Submit, State, Value](self: PForm.Stateful[Env, Action, State, Value]) {
    // TODO (KR) : return a more specialized builder that ignores useless `Submit` action?
    def onSubmitPartial(using ev: UnionRemoving[Action, Form.Submit]): HandleActionBuilders.StatefulPS[Env, Action, State, Value, Form.Submit, ev.Remaining] =
      self.handleActionStateful.ps[Form.Submit]
  }

  ///////  ///////////////////////////////////////////////////////////////

  extension [Env, Action, State, Value](self: PForm.Stateful[Env, Action, State, Option[Value]]) {

    def required: PForm.Stateful[Env, Action, State, Value] =
      self.mapValueEitherError { _.toRight(UIError.ClientSide.FormValidationErrors.ErrorType.MissingRequired) }

  }

  extension [Env, Action, State, Value](self: PForm.Stateful[Env, Action, State, PForm.Result[Value]]) {

    def absolve: PForm.Stateful[Env, Action, State, Value] =
      PForm.TransformValue(self, _.absolve)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object HandleActionBuilders {

    final class Stateful[Env, Action, State, Value](widget: PForm.Stateful[Env, Action, State, Value]) {

      def apply[Env2 <: Env: HasNoScope](f: (Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Nothing, State] =
        new StatefulA[Env, Action, State, Value, Nothing](widget).apply[Env2] { (_, a, v) => f(a, v) }

      def a[Action2]: StatefulA[Env, Action, State, Value, Action2] = new StatefulA(widget)
      def s: StatefulS[Env, Action, State, Value] = new StatefulS(widget)
      def as[Action2]: StatefulAS[Env, Action, State, Value, Action2] = new StatefulAS(widget)

      // handle part of the action, and allow the rest to pass through
      def ps[HandleA](using
          ev: UnionRemoving[Action, HandleA],
      ): StatefulPS[Env, Action, State, Value, HandleA, ev.Remaining] =
        new StatefulPS(widget, ev.apply)

      def map[Action2](f: (Action, Value) => Action2)(using HasNoScope[Env]): Widget.Stateful[Env, Action2, State] =
        this.a[Action2] { (rh, a, v) => rh.raiseAction(f(a, v)) }
      def mapZIO[Env2 <: Env: HasNoScope, Action2](f: (Action, Value) => ZIO[Env2 & Scope, UIError, Action2]): Widget.Stateful[Env2, Action2, State] =
        this.a[Action2] { (rh, a, v) => f(a, v).flatMap(rh.raiseAction) }

    }

    final class StatefulA[Env, Action, State, Value, Action2](widget: PForm.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (RaiseHandler[Any, Action2], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Action2, State] =
        new StatefulAS[Env, Action, State, Value, Action2](widget).apply[Env2] { (_, rh, a, v) => f(rh, a, v) }
    }

    final class StatefulS[Env, Action, State, Value](widget: PForm.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Nothing, State] =
        Widget.state[State].fix { widgetState =>
          val (w, fv) = widget.buildInternal[State](widgetState)
          new PWidget.HandleActionBuilders.StatefulS[Env, Action, State](w).apply[Env2] { (s, a) => fv.valueEffect[State](s).flatMap(f(s, a, _)) }
        }
    }

    final class StatefulAS[Env, Action, State, Value, Action2](widget: PForm.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, Action2], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Action2, State] =
        Widget.state[State].fix { widgetState =>
          val (w, fv) = widget.buildInternal[State](widgetState)
          new PWidget.HandleActionBuilders.StatefulAS[Env, Action, State, Action2](w).apply[Env2] { (s, rh, a) => fv.valueEffect[State](s).flatMap(f(s, rh, a, _)) }
        }
    }

    final class StatefulPS[Env, Action, State, Value, HandledA, UnhandledA](widget: PForm.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {

      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        Widget.state[State].fix { widgetState =>
          val (w, fv) = widget.buildInternal[State](widgetState)
          new PWidget.HandleActionBuilders.StatefulPS[Env, Action, State, HandledA, UnhandledA](w, toEither).apply[Env2] { (s, a) =>
            fv.valueEffect[State](s).flatMap(f(s, a, _))
          }
        }

      def a[UnhandledA2 >: UnhandledA]: StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA2] = new StatefulPAS(widget, toEither)

    }

    final class StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA](widget: PForm.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, UnhandledA], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        Widget.state[State].fix { widgetState =>
          val (w, fv) = widget.buildInternal[State](widgetState)
          new PWidget.HandleActionBuilders.StatefulPAS[Env, Action, State, HandledA, UnhandledA](w, toEither).apply[Env2] { (s, rh, a) =>
            fv.valueEffect[State](s).flatMap(f(s, rh, a, _))
          }
        }
    }

  }

}
