package oxygen.ui.web

import oxygen.meta.typing.UnionRemoving
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.Form
import oxygen.ui.web.create.*
import scala.annotation.targetName
import zio.*

// TODO (KR) : be able to attach a "FormLock", which disables submission while an existing effect is already running
final case class PForm[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value](
    widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    value: FormValue[StateGet, Value],
) {

  def valueEffect(s: StateGet): IO[UIError.ClientSide.FormValidationErrors, Value] =
    ZIO.fromEither(value.valueResult(s).toEither)

  def mapValue[Value2](f: Value => Value2): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm(widget, value.mapValue(f))
  def mapValueEitherMessage[Value2](f: Value => PForm.EitherMessage[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm(widget, value.mapValueEitherMessage(f))
  def mapValueEitherMessages[Value2](f: Value => PForm.EitherMessages[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm(widget, value.mapValueEitherMessages(f))
  def mapValueEitherError[Value2](f: Value => PForm.EitherError[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] =
    PForm(widget, value.mapValueEitherError(f))

  def mapOrFail[Value2](f: Value => PForm.EitherMessage[Value2]): PForm[Env, Action, StateGet, StateSet, Value2] = this.mapValueEitherMessage(f)

  def as[Value2](f: => Value2): PForm[Env, Action, StateGet, StateSet, Value2] = this.mapValue(_ => f)
  def unit: PForm[Env, Action, StateGet, StateSet, Unit] = this.mapValue(_ => ())

  def result: PForm[Env, Action, StateGet, StateSet, PForm.Result[Value]] =
    PForm(widget, value.result)

  @targetName("validateValue_single")
  def validateValue(f: Value => PForm.EitherMessage[Unit]): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm(widget, value.validateValue(f))

  @targetName("validateValue_many")
  def validateValue(f: Value => PForm.EitherMessages[Unit]): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm(widget, value.validateValue(f))

  def inNode[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      node: Node.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): PForm.Polymorphic[Env2, Action2, StateGet2, StateSet2, Value] =
    node.wrapForm(this)

}
object PForm {

  def apply[Env, Action, StateGet, StateSet <: StateGet, Value](
      widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
      fields: List[String],
      valueResult: StateGet => Result[Value],
  ): PForm[Env, Action, StateGet, StateSet, Value] =
    PForm(widget, FormValue(fields, valueResult))

  type Result[+Value] = FormResult[Value]
  type EitherResult[+Value] = Either[UIError.ClientSide.FormValidationErrors, Value]
  type EitherMessage[+Value] = Either[String, Value]
  type EitherMessages[+Value] = EitherNel[String, Value]
  type EitherError[+Value] = Either[UIError.ClientSide.FormValidationErrors.ErrorType, Value]

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = PForm[Env, Action, StateGet, StateSet, Value]
  type Stateless[-Env, +Action, +Value] = PForm[Env, Action, Any, Nothing, Value]
  type Stateful[-Env, +Action, State, +Value] = PForm[Env, Action, State, State, Value]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [Env, Action, State, Value](self: PForm.Stateful[Env, Action, State, Value]) {

    def <*>[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2])(using zip: Zip[Value, Value2]): PForm.Stateful[Env2, Action2, State, zip.Out] =
      PForm(
        fragment(self.widget, that.widget),
        self.value <*> that.value,
      )

    def <*[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2]): PForm.Stateful[Env2, Action2, State, Value] =
      PForm(
        fragment(self.widget, that.widget),
        self.value <* that.value,
      )

    def *>[Env2 <: Env, Action2 >: Action, Value2](that: PForm.Stateful[Env2, Action2, State, Value2]): PForm.Stateful[Env2, Action2, State, Value2] =
      PForm(
        fragment(self.widget, that.widget),
        self.value *> that.value,
      )

    inline def zoomOut[OuterState](inline f: OuterState => State): PForm.Stateful[Env, Action, OuterState, Value] =
      PForm(
        self.widget.zoomOut[OuterState](f),
        self.value.fields,
        state => self.value.valueResult(f(state)),
      )

    def handleActionStateful: HandleActionBuilders.Stateful[Env, Action, State, Value] = HandleActionBuilders.Stateful(self)

    def map[Env2 <: Env, Action2 >: Action, Value2](
        f: (Widget.Stateful[Env, Action, State], FormValue[State, Value]) => (Widget.Stateful[Env2, Action2, State], FormValue[State, Value2]),
    ): PForm.Stateful[Env2, Action2, State, Value2] = {
      val (widgetRes, valueRes) = f(self.widget, self.value)
      PForm(widgetRes, valueRes)
    }

    def flatMap[Env2 <: Env, Action2 >: Action, Value2](
        f: (Widget.Stateful[Env, Action, State], FormValue[State, Value]) => PForm.Stateful[Env2, Action2, State, Value2],
    ): PForm.Stateful[Env2, Action2, State, Value2] =
      f(self.widget, self.value)

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
      PForm(self.widget, self.value.absolve)

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
      ): StatefulPS[Env, Action, State, Value, HandleA, ev.Remaining] = new StatefulPS(widget, ev.apply)

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
        new PWidget.HandleActionBuilders.StatefulS[Env, Action, State](widget.widget).apply[Env2] { (s, a) => widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, a, _)) }
    }

    final class StatefulAS[Env, Action, State, Value, Action2](widget: PForm.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, Action2], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Action2, State] =
        new PWidget.HandleActionBuilders.StatefulAS[Env, Action, State, Action2](widget.widget).apply[Env2] { (s, rh, a) => widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, rh, a, _)) }
    }

    final class StatefulPS[Env, Action, State, Value, HandledA, UnhandledA](widget: PForm.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {

      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        new PWidget.HandleActionBuilders.StatefulPS[Env, Action, State, HandledA, UnhandledA](widget.widget, toEither).apply[Env2] { (s, a) =>
          widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, a, _))
        }

      def a[UnhandledA2 >: UnhandledA]: StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA2] = new StatefulPAS(widget, toEither)

    }

    final class StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA](widget: PForm.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, UnhandledA], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        new PWidget.HandleActionBuilders.StatefulPAS[Env, Action, State, HandledA, UnhandledA](widget.widget, toEither).apply[Env2] { (s, rh, a) =>
          widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, rh, a, _))
        }
    }

  }

}
