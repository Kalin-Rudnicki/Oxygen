package oxygen.ui.web.component

import oxygen.meta.typing.UnionRemoving
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.*
import scala.annotation.targetName
import zio.*

// TODO (KR) : be able to attach a "FormLock", which disables submission while an existing effect is already running
final case class FormWidget[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value](
    widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    fields: List[String],
    stateToValue: StateGet => FormWidget.Result[Value],
) {

  def valueEffect(s: StateGet): IO[UIError.ClientSide.FormValidationErrors, Value] =
    ZIO.fromEither(stateToValue(s).toEither)

  def mapValue[Value2](f: Value => Value2): FormWidget[Env, Action, StateGet, StateSet, Value2] =
    FormWidget(widget, fields, stateToValue(_).map(f))
  def mapValueEitherMessage[Value2](f: Value => FormWidget.EitherMessage[Value2]): FormWidget[Env, Action, StateGet, StateSet, Value2] =
    FormWidget(widget, fields, stateToValue(_).flatMapEitherMessage(fields)(f))
  def mapValueEitherMessages[Value2](f: Value => FormWidget.EitherMessages[Value2]): FormWidget[Env, Action, StateGet, StateSet, Value2] =
    FormWidget(widget, fields, stateToValue(_).flatMapEitherMessages(fields)(f))
  def mapValueEitherError[Value2](f: Value => FormWidget.EitherError[Value2]): FormWidget[Env, Action, StateGet, StateSet, Value2] =
    FormWidget(widget, fields, stateToValue(_).flatMapEitherError(fields)(f))

  def map[Value2](f: Value => Value2): FormWidget[Env, Action, StateGet, StateSet, Value2] = this.mapValue(f)
  def mapOrFail[Value2](f: Value => FormWidget.EitherMessage[Value2]): FormWidget[Env, Action, StateGet, StateSet, Value2] = this.mapValueEitherMessage(f)

  def as[Value2](f: => Value2): FormWidget[Env, Action, StateGet, StateSet, Value2] = this.map(_ => f)
  def unit: FormWidget[Env, Action, StateGet, StateSet, Unit] = this.map(_ => ())

  def result: FormWidget[Env, Action, StateGet, StateSet, FormWidget.Result[Value]] =
    FormWidget(widget, fields, s => FormWidget.Result.Success(stateToValue(s)))

  @targetName("validateValue_single")
  def validateValue(f: Value => FormWidget.EitherMessage[Unit]): FormWidget[Env, Action, StateGet, StateSet, Value] =
    FormWidget(
      widget,
      fields,
      state => stateToValue(state).flatMap { value => FormWidget.Result.fromEitherMessage(fields, f(value)).map(_ => value) },
    )

  @targetName("validateValue_many")
  def validateValue(f: Value => FormWidget.EitherMessages[Unit]): FormWidget[Env, Action, StateGet, StateSet, Value] =
    FormWidget(
      widget,
      fields,
      state => stateToValue(state).flatMap { value => FormWidget.Result.fromEitherMessages(fields, f(value)).map(_ => value) },
    )

  def inNode[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      node: Node.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): FormWidget.Polymorphic[Env2, Action2, StateGet2, StateSet2, Value] =
    node.wrapForm(this)

}
object FormWidget {

  type EitherResult[+Value] = Either[UIError.ClientSide.FormValidationErrors, Value]
  type EitherMessage[+Value] = Either[String, Value]
  type EitherMessages[+Value] = EitherNel[String, Value]
  type EitherError[+Value] = Either[UIError.ClientSide.FormValidationErrors.ErrorType, Value]

  sealed trait Result[+Value] {

    final def map[Value2](f: Value => Value2): Result[Value2] = this match
      case Result.Success(value) => Result.Success(f(value))
      case error: Result.Error   => error

    final def flatMap[Value2](f: Value => Result[Value2]): Result[Value2] = this match
      case Result.Success(value) => f(value)
      case error: Result.Error   => error

    final def flatMapEitherResult[Value2](f: Value => EitherResult[Value2]): Result[Value2] = this match
      case Result.Success(value) => Result.fromEitherResult(f(value))
      case error: Result.Error   => error

    final def flatMapEitherMessage[Value2](fields: List[String])(f: Value => EitherMessage[Value2]): Result[Value2] = this match
      case Result.Success(value) => Result.fromEitherMessage(fields, f(value))
      case error: Result.Error   => error

    final def flatMapEitherMessages[Value2](fields: List[String])(f: Value => EitherMessages[Value2]): Result[Value2] = this match
      case Result.Success(value) => Result.fromEitherMessages(fields, f(value))
      case error: Result.Error   => error

    final def flatMapEitherError[Value2](fields: List[String])(f: Value => EitherError[Value2]): Result[Value2] = this match
      case Result.Success(value) => Result.fromEitherError(fields, f(value))
      case error: Result.Error   => error

    final def toEither: EitherResult[Value] = this match
      case Result.Success(value) => value.asRight
      case Result.Error(error)   => error.asLeft

    final def zipWith[Value2, Out](that: Result[Value2])(f: (Value, Value2) => Out): Result[Out] =
      (this, that) match
        case (Result.Success(a), Result.Success(b))   => Result.Success(f(a, b))
        case (error: Result.Error, Result.Success(_)) => error
        case (Result.Success(_), error: Result.Error) => error
        case (Result.Error(a), Result.Error(b))       => Result.Error(a ++ b)

  }
  object Result {

    // TODO (KR) : allow form to update the underlying widget with the error and/or raise page message(s)
    // type Error[StateGet, StateSet] = Ior[StateGet => StateSet, NonEmptyList[UIError.ClientSide.FormValidationErrors.Error]]

    final case class Success[+Value](value: Value) extends Result[Value]
    final case class Error(error: UIError.ClientSide.FormValidationErrors) extends Result[Nothing]

    def fromEitherResult[Value](either: EitherResult[Value]): Result[Value] = either match
      case Right(value) => Success(value)
      case Left(error)  => Error(error)

    def fromEitherMessage[Value](fields: List[String], either: EitherMessage[Value]): Result[Value] = either match
      case Right(value) => Success(value)
      case Left(error)  => Error(UIError.form.invalid(fields, error))

    def fromEitherMessages[Value](fields: List[String], either: EitherMessages[Value]): Result[Value] = either match
      case Right(value) => Success(value)
      case Left(error)  => Error(UIError.form.invalid(fields, error))

    def fromEitherError[Value](fields: List[String], either: EitherError[Value]): Result[Value] = either match
      case Right(value) => Success(value)
      case Left(error)  => Error(UIError.form.invalid(fields, error))

  }

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = FormWidget[Env, Action, StateGet, StateSet, Value]
  type Stateless[-Env, +Action, +Value] = FormWidget[Env, Action, Any, Nothing, Value]
  type Stateful[-Env, +Action, State, +Value] = FormWidget[Env, Action, State, State, Value]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Extensions
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [Env, Action, State, Value](self: FormWidget.Stateful[Env, Action, State, Value]) {

    def <*>[Env2 <: Env, Action2 >: Action, Value2](that: FormWidget.Stateful[Env2, Action2, State, Value2])(using zip: Zip[Value, Value2]): FormWidget.Stateful[Env2, Action2, State, zip.Out] =
      FormWidget(
        fragment(self.widget, that.widget),
        self.fields ++ that.fields,
        state => self.stateToValue(state).zipWith(that.stateToValue(state))(zip.zip),
      )

    def <*[Env2 <: Env, Action2 >: Action, Value2](that: FormWidget.Stateful[Env2, Action2, State, Value2]): FormWidget.Stateful[Env2, Action2, State, Value] =
      FormWidget(
        fragment(self.widget, that.widget),
        self.fields ++ that.fields,
        state => self.stateToValue(state).zipWith(that.stateToValue(state))((v, _) => v),
      )

    def *>[Env2 <: Env, Action2 >: Action, Value2](that: FormWidget.Stateful[Env2, Action2, State, Value2]): FormWidget.Stateful[Env2, Action2, State, Value2] =
      FormWidget(
        fragment(self.widget, that.widget),
        self.fields ++ that.fields,
        state => self.stateToValue(state).zipWith(that.stateToValue(state))((_, v) => v),
      )

    inline def zoomOut[OuterState](inline f: OuterState => State): FormWidget.Stateful[Env, Action, OuterState, Value] =
      FormWidget(
        self.widget.zoomOut[OuterState](f),
        self.fields,
        state => self.stateToValue(f(state)),
      )

    def handleActionStateful: HandleActionBuilders.Stateful[Env, Action, State, Value] = HandleActionBuilders.Stateful(self)

    // TODO (KR) : need some way to allow the `Value` of one form to control how other forms are created.
    /*
    def foldValue[Env2 <: Env, Action2 >: Action, Value2](
        withValue: (FormWidget.Stateful[Env, Action, State, Value], Value) => FormWidget.Stateful[Env2, Action2, State, Value2],
    ): FormWidget.Stateful[Env2, Action2, State, Value] =
     */

  }

  extension [Env, State, Value](self: FormWidget.Stateful[Env, Form.Submit, State, Value]) {
    // TODO (KR) : return a more specialized builder that ignores useless `Submit` action?
    def onSubmit: HandleActionBuilders.Stateful[Env, Form.Submit, State, Value] = HandleActionBuilders.Stateful(self)
  }

  extension [Env, Action >: Form.Submit, State, Value](self: FormWidget.Stateful[Env, Action, State, Value]) {
    // TODO (KR) : return a more specialized builder that ignores useless `Submit` action?
    def onSubmitPartial(using ev: UnionRemoving[Action, Form.Submit]): HandleActionBuilders.StatefulPS[Env, Action, State, Value, Form.Submit, ev.Remaining] =
      self.handleActionStateful.ps[Form.Submit]
  }

  ///////  ///////////////////////////////////////////////////////////////

  extension [Env, Action, State, Value](self: FormWidget.Stateful[Env, Action, State, Option[Value]]) {

    def required: FormWidget.Stateful[Env, Action, State, Value] =
      self.mapValueEitherError { _.toRight(UIError.ClientSide.FormValidationErrors.ErrorType.MissingRequired) }

  }

  extension [Env, Action, State, Value](self: FormWidget.Stateful[Env, Action, State, FormWidget.Result[Value]]) {

    def absolve: FormWidget.Stateful[Env, Action, State, Value] =
      FormWidget(self.widget, self.fields, self.stateToValue(_).flatMap(identity))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object HandleActionBuilders {

    final class Stateful[Env, Action, State, Value](widget: FormWidget.Stateful[Env, Action, State, Value]) {

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

    final class StatefulA[Env, Action, State, Value, Action2](widget: FormWidget.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (RaiseHandler[Any, Action2], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Action2, State] =
        new StatefulAS[Env, Action, State, Value, Action2](widget).apply[Env2] { (_, rh, a, v) => f(rh, a, v) }
    }

    final class StatefulS[Env, Action, State, Value](widget: FormWidget.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Nothing, State] =
        new PWidget.HandleActionBuilders.StatefulS[Env, Action, State](widget.widget).apply[Env2] { (s, a) => widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, a, _)) }
    }

    final class StatefulAS[Env, Action, State, Value, Action2](widget: FormWidget.Stateful[Env, Action, State, Value]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, Action2], Action, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, Action2, State] =
        new PWidget.HandleActionBuilders.StatefulAS[Env, Action, State, Action2](widget.widget).apply[Env2] { (s, rh, a) => widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, rh, a, _)) }
    }

    final class StatefulPS[Env, Action, State, Value, HandledA, UnhandledA](widget: FormWidget.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {

      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        new PWidget.HandleActionBuilders.StatefulPS[Env, Action, State, HandledA, UnhandledA](widget.widget, toEither).apply[Env2] { (s, a) =>
          widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, a, _))
        }

      def a[UnhandledA2 >: UnhandledA]: StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA2] = new StatefulPAS(widget, toEither)

    }

    final class StatefulPAS[Env, Action, State, Value, HandledA, UnhandledA](widget: FormWidget.Stateful[Env, Action, State, Value], toEither: Action => Either[HandledA, UnhandledA]) {
      def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, UnhandledA], HandledA, Value) => ZIO[Env2 & Scope, UIError, Unit]): Widget.Stateful[Env2, UnhandledA, State] =
        new PWidget.HandleActionBuilders.StatefulPAS[Env, Action, State, HandledA, UnhandledA](widget.widget, toEither).apply[Env2] { (s, rh, a) =>
          widget.valueEffect(s.unsafeCurrentValue).flatMap(f(s, rh, a, _))
        }
    }

  }

}
