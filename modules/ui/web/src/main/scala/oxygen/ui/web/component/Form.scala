package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import scala.annotation.targetName
import zio.*

// TODO (KR) : be able to attach a "FormLock", which disables submission while an existing effect is already running
final case class Form[-Env, -StateGet, +StateSet <: StateGet, Value](
    widget: Widget.Polymorphic[Env, Form.Submit, StateGet, StateSet],
    fields: List[String],
    stateToValue: StateGet => Either[UIError.ClientSide.FormValidationErrors, Value],
) {

  def mapValue[Value2](f: Value => Value2): Form[Env, StateGet, StateSet, Value2] =
    Form(widget, fields, stateToValue(_).map(f))

  def flatMapValue[Value2](f: Value => Either[String, Value2]): Form[Env, StateGet, StateSet, Value2] =
    Form(widget, fields, stateToValue(_).flatMap(f(_).leftMap(UIError.form.invalid(fields, _))))

  @targetName("validateValue_single")
  def validateValue(f: Value => Either[String, Unit]): Form[Env, StateGet, StateSet, Value] =
    Form(
      widget,
      fields,
      state =>
        for {
          value <- stateToValue(state)
          _ <- UIError.form.validate(fields)(f(value))
        } yield value,
    )

  @targetName("validateValue_many")
  def validateValue(f: Value => EitherNel[String, Unit]): Form[Env, StateGet, StateSet, Value] =
    Form(
      widget,
      fields,
      state =>
        for {
          value <- stateToValue(state)
          _ <- UIError.form.validate(fields)(f(value))
        } yield value,
    )

}
object Form {

  // TODO (KR) : allow form to update the underlying widget with the error and/or raise page message(s)
  // type Error[StateGet, StateSet] = Ior[StateGet => StateSet, NonEmptyList[UIError.ClientSide.FormValidationErrors.Error]]

  type Stateless[Env, Value] = Form[Env, Any, Nothing, Value]
  type Stateful[Env, State, Value] = Form[Env, State, State, Value]

  type Submit = Submit.type
  object Submit

  extension [Env, State, Value](self: Form.Stateful[Env, State, Value]) {

    def &&[Env2 <: Env, Value2](that: Form.Stateful[Env2, State, Value2])(using zip: Zip[Value, Value2]): Form.Stateful[Env2, State, zip.Out] =
      Form(
        fragment(self.widget, that.widget),
        self.fields ++ that.fields,
        state => UIError.ClientSide.FormValidationErrors.zipWith(self.stateToValue(state), that.stateToValue(state))(zip.zip),
      )

    inline def zoomOut[OuterState](inline f: OuterState => State): Form.Stateful[Env, OuterState, Value] =
      Form(
        self.widget.zoomOut[OuterState](f),
        self.fields,
        state => self.stateToValue(f(state)),
      )

    def onSubmit: OnSubmitBuilder[Env, State, Value] = OnSubmitBuilder(self)

  }

  extension [Env, State, Value](self: Form.Stateful[Env, State, Option[Value]]) {

    def required: Form.Stateful[Env, State, Value] =
      Form(
        self.widget,
        self.fields,
        self.stateToValue(_).flatMap {
          _.toRight { UIError.form.missingRequired(self.fields) }
        },
      )

  }

  def unit[Env, StateGet, StateSet <: StateGet](widget: Widget.Polymorphic[Env, Form.Submit, StateGet, StateSet]): Form[Env, StateGet, StateSet, Unit] =
    Form(widget, Nil, _ => ().asRight)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form Elems
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def elementLabel(
      labelText: String,
      descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
      labelMod: NodeModifier = NodeModifier.empty,
  ): Widget.Const =
    fragment(
      div(label(labelText, padding(S.spacing._2, S.spacing._5), fontWeight._600, fontSize := S.fontSize._4)(labelMod)),
      Widget.foreach(descriptionText.toOption) { descr =>
        div(padding(S.spacing._0, S.spacing._8))(
          descr,
        )
      },
    )

  object textField {

    final case class Props(
        width: String = "fit-content",
        padding: String = 10.px,
        display: String = "block",
        labelMod: NodeModifier = NodeModifier.empty,
        trimInput: Boolean = true,
    )

    def apply[A: StringDecoder as dec](
        inputLabel: String,
        descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
        formProps: Props = Props(),
        inputProps: TextField.Props = TextField.Props(),
    ): Form.Stateful[Any, String, Option[A]] =
      Form(
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(TextField(inputProps)),
        ),
        inputLabel :: Nil,
        { _str =>
          val str: String = if (formProps.trimInput) _str.trim else _str
          Option.when(str.nonEmpty)(str).traverse { str =>
            UIError.form.validate(inputLabel) { dec.decodeSimple(str) }
          }
        },
      )

  }

  object textArea {

    final case class Props(
        width: String = "fit-content",
        padding: String = 10.px,
        display: String = "block",
        labelMod: NodeModifier = NodeModifier.empty,
        trimInput: Boolean = true,
    )

    def apply[A: StringDecoder as dec](
        inputLabel: String,
        descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
        formProps: Props = Props(),
        inputProps: TextArea.Props = TextArea.Props(),
    ): Form.Stateful[Any, String, Option[A]] =
      Form(
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(TextArea(inputProps)),
        ),
        inputLabel :: Nil,
        { _str =>
          val str: String = if (formProps.trimInput) _str.trim else _str
          Option.when(str.nonEmpty)(str).traverse { str =>
            UIError.form.validate(inputLabel) { dec.decodeSimple(str) }
          }
        },
      )

  }

  object horizontalRadio {

    final case class Props(
        width: String = "fit-content",
        padding: String = 10.px,
        display: String = "block",
        labelMod: NodeModifier = NodeModifier.empty,
    )

    def apply[A](
        inputLabel: String,
        descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
        show: A => String = (_: A).toString,
        formProps: Props = Props(),
        inputProps: HorizontalRadio.Props.type => HorizontalRadio.Props = _(),
    ): Form.Stateful[Any, HorizontalRadio.State[A], A] =
      Form(
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(HorizontalRadio[A](inputProps, show)),
        ),
        inputLabel :: Nil,
        _.selected.asRight,
      )

  }

  object dropdown {

    final case class Props(
        width: String = "fit-content",
        padding: String = 10.px,
        display: String = "block",
        labelMod: NodeModifier = NodeModifier.empty,
    )

    def apply[A](
        inputLabel: String,
        descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
        show: A => String = (_: A).toString,
        showEmpty: String = "",
        showSetNone: Specified[String] = Specified.WasNotSpecified,
        formProps: Props = Props(),
        inputProps: Dropdown.Props.type => Dropdown.Props = _(),
    ): Form.Stateful[Any, Dropdown.State[A], Option[A]] =
      Form(
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(Dropdown[A](inputProps, show, showEmpty, showSetNone)),
        ),
        inputLabel :: Nil,
        _.selected.asRight,
      )

  }

  object submitButton {

    def apply(
        text: String,
        buttonProps: Button.Props.type => Button.Props = _(),
    ): Form.Stateless[Any, Unit] =
      Form.unit(
        div(
          padding := 10.px,
          Button(
            text,
            buttonProps,
          )(
            onClick.action(Form.Submit),
          ),
        ),
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class OnSubmitBuilder[Env, State, Value](form: Form.Stateful[Env, State, Value]) {

    def asv[Action2]: OnSubmitBuilderASV[Env, Action2, State, Value] = OnSubmitBuilderASV(form)

    def sv[Env2 <: Env: HasNoScope](f: (WidgetState[State], Value) => ZIO[Env2 & Scope, UIError, Unit]): WidgetES[Env2, State] =
      form.widget.handleActionStateful.s { case (s, Form.Submit) =>
        form.stateToValue(s.renderTimeValue) match {
          case Right(value) => f(s, value)
          case Left(errors) => ZIO.fail(errors)
        }
      }

  }

  final class OnSubmitBuilderASV[Env, Action2, State, Value](form: Form.Stateful[Env, State, Value]) {

    def apply[Env2 <: Env: HasNoScope](f: (WidgetState[State], RaiseHandler[Any, Action2], Value) => ZIO[Env2 & Scope, UIError, Unit]): WidgetEAS[Env2, Action2, State] =
      form.widget.handleActionStateful.as[Action2] { case (s, rh, Form.Submit) =>
        form.stateToValue(s.renderTimeValue) match {
          case Right(value) => f(s, rh, value)
          case Left(errors) => ZIO.fail(errors)
        }
      }

  }

}
