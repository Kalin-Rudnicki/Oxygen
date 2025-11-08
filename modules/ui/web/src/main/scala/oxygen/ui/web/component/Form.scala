package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Form
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Form.Stateful
type FormEAS[-Env, +Action, State, +Value] = FormWidget.Stateful[Env, Action, State, Value]
type FormES[-Env, State, +Value] = FormWidget.Stateful[Env, Nothing, State, Value]
type FormAS[+Action, State, +Value] = FormWidget.Stateful[Any, Action, State, Value]
type FormS[State, +Value] = FormWidget.Stateful[Any, Nothing, State, Value]

// Form.Stateless
type FormEA[-Env, +Action, +Value] = FormWidget.Stateless[Env, Action, Value]
type FormE[-Env, +Value] = FormWidget.Stateless[Env, Nothing, Value]
type FormA[+Action, +Value] = FormWidget.Stateless[Any, Action, Value]
type Form[+Value] = FormWidget.Stateless[Any, Nothing, Value]

// SubmitForm.Stateful
type SubmitFormEAS[-Env, +Action, State, +Value] = FormWidget.Stateful[Env, Action | Form.Submit, State, Value]
type SubmitFormES[-Env, State, +Value] = FormWidget.Stateful[Env, Form.Submit, State, Value]
type SubmitFormAS[+Action, State, +Value] = FormWidget.Stateful[Any, Action | Form.Submit, State, Value]
type SubmitFormS[State, +Value] = FormWidget.Stateful[Any, Form.Submit, State, Value]

// SubmitForm.Stateless
type SubmitFormEA[-Env, +Action, +Value] = FormWidget.Stateless[Env, Action | Form.Submit, Value]
type SubmitFormE[-Env, +Value] = FormWidget.Stateless[Env, Form.Submit, Value]
type SubmitFormA[+Action, +Value] = FormWidget.Stateless[Any, Action | Form.Submit, Value]
type SubmitForm[+Value] = FormWidget.Stateless[Any, Form.Submit, Value]

object Form {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = FormWidget.Polymorphic[Env, Action, StateGet, StateSet, Value]
  type Stateless[-Env, +Action, +Value] = FormWidget.Stateless[Env, Action, Value]
  type Stateful[-Env, +Action, State, +Value] = FormWidget.Stateful[Env, Action, State, Value]

  type Submit = Submit.type
  case object Submit

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[Env, Action, StateGet, StateSet <: StateGet, Value](
      widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
      fields: List[String],
      stateToValue: StateGet => FormWidget.Result[Value],
  ): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
    FormWidget(widget, fields, stateToValue)

  def const[Value](value: Value): Form[Value] =
    Form(Widget.empty, Nil, _ => FormWidget.Result.Success(value))

  def const[Env, Action, StateGet, StateSet <: StateGet, Value](widget: Widget.Polymorphic[Env, Action, StateGet, StateSet], value: Value): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
    Form(widget, Nil, _ => FormWidget.Result.Success(value))

  def succeed[Env, Action, State, Value](widget: Widget.Stateful[Env, Action, State])(stateToValue: State => Value): Form.Stateful[Env, Action, State, Value] =
    Form(widget, Nil, s => FormWidget.Result.Success(stateToValue(s)))

  def wrap[Env, Action, StateGet, StateSet <: StateGet](widget: Widget.Polymorphic[Env, Action, StateGet, StateSet]): Form.Polymorphic[Env, Action, StateGet, StateSet, Unit] =
    Form.const(widget, ())

  def unit[Env, Action, StateGet, StateSet <: StateGet](widget: Widget.Polymorphic[Env, Action, StateGet, StateSet]): Form.Polymorphic[Env, Action, StateGet, StateSet, Unit] =
    Form.const(widget, ())

  def fragment[Env, Action, StateGet, StateSet <: StateGet, Value](
      before: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
  )(
      form: Form.Polymorphic[Env, Action, StateGet, StateSet, Value],
  )(
      after: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
  ): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
    Form(Widget.fragment.apply(before*).apply(form.widget).apply(after*), form.fields, form.stateToValue)

  /////// make ///////////////////////////////////////////////////////////////

  def makeWithValidation[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormWidget.Result.fromEitherMessage(fields, stateToValue(s)))

  def makeWithValidation[Env, Action, State, Value](
      field: String,
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidation(field :: Nil, widget)(stateToValue)

  def makeWithValidation[Env, Action, State, Value](
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidation(Nil, widget)(stateToValue)

  def makeWithValidations[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormWidget.Result.fromEitherMessages(fields, stateToValue(s)))

  def makeWithValidations[Env, Action, State, Value](
      field: String,
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidations(field :: Nil, widget)(stateToValue)

  def makeWithValidations[Env, Action, State, Value](
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => FormWidget.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidations(Nil, widget)(stateToValue)

  def makeWith[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => Value,
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormWidget.Result.Success(stateToValue(s)))

  def makeWith[Env, Action, State, Value](
      field: String,
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => Value,
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWith(field :: Nil, widget)(stateToValue)

  def makeWith[Env, Action, State, Value](
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => Value,
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWith(Nil, widget)(stateToValue)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form Elems
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def elementLabel(
      labelText: String,
      descriptionText: Specified[Widget.Const] = Specified.WasNotSpecified,
      labelMod: NodeModifier = NodeModifier.empty,
  ): Widget.Const =
    Widget.fragment(
      div(label(labelText, padding(S.spacing._2, S.spacing._5), fontWeight := S.fontWeight.semiBold, fontSize := S.fontSize._4)(labelMod)),
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
    ): SubmitFormS[String, Option[A]] =
      Form.makeWithValidation(
        inputLabel,
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(TextField(inputProps)),
        ),
      ) { _str =>
        val str: String = if (formProps.trimInput) _str.trim else _str
        Option.when(str.nonEmpty)(str).traverse(dec.decodeSimple)
      }

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
    ): SubmitFormS[String, Option[A]] =
      Form.makeWithValidation(
        inputLabel,
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(TextArea(inputProps)),
        ),
      ) { _str =>
        val str: String = if (formProps.trimInput) _str.trim else _str
        Option.when(str.nonEmpty)(str).traverse(dec.decodeSimple)
      }

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
        radioDecorator: HorizontalRadio.Decorator => HorizontalRadio.Decorator = identity,
    ): FormS[HorizontalRadio.State[A], A] =
      Form.makeWith(
        inputLabel,
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(HorizontalRadio[A].show(show).decorate(radioDecorator)),
        ),
      )(_.selected)

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
        formProps: Props = Props(),
        dropdownDecorator: Dropdown.Decorator => Dropdown.Decorator = identity,
    ): FormS[Dropdown.State[A], Option[A]] =
      Form.makeWith(
        inputLabel,
        div(
          width := formProps.width,
          padding := formProps.padding,
          elementLabel(inputLabel, descriptionText, formProps.labelMod),
          div(Dropdown[A].show(show).decorate(dropdownDecorator)),
        ),
      )(_.selected)

  }

  object submitButton {

    def apply(
        buttonMainText: String,
        buttonDecorator: Button.Decorator,
    ): SubmitForm[Unit] =
      Form.unit(
        div(
          padding := 10.px,
          Button(
            buttonMainText,
            buttonDecorator,
          )(
            onClick.action(Form.Submit),
          ),
        ),
      )

    def apply(
        buttonMainText: String,
        buttonDecorator: Button.Decorator => Button.Decorator,
    ): SubmitForm[Unit] =
      Form.submitButton(
        buttonMainText,
        buttonDecorator(Button.Decorator.defaultStyling),
      )

    def apply(
        buttonMainText: String,
    ): SubmitForm[Unit] =
      Form.submitButton(
        buttonMainText,
        Button.Decorator.defaultStyling,
      )

  }

}
