package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.*

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Form
//////////////////////////////////////////////////////////////////////////////////////////////////////

// Form.Stateful
type FormEAS[-Env, +Action, State, +Value] = PForm.Stateful[Env, Action, State, Value]
type FormES[-Env, State, +Value] = PForm.Stateful[Env, Nothing, State, Value]
type FormAS[+Action, State, +Value] = PForm.Stateful[Any, Action, State, Value]
type FormS[State, +Value] = PForm.Stateful[Any, Nothing, State, Value]

// Form.Stateless
type FormEA[-Env, +Action, +Value] = PForm.Stateless[Env, Action, Value]
type FormE[-Env, +Value] = PForm.Stateless[Env, Nothing, Value]
type FormA[+Action, +Value] = PForm.Stateless[Any, Action, Value]
type Form[+Value] = PForm.Stateless[Any, Nothing, Value]

// SubmitForm.Stateful
type SubmitFormEAS[-Env, +Action, State, +Value] = PForm.Stateful[Env, Action | Form.Submit, State, Value]
type SubmitFormES[-Env, State, +Value] = PForm.Stateful[Env, Form.Submit, State, Value]
type SubmitFormAS[+Action, State, +Value] = PForm.Stateful[Any, Action | Form.Submit, State, Value]
type SubmitFormS[State, +Value] = PForm.Stateful[Any, Form.Submit, State, Value]

// SubmitForm.Stateless
type SubmitFormEA[-Env, +Action, +Value] = PForm.Stateless[Env, Action | Form.Submit, Value]
type SubmitFormE[-Env, +Value] = PForm.Stateless[Env, Form.Submit, Value]
type SubmitFormA[+Action, +Value] = PForm.Stateless[Any, Action | Form.Submit, Value]
type SubmitForm[+Value] = PForm.Stateless[Any, Form.Submit, Value]

object Form {

  type Polymorphic[-Env, +Action, -StateGet, +StateSet <: StateGet, +Value] = PForm.Polymorphic[Env, Action, StateGet, StateSet, Value]
  type Stateless[-Env, +Action, +Value] = PForm.Stateless[Env, Action, Value]
  type Stateful[-Env, +Action, State, +Value] = PForm.Stateful[Env, Action, State, Value]

  type Submit = Submit.type
  case object Submit

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply[Env, Action, StateGet, StateSet <: StateGet, Value](
      widget: Widget.Polymorphic[Env, Action, StateGet, StateSet],
      fields: List[String],
      stateToValue: StateGet => PForm.Result[Value],
  ): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
    PForm(widget, fields, stateToValue)

  def const[Value](value: Value): Form[Value] =
    Form(Widget.empty, Nil, _ => FormResult.Success(value))

  def const[Env, Action, StateGet, StateSet <: StateGet, Value](widget: Widget.Polymorphic[Env, Action, StateGet, StateSet], value: Value): Form.Polymorphic[Env, Action, StateGet, StateSet, Value] =
    Form(widget, Nil, _ => FormResult.Success(value))

  def succeed[Env, Action, State, Value](widget: Widget.Stateful[Env, Action, State])(stateToValue: State => Value): Form.Stateful[Env, Action, State, Value] =
    Form(widget, Nil, s => FormResult.Success(stateToValue(s)))

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
    PForm(Widget.fragment.apply(before*).apply(form.widget).apply(after*), form.value)

  /////// make ///////////////////////////////////////////////////////////////

  def makeWithValidation[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormResult.fromEitherMessage(fields, stateToValue(s)))

  def makeWithValidation[Env, Action, State, Value](
      field: String,
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidation(field :: Nil, widget)(stateToValue)

  def makeWithValidation[Env, Action, State, Value](
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessage[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidation(Nil, widget)(stateToValue)

  def makeWithValidations[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormResult.fromEitherMessages(fields, stateToValue(s)))

  def makeWithValidations[Env, Action, State, Value](
      field: String,
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidations(field :: Nil, widget)(stateToValue)

  def makeWithValidations[Env, Action, State, Value](
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => PForm.EitherMessages[Value],
  ): Form.Stateful[Env, Action, State, Value] =
    Form.makeWithValidations(Nil, widget)(stateToValue)

  def makeWith[Env, Action, State, Value](
      fields: List[String],
      widget: Widget.Stateful[Env, Action, State],
  )(
      stateToValue: State => Value,
  ): Form.Stateful[Env, Action, State, Value] =
    Form(widget, fields, s => FormResult.Success(stateToValue(s)))

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

}
