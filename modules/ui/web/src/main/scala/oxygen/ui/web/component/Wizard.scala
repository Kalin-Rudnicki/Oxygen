package oxygen.ui.web.component

import oxygen.ui.web.{FormResult, FormValue, PForm}

final case class Wizard[State, Value, Out](
    formValue: FormValue[State, Value],
    toOut: FormResult[Value] => Out,
) {

  def flatMap[Env, Action, Value2](f: Out => Form.Stateful[Env, Action, State, Value2]): Form.Stateful[Env, Action, State, Value2] =
    PForm.FlatMapWizard(this, f)

}
