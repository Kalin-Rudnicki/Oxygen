package oxygen.ui.web

import monocle.Lens
import oxygen.predef.core.*
import oxygen.ui.web.component.Wizard
import scala.annotation.targetName
import zio.*

final case class FormValue[-StateGet, +Value](fields: List[String], valueResult: StateGet => PForm.Result[Value]) {

  def mapValue[Value2](f: Value => Value2): FormValue[StateGet, Value2] =
    FormValue(fields, valueResult(_).map(f))
  def mapValueEitherMessage[Value2](f: Value => PForm.EitherMessage[Value2]): FormValue[StateGet, Value2] =
    FormValue(fields, valueResult(_).flatMapEitherMessage(fields)(f))
  def mapValueEitherMessages[Value2](f: Value => PForm.EitherMessages[Value2]): FormValue[StateGet, Value2] =
    FormValue(fields, valueResult(_).flatMapEitherMessages(fields)(f))
  def mapValueEitherError[Value2](f: Value => PForm.EitherError[Value2]): FormValue[StateGet, Value2] =
    FormValue(fields, valueResult(_).flatMapEitherError(fields)(f))

  def result: FormValue[StateGet, PForm.Result[Value]] =
    FormValue(fields, state => FormResult.Success(valueResult(state)))

  @targetName("validateValue_single")
  def validateValue(f: Value => PForm.EitherMessage[Unit]): FormValue[StateGet, Value] =
    FormValue(
      fields,
      state => valueResult(state).flatMap { value => FormResult.fromEitherMessage(fields, f(value)).map(_ => value) },
    )

  @targetName("validateValue_many")
  def validateValue(f: Value => PForm.EitherMessages[Unit]): FormValue[StateGet, Value] =
    FormValue(
      fields,
      state => valueResult(state).flatMap { value => FormResult.fromEitherMessages(fields, f(value)).map(_ => value) },
    )

  def valueEffect[State <: StateGet](state: WidgetState[State]): IO[UIError.ClientSide.FormValidationErrors, Value] =
    state.currentValue.flatMap { s => ZIO.fromEither { valueResult(s).toEither } }

}
object FormValue {

  def success[Value](value: Value): FormValue[Any, Value] = FormValue(Nil, _ => FormResult.Success(value))

  extension [StateGet, Value1](self: FormValue[StateGet, Value1]) {

    def <*>[Value2](that: FormValue[StateGet, Value2])(using zip: Zip[Value1, Value2]): FormValue[StateGet, zip.Out] =
      self.zipWith(that)(zip.zip)

    def <*[Value2](that: FormValue[StateGet, Value2]): FormValue[StateGet, Value1] =
      self.zipWith(that)((v1, _) => v1)

    def *>[Value2](that: FormValue[StateGet, Value2]): FormValue[StateGet, Value2] =
      self.zipWith(that)((_, v2) => v2)

    def zipWith[Value2, Value3](that: FormValue[StateGet, Value2])(f: (Value1, Value2) => Value3): FormValue[StateGet, Value3] =
      FormValue(self.fields ++ that.fields, state => self.valueResult(state).zipWith(that.valueResult(state))(f))

    def optionWizard: Wizard[StateGet, Value1, Option[Value1]] =
      Wizard(self, _.toOption)

    def zoomOutLens[State2](lens: Lens[State2, StateGet]): FormValue[State2, Value1] =
      FormValue(
        self.fields,
        s => self.valueResult(lens.get(s)),
      )

  }

  extension [StateGet, Value](self: FormValue[StateGet, PForm.Result[Value]]) {

    def absolve: FormValue[StateGet, Value] =
      FormValue(self.fields, self.valueResult(_).flatMap(identity))

  }

}
