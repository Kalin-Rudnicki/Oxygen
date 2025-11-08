package oxygen.ui.web

import oxygen.predef.core.*
import scala.annotation.targetName

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

}
object FormValue {

  def success[Value](value: Value): FormValue[Any, Value] = FormValue(Nil, _ => FormResult.Success(value))

  extension [StateGet, Value1](self: FormValue[StateGet, Value1]) {

    def <*>[Value2](that: FormValue[StateGet, Value2])(using zip: Zip[Value1, Value2]): FormValue[StateGet, zip.Out] =
      FormValue(self.fields ++ that.fields, state => self.valueResult(state).zipWith(that.valueResult(state))(zip.zip))

    def <*[Value2](that: FormValue[StateGet, Value2]): FormValue[StateGet, Value1] =
      FormValue(self.fields ++ that.fields, state => self.valueResult(state).zipWith(that.valueResult(state))((v1, _) => v1))

    def *>[Value2](that: FormValue[StateGet, Value2]): FormValue[StateGet, Value2] =
      FormValue(self.fields ++ that.fields, state => self.valueResult(state).zipWith(that.valueResult(state))((_, v2) => v2))

  }

  extension [StateGet, Value](self: FormValue[StateGet, PForm.Result[Value]]) {

    def absolve: FormValue[StateGet, Value] =
      FormValue(self.fields, self.valueResult(_).flatMap(identity))

  }

}
