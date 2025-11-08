package oxygen.ui.web

import oxygen.predef.core.*

sealed trait FormResult[+Value] {

  final def toOption: Option[Value] = this match
    case FormResult.Success(value) => value.some
    case _: FormResult.Error       => None

  final def map[Value2](f: Value => Value2): FormResult[Value2] = this match
    case FormResult.Success(value) => FormResult.Success(f(value))
    case error: FormResult.Error   => error

  final def flatMap[Value2](f: Value => FormResult[Value2]): FormResult[Value2] = this match
    case FormResult.Success(value) => f(value)
    case error: FormResult.Error   => error

  final def flatMapEitherResult[Value2](f: Value => PForm.EitherResult[Value2]): FormResult[Value2] = this match
    case FormResult.Success(value) => FormResult.fromEitherResult(f(value))
    case error: FormResult.Error   => error

  final def flatMapEitherMessage[Value2](fields: List[String])(f: Value => PForm.EitherMessage[Value2]): FormResult[Value2] = this match
    case FormResult.Success(value) => FormResult.fromEitherMessage(fields, f(value))
    case error: FormResult.Error   => error

  final def flatMapEitherMessages[Value2](fields: List[String])(f: Value => PForm.EitherMessages[Value2]): FormResult[Value2] = this match
    case FormResult.Success(value) => FormResult.fromEitherMessages(fields, f(value))
    case error: FormResult.Error   => error

  final def flatMapEitherError[Value2](fields: List[String])(f: Value => PForm.EitherError[Value2]): FormResult[Value2] = this match
    case FormResult.Success(value) => FormResult.fromEitherError(fields, f(value))
    case error: FormResult.Error   => error

  final def toEither: PForm.EitherResult[Value] = this match
    case FormResult.Success(value) => value.asRight
    case FormResult.Error(error)   => error.asLeft

  final def zipWith[Value2, Out](that: FormResult[Value2])(f: (Value, Value2) => Out): FormResult[Out] =
    (this, that) match
      case (FormResult.Success(a), FormResult.Success(b))   => FormResult.Success(f(a, b))
      case (error: FormResult.Error, FormResult.Success(_)) => error
      case (FormResult.Success(_), error: FormResult.Error) => error
      case (FormResult.Error(a), FormResult.Error(b))       => FormResult.Error(a ++ b)

}
object FormResult {

  // TODO (KR) : allow form to update the underlying widget with the error and/or raise page message(s)
  // type Error[StateGet, StateSet] = Ior[StateGet => StateSet, NonEmptyList[UIError.ClientSide.FormValidationErrors.Error]]

  final case class Success[+Value](value: Value) extends FormResult[Value]
  final case class Error(error: UIError.ClientSide.FormValidationErrors) extends FormResult[Nothing]

  def fromEitherResult[Value](either: PForm.EitherResult[Value]): FormResult[Value] = either match
    case Right(value) => Success(value)
    case Left(error)  => Error(error)

  def fromEitherMessage[Value](fields: List[String], either: PForm.EitherMessage[Value]): FormResult[Value] = either match
    case Right(value) => Success(value)
    case Left(error)  => Error(UIError.form.invalid(fields, error))

  def fromEitherMessages[Value](fields: List[String], either: PForm.EitherMessages[Value]): FormResult[Value] = either match
    case Right(value) => Success(value)
    case Left(error)  => Error(UIError.form.invalid(fields, error))

  def fromEitherError[Value](fields: List[String], either: PForm.EitherError[Value]): FormResult[Value] = either match
    case Right(value) => Success(value)
    case Left(error)  => Error(UIError.form.invalid(fields, error))

}
