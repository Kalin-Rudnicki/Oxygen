package oxygen.ui.web

import oxygen.http.core.RequestDecodingFailure
import oxygen.predef.core.*
import oxygen.ui.web.internal.NavigationEvent
import scala.annotation.targetName

sealed trait UIError extends Throwable {

  override final def toString: String = getMessage
  override final def getMessage: String = this match
    case UIError.Redirect(navEvent)                                   => s"Attempting to redirect: $navEvent"
    case UIError.ClientSide.UserActionable(message)                   => message
    case UIError.ClientSide.InternalDefect(userMessage, _)            => userMessage
    case UIError.ClientSide.FormValidationErrors(errors)              => errors.map(_.show).mkString("\n")
    case UIError.ServerSide.UserActionable(message)                   => message
    case UIError.ServerSide.InternalDefect(userMessage, _)            => userMessage
    case UIError.Internal.DirectPageNavigationToPageNotInRouter(page) => s"Page is missing from route table: ${page.pageName}"
    case UIError.Internal.UrlNotFound(url)                            => s"Not found: ${url.formatted}"
    case UIError.Internal.PageParamDecodingFailure(error)             => s"Error decoding page url: $error"

  final def toPageMessages: NonEmptyList[PageMessage] = this match
    case UIError.ClientSide.FormValidationErrors(errors) => errors.map(e => PageMessage.error(e.show))
    case _                                               => NonEmptyList.one(PageMessage.error(getMessage))

}
object UIError {
  // TODO (KR) : Have something like the Server error mapper, that logs domain errors before converting to api errors.
  //           : Here, we can log api errors before converting to UI errors.

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object form {

    def missingRequired(field: String): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.missingRequired(field)

    def missingRequired(fields: List[String]): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.missingRequired(fields)

    @targetName("invalid_single_single")
    def invalid(field: String, message: String): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.invalid(field, message)

    @targetName("invalid_single_many")
    def invalid(field: String, messages: NonEmptyList[String]): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.invalid(field, messages)

    @targetName("invalid_many_single")
    def invalid(fields: List[String], message: String): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.invalid(fields, message)

    @targetName("invalid_many_many")
    def invalid(fields: List[String], messages: NonEmptyList[String]): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.invalid(fields, messages)

    @targetName("invalid_many_singleError")
    def invalid(fields: List[String], error: UIError.ClientSide.FormValidationErrors.ErrorType): UIError.ClientSide.FormValidationErrors =
      UIError.ClientSide.FormValidationErrors.invalid(fields, error)

    @targetName("validate_single_single")
    def validate[A](field: String)(value: Either[String, A]): Either[UIError.ClientSide.FormValidationErrors, A] =
      value.leftMap(form.invalid(field, _))

    @targetName("validate_single_many")
    def validate[A](field: String)(value: EitherNel[String, A]): Either[UIError.ClientSide.FormValidationErrors, A] =
      value.leftMap(form.invalid(field, _))

    @targetName("validate_many_single")
    def validate[A](fields: List[String])(value: Either[String, A]): Either[UIError.ClientSide.FormValidationErrors, A] =
      value.leftMap(form.invalid(fields, _))

    @targetName("validate_many_many")
    def validate[A](fields: List[String])(value: EitherNel[String, A]): Either[UIError.ClientSide.FormValidationErrors, A] =
      value.leftMap(form.invalid(fields, _))

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Types
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Redirect(navEvent: NavigationEvent) extends UIError

  sealed trait NonRedirect extends UIError
  sealed trait Standard extends UIError.NonRedirect

  sealed trait ClientSide extends Standard
  object ClientSide {

    final case class UserActionable(message: String) extends ClientSide

    // TODO (KR) : can we do better here? stuff like whether the error is likely to resolve itself, should the user reload, help text?
    //           : what if there was a link to a FAQ that had help with known errors?
    final case class InternalDefect(userMessage: String, internalMessage: Option[String]) extends ClientSide
    object InternalDefect {
      private val somethingWentWrongMessage: String = "The UI encountered an unexpected error"
      val somethingWentWrong: InternalDefect = InternalDefect(somethingWentWrongMessage, None)
      def somethingWentWrong(internalMessage: String): InternalDefect = InternalDefect(somethingWentWrongMessage, internalMessage.some)
      def somethingWentWrong(internalMessage: Throwable): InternalDefect = somethingWentWrong(internalMessage.safeGetMessage)
      def somethingWentWrong(internalMessage: Option[String]): InternalDefect = InternalDefect(somethingWentWrongMessage, internalMessage)
    }

    final case class FormValidationErrors(errors: NonEmptyList[FormValidationErrors.Error]) extends ClientSide {

      def ++(that: FormValidationErrors): FormValidationErrors =
        FormValidationErrors(this.errors ++ that.errors)

    }
    object FormValidationErrors {

      def missingRequired(field: String): FormValidationErrors =
        FormValidationErrors(NonEmptyList.one(Error(field :: Nil, ErrorType.MissingRequired)))

      def missingRequired(fields: List[String]): FormValidationErrors =
        FormValidationErrors(NonEmptyList.one(Error(fields, ErrorType.MissingRequired)))

      @targetName("invalid_single_single")
      def invalid(field: String, message: String): FormValidationErrors =
        FormValidationErrors(NonEmptyList.one(Error(field :: Nil, ErrorType.Invalid(message))))

      @targetName("invalid_single_many")
      def invalid(field: String, messages: NonEmptyList[String]): FormValidationErrors =
        FormValidationErrors(messages.map { message => Error(field :: Nil, ErrorType.Invalid(message)) })

      @targetName("invalid_many_single")
      def invalid(fields: List[String], message: String): FormValidationErrors =
        FormValidationErrors(NonEmptyList.one(Error(fields, ErrorType.Invalid(message))))

      @targetName("invalid_many_many")
      def invalid(fields: List[String], messages: NonEmptyList[String]): FormValidationErrors =
        FormValidationErrors(messages.map { message => Error(fields, ErrorType.Invalid(message)) })

      @targetName("invalid_many_singleError")
      def invalid(fields: List[String], error: FormValidationErrors.ErrorType): FormValidationErrors =
        FormValidationErrors(NonEmptyList.one(Error(fields, error)))

      final case class Error(fields: List[String], error: ErrorType) {

        def show: String =
          fields match {
            case Nil          => error.show
            case field :: Nil =>
              error match {
                case ErrorType.MissingRequired  => s"Missing required value for $field"
                case ErrorType.Invalid(message) => s"Invalid value for $field: $message"
              }
            case _ =>
              s"Error with fields ${fields.mkString(", ")}: ${error.show}"
          }

      }

      enum ErrorType { // TODO (KR) : have other options
        case MissingRequired
        case Invalid(message: String)

        final def show: String = this match
          case ErrorType.MissingRequired  => "Missing required value"
          case ErrorType.Invalid(message) => message

      }

    }

  }

  sealed trait ServerSide extends Standard
  object ServerSide {

    final case class UserActionable(message: String) extends ServerSide

    // TODO (KR) : can we do better here? stuff like whether the error is likely to resolve itself, should the user reload, help text?
    //           : what if there was a link to a FAQ that had help with known errors?
    final case class InternalDefect(userMessage: String, internalMessage: Option[String]) extends ServerSide
    object InternalDefect {
      private val somethingWentWrongMessage: String = "An unexpected server error occurred"
      val somethingWentWrong: InternalDefect = InternalDefect(somethingWentWrongMessage, None)
      def somethingWentWrong(internalMessage: String): InternalDefect = InternalDefect(somethingWentWrongMessage, internalMessage.some)
      def somethingWentWrong(internalMessage: Throwable): InternalDefect = somethingWentWrong(internalMessage.safeGetMessage)
      def somethingWentWrong(internalMessage: Option[String]): InternalDefect = InternalDefect(somethingWentWrongMessage, internalMessage)
    }

  }

  /////// Internal ///////////////////////////////////////////////////////////////

  private[web] sealed trait Internal extends UIError.NonRedirect
  private[web] object Internal {
    final case class DirectPageNavigationToPageNotInRouter(page: RoutablePage[?]) extends Internal
    final case class UrlNotFound(url: PageURL) extends Internal
    final case class PageParamDecodingFailure(error: RequestDecodingFailure) extends Internal
  }

}
