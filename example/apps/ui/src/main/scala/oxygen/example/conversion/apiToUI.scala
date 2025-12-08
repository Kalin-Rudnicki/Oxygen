package oxygen.example.conversion

import oxygen.example.api.model.error.*
import oxygen.ui.web.UIError
import zio.*

object apiToUI {

  extension (self: RegistrationError)
    def toUI: UIError.ServerSide =
      self match {
        case RegistrationError.EmailAlreadyExists(_)      => UIError.ServerSide.UserActionable("A user with that email already exists.\nTry logging in?")
        case RegistrationError.DecodingFailure(message)   => UIError.ServerSide.InternalDefect.somethingWentWrong(s"decoding failure: $message")
        case RegistrationError.InternalServerError(error) => UIError.ServerSide.InternalDefect.somethingWentWrong(error.map(_.errors.map(_.simpleMessage).mkString("\n\n")))
      }

  extension (self: LoginError)
    def toUI: UIError.ServerSide =
      self match {
        case LoginError.InvalidCredentials         => UIError.ServerSide.UserActionable("Unable to login with those credentials")
        case LoginError.DecodingFailure(message)   => UIError.ServerSide.InternalDefect.somethingWentWrong(s"decoding failure: $message")
        case LoginError.InternalServerError(error) => UIError.ServerSide.InternalDefect.somethingWentWrong(error.map(_.errors.map(_.simpleMessage).mkString("\n\n")))
      }

  extension (self: ApiError)
    def toUI: UIError.ServerSide =
      self match {
        case ApiError.InvalidToken(None)                 => UIError.ServerSide.InternalDefect.somethingWentWrong("Invalid token")
        case ApiError.InvalidToken(Some(message))        => UIError.ServerSide.InternalDefect.somethingWentWrong(s"Invalid token: $message")
        case ApiError.Unauthorized(_, Some(userMessage)) => UIError.ServerSide.UserActionable(userMessage)
        case ApiError.Forbidden(_, Some(userMessage))    => UIError.ServerSide.UserActionable(userMessage)
        case ApiError.Conflict(_, Some(userMessage))     => UIError.ServerSide.UserActionable(userMessage)
        case ApiError.NotFound(_, Some(userMessage))     => UIError.ServerSide.UserActionable(userMessage)
        case ApiError.Unauthorized(message, None)        => UIError.ServerSide.InternalDefect.somethingWentWrong(message)
        case ApiError.Forbidden(message, None)           => UIError.ServerSide.InternalDefect.somethingWentWrong(message)
        case ApiError.Conflict(message, None)            => UIError.ServerSide.InternalDefect.somethingWentWrong(message)
        case ApiError.NotFound(message, None)            => UIError.ServerSide.InternalDefect.somethingWentWrong(message)
        case ApiError.DecodingFailure(message)           => UIError.ServerSide.InternalDefect.somethingWentWrong(s"decoding failure: $message")
        case ApiError.InternalServerError(error)         => UIError.ServerSide.InternalDefect.somethingWentWrong(error.map(_.errors.map(_.simpleMessage).mkString("\n\n")))
      }

  extension [R, E, A](self: ZIO[R, E, A])
    def toUILogged[E2](f: E => E2): ZIO[R, E2, A] =
      self
        .tapErrorCause { ZIO.logDebugCause("Mapping API error to UI error", _) }
        .mapError(f)

}
