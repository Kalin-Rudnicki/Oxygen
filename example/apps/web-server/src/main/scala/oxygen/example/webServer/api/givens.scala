package oxygen.example.webServer.api

import oxygen.example.api.*
import oxygen.example.api.model.error.*
import oxygen.example.conversion.domainToApi.*
import oxygen.example.core.model.post.{*, given}
import oxygen.example.core.model.user.{*, given}
import oxygen.example.domain.model as DM
import oxygen.http.core.RequestDecodingFailure
import oxygen.http.server.{DeriveEndpoints, ErrorConverter, ServerErrorConfig, ServerErrorHandler}
import oxygen.predef.core.*
import oxygen.zio.ExtractedCauses

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Errors
//////////////////////////////////////////////////////////////////////////////////////////////////////

given ServerErrorHandler[ApiError] =
  new ServerErrorHandler[ApiError] {
    override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[ApiError] =
      cause match {
        case ExtractedCauses.Failures(failures, _, _) =>
          failures.find(_.value.isMissingAuth) match
            case Some(value) => ApiError.Unauthorized(value.value.show, None).some
            case None        => ApiError.DecodingFailure(failures.head.value.show).some
        case noFailures: ExtractedCauses.NoFailures =>
          ApiError.InternalServerError(errorConfig.serverErrors(noFailures)).some
      }
  }

given ServerErrorHandler[RegistrationError] =
  new ServerErrorHandler[RegistrationError] {
    override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[RegistrationError] =
      cause match {
        case ExtractedCauses.Failures(failures, _, _) =>
          RegistrationError.DecodingFailure(failures.head.value.show).some
        case noFailures: ExtractedCauses.NoFailures =>
          RegistrationError.InternalServerError(errorConfig.serverErrors(noFailures)).some
      }
  }

given ServerErrorHandler[LoginError] =
  new ServerErrorHandler[LoginError] {
    override def convertCause(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): Option[LoginError] =
      cause match {
        case ExtractedCauses.Failures(failures, _, _) =>
          LoginError.DecodingFailure(failures.head.value.show).some
        case noFailures: ExtractedCauses.NoFailures =>
          LoginError.InternalServerError(errorConfig.serverErrors(noFailures)).some
      }
  }

given ServerErrorHandler[UIApiError] = ServerErrorHandler.notHandled

given ErrorConverter[DM.error.RegistrationError, RegistrationError] = _.toApi.asRight
given ErrorConverter[DM.error.LoginError, LoginError] = _.toApi.asRight
given ErrorConverter[DM.error.ConnectionError, ApiError] = _.toApi.asRight
given ErrorConverter[DM.error.DomainError, ApiError] = _.toApi.asRight

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Derive Endpoints
//////////////////////////////////////////////////////////////////////////////////////////////////////

given DeriveEndpoints[UserApi] = DeriveEndpoints.derived
given DeriveEndpoints[ConnectionApi] = DeriveEndpoints.derived
given DeriveEndpoints[PostApi] = DeriveEndpoints.derived
given DeriveEndpoints[UIApi] = DeriveEndpoints.derived
given DeriveEndpoints[StreamApi] = DeriveEndpoints.derived

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Show
//////////////////////////////////////////////////////////////////////////////////////////////////////

// TODO (KR) : move to companion objects?
given Show[Email] = _.email
given Show[UserId] = _.id.toString
given Show[PostId] = _.id.toString
given Show[CommentId] = _.id.toString
given Show[DM.error.RegistrationError] = Show.derived
given Show[DM.error.LoginError] = Show.derived
given Show[DM.error.ConnectionError] = Show.derived
given Show[DM.error.DomainError] = Show.derived
