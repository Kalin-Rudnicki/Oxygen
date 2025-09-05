package oxygen.example.webServer.api

import oxygen.core.generic.*
import oxygen.example.api.*
import oxygen.example.api.model.error.*
import oxygen.example.conversion.domainToApi.*
import oxygen.example.core.model.post.{*, given}
import oxygen.example.core.model.user.{*, given}
import oxygen.example.domain.model as DM
import oxygen.http.core.RequestDecodingFailure
import oxygen.http.server.{DeriveEndpoints, ErrorConverter, ServerErrorHandler}
import oxygen.predef.core.*
import zio.StackTrace

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Errors
//////////////////////////////////////////////////////////////////////////////////////////////////////

private def internalError(exposeInternalErrors: Boolean)(error: Throwable, trace: StackTrace): Option[InternalError] =
  Option.when(exposeInternalErrors)(InternalError(error.safeGetMessage, trace.stackTrace.map(_.toString)))

given ServerErrorHandler[ApiError] =
  new ServerErrorHandler[ApiError] {
    override def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[ApiError] =
      ApiError.InternalServerError(internalError(exposeInternalErrors)(error, trace)).some
    override def wrapDecodingFailure(error: RequestDecodingFailure): Option[ApiError] =
      ApiError.DecodingFailure(error.getMessage).some
  }

given ServerErrorHandler[RegistrationError] =
  new ServerErrorHandler[RegistrationError] {
    override def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[RegistrationError] =
      RegistrationError.InternalServerError(internalError(exposeInternalErrors)(error, trace)).some
    override def wrapDecodingFailure(error: RequestDecodingFailure): Option[RegistrationError] =
      RegistrationError.DecodingFailure(error.getMessage).some
  }

given ServerErrorHandler[LoginError] =
  new ServerErrorHandler[LoginError] {
    override def wrapDeath(error: Throwable, trace: StackTrace, exposeInternalErrors: Boolean): Option[LoginError] =
      LoginError.InternalServerError(internalError(exposeInternalErrors)(error, trace)).some
    override def wrapDecodingFailure(error: RequestDecodingFailure): Option[LoginError] =
      LoginError.DecodingFailure(error.getMessage).some
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
