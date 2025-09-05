package oxygen.http.server

import oxygen.http.core.{BodyUtil, RequestDecodingFailure, ResponseCodec}
import oxygen.predef.core.*
import zio.*
import zio.http.{Body, Response, Status}

final case class ResponseHandler[E, A](
    errorResponseCodec: ResponseCodec[E],
    successResponseCodec: ResponseCodec[A],
    serverErrorHandler: ServerErrorHandler[E],
) {

  private def makeResponse[V](value: V, codec: ResponseCodec[V]): Response = {
    val (status, headers, body) = codec.encode(value)
    Response(
      status = status,
      headers = headers,
      body = body,
    )
  }

  def errorResponse(error: E): Response = makeResponse(error, errorResponseCodec)
  def successResponse(success: A): Response = makeResponse(success, successResponseCodec)

  // TODO (KR) : error logging
  //           : add Show[ApiError] + FiberRef[Option[DomainError]], and then log a message like:
  //           : api-error: <...>, domain-error: <...>
  def convertDefect(cause: Cause[Nothing], exposeInternalErrors: Boolean): UIO[Response] =
    cause.find { case Cause.Die(value, trace) => (value, trace) } match {
      case Some((error, trace)) =>
        serverErrorHandler.wrapDeath(error, trace, exposeInternalErrors) match {
          case Some(error)                  => ZIO.succeed(errorResponse(error))
          case None if exposeInternalErrors => ZIO.succeed(Response(status = Status.InternalServerError, body = BodyUtil.fromString(error.safeGetMessage)))
          case None                         => ZIO.succeed(Response(status = Status.InternalServerError))
        }
      case None if exposeInternalErrors => ZIO.succeed(Response(status = Status.InternalServerError, body = BodyUtil.fromString(cause.prettyPrint)))
      case None                         => ZIO.succeed(Response(status = Status.InternalServerError))
    }

  def convertCause(cause: Cause[E], exposeInternalErrors: Boolean): UIO[Response] =
    cause.failureOrCause match {
      case Left(failure) => ZIO.succeed(errorResponse(failure))
      case Right(defect) => convertDefect(defect, exposeInternalErrors)
    }

  def convertDecodingFailure(error: RequestDecodingFailure): UIO[Response] =
    serverErrorHandler.wrapDecodingFailure(error) match
      case Some(error) => ZIO.succeed(errorResponse(error))
      case None        => ZIO.succeed(Response(status = Status.BadRequest, body = BodyUtil.fromString(error.safeGetMessage)))

}
