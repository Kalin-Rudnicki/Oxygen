package oxygen.http.server

import oxygen.http.core.*
import oxygen.http.model.*
import oxygen.predef.core.*
import zio.*

final case class RouteHandler[E, A](
    errorResponseCodec: ResponseCodec[E],
    successResponseCodec: ResponseCodec[A],
    errorCodes: HttpCodes[E],
    successCodes: HttpCodes[A],
    serverErrorHandler: ServerErrorHandler[E],
) {

  private def makeResponse[V](value: V, codec: ResponseCodec[V], codes: HttpCodes[V]): HttpResponse = {
    val (headers, body) = codec.encode(value)
    HttpResponse(codes.code(value), headers, body)
  }

  def convertError(error: E): UIO[HttpResponse] = ZIO.succeed { makeResponse(error, errorResponseCodec, errorCodes) }
  def convertSuccess(success: A): HttpResponse = makeResponse(success, successResponseCodec, successCodes)

  // TODO (KR) : error logging
  def convertDefect(cause: Cause[Nothing], exposeInternalErrors: Boolean): UIO[HttpResponse] =
    cause.dieOption match {
      case Some(error) =>
        serverErrorHandler.wrapDeath(error, exposeInternalErrors) match {
          case Some(error)                  => convertError(error)
          case None if exposeInternalErrors => ZIO.succeed(HttpResponse(HttpCode.`500`, error.safeGetMessage))
          case None                         => ZIO.succeed(HttpResponse(HttpCode.`500`))
        }
      case None if exposeInternalErrors => ZIO.succeed(HttpResponse(HttpCode.`500`, cause.prettyPrint))
      case None                         => ZIO.succeed(HttpResponse(HttpCode.`500`))
    }

  def convertDecodingFailure(error: DecodingFailure): UIO[HttpResponse] =
    serverErrorHandler.wrapDecodingFailure(error) match
      case Some(error) => convertError(error)
      case None        => ZIO.succeed(HttpResponse(HttpCode.`400`, error.getMessage))

}
