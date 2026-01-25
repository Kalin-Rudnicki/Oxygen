package oxygen.http.client.generic

import oxygen.http.client.*
import oxygen.http.core.*
import oxygen.http.core.partial.ResponseCodecNoStatus
import oxygen.http.model.internal.*
import oxygen.schema.AnySchemaT
import zio.*
import zio.http.{Client as _, *}
import zio.stream.*

sealed trait DerivedClientEndpointImpl[In, +Out] {

  protected val extras: Client.RequestExtras
  protected val requestCodec: RequestCodec[In]

  protected def makeOut(responseEffect: RIO[Scope, ReceivedResponse]): Out

  final def send(in: In, client: Client): Out =
    makeOut {
      for {
        rawResponse <- client.send(requestCodec.encode(in), extras)
        response <- ReceivedResponse.fromResponse(rawResponse)
      } yield response
    }

}
object DerivedClientEndpointImpl {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait FromZIO[In, R, E, A] extends DerivedClientEndpointImpl[In, ZIO[R, E, A]] {

    protected val errorResponseCodec: ResponseCodec[E]
    protected val successResponseCodec: ResponseCodec[A]
    protected val clientErrorHandler: ClientErrorHandler[E]

    protected def convertR(effect: ZIO[Scope, E, A]): ZIO[R, E, A]

    override protected final def makeOut(responseEffect: RIO[Scope, ReceivedResponse]): ZIO[R, E, A] =
      convertR { responseEffect.convertErrors(clientErrorHandler).flatMap { handleResponse(_, errorResponseCodec, successResponseCodec, clientErrorHandler) } }

  }

  final case class FromZIOScopedEnv[In, E, A](
      extras: Client.RequestExtras,
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      successResponseCodec: ResponseCodec[A],
      clientErrorHandler: ClientErrorHandler[E],
  ) extends FromZIO[In, Scope, E, A] {

    override protected def convertR(effect: ZIO[Scope, E, A]): ZIO[Scope, E, A] = effect

  }

  final case class FromZIOAnyEnv[In, E, A](
      extras: Client.RequestExtras,
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      successResponseCodec: ResponseCodec[A],
      clientErrorHandler: ClientErrorHandler[E],
  ) extends FromZIO[In, Any, E, A] {

    override protected def convertR(effect: ZIO[Scope, E, A]): ZIO[Any, E, A] = ZIO.scoped { effect }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class FromSSE[In, E, A](
      extras: Client.RequestExtras,
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      schema: AnySchemaT[A],
      clientErrorHandler: ClientErrorHandler[E],
  ) extends DerivedClientEndpointImpl[In, ServerSentEvents[E, A]] {

    private val sseResponseCodec: ResponseCodec[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
      ResponseCodec.Standard(StatusCodes.Exact(Status.Ok), ResponseCodecNoStatus.sseBody(schema))

    override protected def makeOut(responseEffect: RIO[Scope, ReceivedResponse]): ServerSentEvents[E, A] =
      ServerSentEvents.makeRaw {
        for {
          response <- responseEffect.convertErrors(clientErrorHandler)
          stream <- handleResponse(response, errorResponseCodec, sseResponseCodec, clientErrorHandler)
        } yield stream.orDie // TODO (KR) : can we do better than dying here?
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromLines
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class FromLines[In, E, A](
      extras: Client.RequestExtras,
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      schema: AnySchemaT[A],
      clientErrorHandler: ClientErrorHandler[E],
  ) extends DerivedClientEndpointImpl[In, LineStream[E, A]] {

    private val linesResponseCodec: ResponseCodec[Stream[ResponseDecodingFailure, A]] =
      ResponseCodec.Standard(StatusCodes.Exact(Status.Ok), ResponseCodecNoStatus.lineStreamBody(schema))

    override protected def makeOut(responseEffect: RIO[Scope, ReceivedResponse]): LineStream[E, A] =
      LineStream.makeRaw {
        for {
          response <- responseEffect.convertErrors(clientErrorHandler)
          stream <- handleResponse(response, errorResponseCodec, linesResponseCodec, clientErrorHandler)
        } yield stream.orDie // TODO (KR) : can we do better than dying here?
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  extension [R2, B](self: ZIO[R2, Throwable, B])
    def convertErrors[E](clientErrorHandler: ClientErrorHandler[E]): ZIO[R2, E, B] =
      self.mapErrorCause { _.foldContext(())(clientErrorHandler) }

  private def handleResponse[E, A](
      response: ReceivedResponse,
      errorResponseCodec: ResponseCodec[E],
      successResponseCodec: ResponseCodec[A],
      clientErrorHandler: ClientErrorHandler[E],
  ): ZIO[Scope, E, A] =
    if successResponseCodec.canLikelyDecode(response.status) then //
      successResponseCodec.decode(response).convertErrors(clientErrorHandler)
    else if errorResponseCodec.canLikelyDecode(response.status) then //
      errorResponseCodec.decode(response).convertErrors(clientErrorHandler).flatMap(ZIO.fail(_))
    else if response.status.isSuccess then // TODO (KR) : special handling? logWarning?
      ZIO.logInfo(s"Received unexpected (success) status code (${response.status}), attempting to decode as success...") *>
        successResponseCodec.decode(response).convertErrors(clientErrorHandler)
    else if response.status.isError then // TODO (KR) : special handling? logWarning?
      ZIO.logInfo(s"Received unexpected (error) status code (${response.status}), attempting to decode as error...") *>
        errorResponseCodec.decode(response).convertErrors(clientErrorHandler).flatMap(ZIO.fail(_))
    else //
      ZIO.dieMessage(s"Unable to handle http response code: ${response.status}").convertErrors(clientErrorHandler)

}
