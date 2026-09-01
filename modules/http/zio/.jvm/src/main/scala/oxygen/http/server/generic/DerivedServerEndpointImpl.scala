package oxygen.http.server.generic

import oxygen.http.core.*
import oxygen.http.core.partial.ResponseCodecNoStatus
import oxygen.http.model.ServerErrors
import oxygen.http.model.internal.*
import oxygen.http.server.*
import oxygen.predef.core.*
import oxygen.schema.AnySchemaT
import oxygen.zio.ExtractedCauses
import zio.*
import zio.http.*
import zio.stream.*

trait DerivedServerEndpointImpl[Api, In] {

  val apiName: String
  val endpointName: String
  val doc: Option[String]

  protected val requestCodec: RequestCodec[In]

  protected def convertError(cause: Cause[RequestDecodingFailure], errorConfig: ServerErrorConfig): URIO[Scope, Response]

  protected def convertSuccess(api: Api)(in: In, errorConfig: ServerErrorConfig): URIO[Scope, Response]

  final def handle(api: Api)(input: EndpointInput): Option[URIO[Scope, Option[Response]]] =
    requestCodec.decode(input.request) match {
      case FinalRequestParseResult.NotFound       => None
      case FinalRequestParseResult.Success(value) =>
        Some(
          convertSuccess(api)(value, input.errorConfig).asSome,
        )
      case FinalRequestParseResult.Error(error)   => Some(convertError(Cause.fail(error), input.errorConfig).asSome)
      case FinalRequestParseResult.Effect(effect) =>
        Some(
          effect.unsome.foldCauseZIO(
            convertError(_, input.errorConfig).asSome,
            ZIO.foreach(_) { convertSuccess(api)(_, input.errorConfig) },
          ),
        )
    }

  protected def makeSchema: EndpointSchema

  final def toEndpoint: Endpoint[Api] =
    Endpoint[Api](makeSchema, handle)

}
object DerivedServerEndpointImpl {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromZIO
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class FromZIO[Api, In, E, A](
      apiName: String,
      endpointName: String,
      doc: Option[String],
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      successResponseCodec: ResponseCodec[A],
      serverErrorHandler: ServerErrorHandler[E],
      impl: (Api, In) => ZIO[Scope, E, A],
  ) extends DerivedServerEndpointImpl[Api, In] {

    override protected def convertError(cause: Cause[RequestDecodingFailure], errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      decodingFailureCauseToResponse(cause, errorConfig, serverErrorHandler, errorResponseCodec)

    override protected def convertSuccess(api: Api)(in: In, errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      impl(api, in).foldCauseZIO(
        typedFailureCauseToResponse(_, errorConfig, serverErrorHandler, errorResponseCodec),
        value => ZIO.succeed(successResponseCodec.encodeResponse(value)),
      )

    override protected def makeSchema: EndpointSchema =
      EndpointSchema(
        apiName = apiName.some,
        endpointName = endpointName,
        requestSchema = requestCodec.schemaAggregator.unsafeBuild(apiName, endpointName),
        successResponseSchema = successResponseCodec.unsafeBuild(apiName, endpointName),
        errorResponseSchema = errorResponseCodec.unsafeBuild(apiName, endpointName),
        doc = doc,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromSSE
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class FromSSE[Api, In, E, A](
      apiName: String,
      endpointName: String,
      doc: Option[String],
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      schema: AnySchemaT[A],
      serverErrorHandler: ServerErrorHandler[E],
      impl: (Api, In) => ServerSentEvents[E, A],
  ) extends DerivedServerEndpointImpl[Api, In] {

    private val sseResponseCodec: ResponseCodec[Stream[ResponseDecodingFailure, ServerSentEvent[A]]] =
      ResponseCodec.Standard(StatusCodes.Exact(Status.Ok), ResponseCodecNoStatus.sseBody(schema))

    override protected def convertError(cause: Cause[RequestDecodingFailure], errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      decodingFailureCauseToResponse(cause, errorConfig, serverErrorHandler, errorResponseCodec)

    override protected def convertSuccess(api: Api)(in: In, errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      impl(api, in).raw.foldCauseZIO(
        typedFailureCauseToResponse(_, errorConfig, serverErrorHandler, errorResponseCodec),
        value => ZIO.succeed(sseResponseCodec.encodeResponse(value)),
      )

    override protected def makeSchema: EndpointSchema =
      EndpointSchema(
        apiName = apiName.some,
        endpointName = endpointName,
        requestSchema = requestCodec.schemaAggregator.unsafeBuild(apiName, endpointName),
        successResponseSchema = sseResponseCodec.unsafeBuild(apiName, endpointName),
        errorResponseSchema = errorResponseCodec.unsafeBuild(apiName, endpointName),
        doc = doc,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      FromLines
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class FromLines[Api, In, E, A](
      apiName: String,
      endpointName: String,
      doc: Option[String],
      requestCodec: RequestCodec[In],
      errorResponseCodec: ResponseCodec[E],
      schema: AnySchemaT[A],
      serverErrorHandler: ServerErrorHandler[E],
      impl: (Api, In) => LineStream[E, A],
  ) extends DerivedServerEndpointImpl[Api, In] {

    private val linesResponseCodec: ResponseCodec[Stream[ResponseDecodingFailure, A]] =
      ResponseCodec.Standard(StatusCodes.Exact(Status.Ok), ResponseCodecNoStatus.lineStreamBody(schema))

    override protected def convertError(cause: Cause[RequestDecodingFailure], errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      decodingFailureCauseToResponse(cause, errorConfig, serverErrorHandler, errorResponseCodec)

    override protected def convertSuccess(api: Api)(in: In, errorConfig: ServerErrorConfig): URIO[Scope, Response] =
      impl(api, in).raw.foldCauseZIO(
        typedFailureCauseToResponse(_, errorConfig, serverErrorHandler, errorResponseCodec),
        value => ZIO.succeed(linesResponseCodec.encodeResponse(value)),
      )

    override protected def makeSchema: EndpointSchema =
      EndpointSchema(
        apiName = apiName.some,
        endpointName = endpointName,
        requestSchema = requestCodec.schemaAggregator.unsafeBuild(apiName, endpointName),
        successResponseSchema = linesResponseCodec.unsafeBuild(apiName, endpointName),
        errorResponseSchema = errorResponseCodec.unsafeBuild(apiName, endpointName),
        doc = doc,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def serverErrors(cause: ExtractedCauses[RequestDecodingFailure], errorConfig: ServerErrorConfig): ServerErrors =
    ServerErrors.fromCause(
      cause = cause,
      includeTraces = errorConfig.withDefaults.includeTraces,
      includeDefectsOnFailure = errorConfig.withDefaults.includeDefectsOnFailure,
      includeInterruptsOnFailure = errorConfig.withDefaults.includeInterruptsOnFailure,
      includeInterruptsOnDefect = errorConfig.withDefaults.includeInterruptsOnDefect,
    )

  // TODO (KR) : could put error related details in a header potentially?
  //           : `Oxygen-Error-Case`
  private def makeErrorResponse[E](
      extracted: ExtractedCauses[RequestDecodingFailure],
      errorConfig: ServerErrorConfig,
      serverErrorHandler: ServerErrorHandler[E],
      errorResponseCodec: ResponseCodec[E],
  ): UIO[Response] =
    serverErrorHandler.convertCause(extracted, errorConfig) match {
      case Some(typedError) =>
        ZIO.succeed(errorResponseCodec.encodeResponse(typedError))
      case None =>
        extracted match {
          case failures: ExtractedCauses.Failures[RequestDecodingFailure] =>
            val status: Status = if failures.failures.exists(_.value.isMissingAuth) then Status.Unauthorized else Status.BadRequest
            ZIO.succeed(Response(status = status, body = BodyUtil.usingJsonSchema(serverErrors(failures, errorConfig))))
          case noFailures: ExtractedCauses.NoFailures if errorConfig.exposeInternalErrors =>
            ZIO.succeed(Response(status = Status.InternalServerError, body = BodyUtil.usingJsonSchema(serverErrors(noFailures, errorConfig))))
          case _: ExtractedCauses.NoFailures =>
            ZIO.succeed(Response.status(Status.InternalServerError))
        }
    }

  private def decodingFailureCauseToResponse[E](
      cause: Cause[RequestDecodingFailure],
      errorConfig: ServerErrorConfig,
      serverErrorHandler: ServerErrorHandler[E],
      errorResponseCodec: ResponseCodec[E],
  ): UIO[Response] =
    makeErrorResponse(ExtractedCauses.fromCause(cause), errorConfig, serverErrorHandler, errorResponseCodec)

  private def typedFailureCauseToResponse[E](
      extracted: ExtractedCauses[E],
      errorConfig: ServerErrorConfig,
      serverErrorHandler: ServerErrorHandler[E],
      errorResponseCodec: ResponseCodec[E],
  ): UIO[Response] =
    extracted match {
      case failures: ExtractedCauses.Failures[E] =>
        ZIO.succeed(errorResponseCodec.encodeResponse(failures.failures.head.value))
      case noFailures: ExtractedCauses.NoFailures =>
        serverErrorHandler.convertCause(noFailures, errorConfig) match {
          case Some(value) =>
            ZIO.succeed(errorResponseCodec.encodeResponse(value))
          case None if errorConfig.exposeInternalErrors =>
            ZIO.succeed(Response(status = Status.InternalServerError, body = BodyUtil.usingJsonSchema(serverErrors(noFailures, errorConfig))))
          case None =>
            ZIO.succeed(Response.status(Status.InternalServerError))
        }
    }

  private def typedFailureCauseToResponse[E](
      cause: Cause[E],
      errorConfig: ServerErrorConfig,
      serverErrorHandler: ServerErrorHandler[E],
      errorResponseCodec: ResponseCodec[E],
  ): UIO[Response] =
    typedFailureCauseToResponse(ExtractedCauses.fromCause(cause), errorConfig, serverErrorHandler, errorResponseCodec)

}
