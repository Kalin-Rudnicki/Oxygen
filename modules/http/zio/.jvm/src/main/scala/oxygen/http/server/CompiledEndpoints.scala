package oxygen.http.server

import oxygen.http.core.BodyUtil
import oxygen.http.model.internal.*
import oxygen.predef.core.*
import oxygen.zio.syntax.log.*
import zio.*
import zio.http.{Server as _, *}

trait CompiledEndpoints {

  def handle(input: EndpointInput): ZIO[Scope, Response, Response]

  final def withRequestMiddleware(requestMiddleware: RequestMiddleware): CompiledEndpoints =
    requestMiddleware match
      case RequestMiddleware.Empty => this
      case _                       => CompiledEndpoints.WithRequestMiddleware(this, requestMiddleware)

  final def withResponseMiddleware(responseMiddleware: ResponseMiddleware): CompiledEndpoints =
    responseMiddleware match
      case ResponseMiddleware.Empty => this
      case _                        => CompiledEndpoints.WithResponseMiddleware(this, responseMiddleware)

  final def withMiddleware(
      requestMiddleware: RequestMiddleware,
      responseMiddleware: ResponseMiddleware,
  ): CompiledEndpoints =
    this.withRequestMiddleware(requestMiddleware).withResponseMiddleware(responseMiddleware)

  final def toRoutes(config: Server.Config): Routes[Any, Response] =
    Handler
      .scoped[Any] {
        Handler.fromFunctionHandler[Request] { request =>
          Handler.fromZIO[Scope, Response, Response] {
            ZIO.scope.flatMap { scope =>
              ReceivedRequest.fromRequest(request).flatMap { request =>
                CurrentRequest.ref.locallyScoped(CurrentRequest(request, scope).some) *>
                  ZIO.logInfoAnnotated("Handling http request", "method" -> request.method.name, "path" -> request.url.path.encode) *>
                  handle(EndpointInput(request, config.errorConfig))
                    .tapDefect { ZIO.logErrorCause("Unhandled defect", _) }
              }
            }
          }
        }
      }
      .toRoutes

}
object CompiledEndpoints {

  def layer(
      endpoints: AppliedEndpoints,
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): TaskLayer[CompiledEndpoints] =
    ZLayer.scoped { endpoints.compile(requestMiddleware, responseMiddleware, endpointMiddleware) }

  def endpointLayer(
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): RLayer[AppliedEndpoints, CompiledEndpoints] =
    ZLayer.scoped { ZIO.serviceWithZIO[AppliedEndpoints] { _.compile(requestMiddleware, responseMiddleware, endpointMiddleware) } }

  final case class SeqScan(endpoints: AppliedEndpoints) extends CompiledEndpoints {

    private val endpointArray: ArraySeq[AppliedEndpoint] = endpoints.arraySeq
    private val endpointArrayLength: Int = endpointArray.length

    private def loop(idx: Int, input: EndpointInput): ZIO[Scope, Response, Response] =
      if idx < endpointArrayLength then {
        val endpoint: AppliedEndpoint = endpointArray(idx)
        endpoint.handle(input) match {
          case Some(maybeResponse) =>
            maybeResponse.flatMap {
              case Some(response) => ZIO.succeed(response)
              case None           => loop(idx + 1, input)
            }
          case None => loop(idx + 1, input)
        }
      } else //
        ZIO.fail(Response(status = Status.NotFound, body = BodyUtil.fromString(Status.NotFound.text)))

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      loop(0, input)

  }

  // TODO (KR) : TreeScan

  final case class WithRequestMiddleware(
      underlying: CompiledEndpoints,
      requestMiddleware: RequestMiddleware,
  ) extends CompiledEndpoints {

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      requestMiddleware(input.request)
        .flatMap { newRequest => underlying.handle(input.copy(request = newRequest)) }

  }

  final case class WithResponseMiddleware(
      underlying: CompiledEndpoints,
      responseMiddleware: ResponseMiddleware,
  ) extends CompiledEndpoints {

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
      underlying.handle(input).flatMap { responseMiddleware.apply }

  }

}
