package oxygen.http.server

import oxygen.predef.core.*
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
            handle(EndpointInput(request, config.exposeInternalErrors))
          }
        }
      }
      .toRoutes

}
object CompiledEndpoints {

  def layer(
      endpoints: Endpoints,
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): TaskLayer[CompiledEndpoints] =
    ZLayer.scoped { endpoints.compile(requestMiddleware, responseMiddleware, endpointMiddleware) }

  def endpointLayer(
      requestMiddleware: RequestMiddleware = RequestMiddleware.Empty,
      responseMiddleware: ResponseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware: EndpointMiddleware = EndpointMiddleware.Empty,
  ): RLayer[Endpoints, CompiledEndpoints] =
    ZLayer.scoped { ZIO.serviceWithZIO[Endpoints] { _.compile(requestMiddleware, responseMiddleware, endpointMiddleware) } }

  final case class SeqScan(endpoints: Endpoints) extends CompiledEndpoints {

    private val endpointArray: ArraySeq[Endpoint] = endpoints.arraySeq
    private val endpointArrayLength: Int = endpointArray.length

    override def handle(input: EndpointInput): ZIO[Scope, Response, Response] = {
      var idx: Int = 0
      var endpoint: Endpoint = null
      while (idx < endpointArrayLength) {
        endpoint = endpointArray(idx)
        endpoint.handle(input) match
          case Some(effect) => return effect
          case None         => idx = idx + 1
      }

      ZIO.fail(Response(status = Status.NotFound, body = Body.fromString(Status.NotFound.text)))
    }

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
