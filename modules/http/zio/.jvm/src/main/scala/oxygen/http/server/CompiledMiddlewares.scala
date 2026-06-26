package oxygen.http.server

final case class CompiledMiddlewares(
    requestMiddleware: RequestMiddleware,
    responseMiddleware: ResponseMiddleware,
    endpointMiddleware: EndpointMiddleware,
) {

  def >>>(that: CompiledMiddlewares): CompiledMiddlewares =
    CompiledMiddlewares(
      requestMiddleware = this.requestMiddleware >>> that.requestMiddleware,
      responseMiddleware = this.responseMiddleware >>> that.responseMiddleware,
      endpointMiddleware = this.endpointMiddleware >>> that.endpointMiddleware,
    )

  def >>>(that: RequestMiddleware): CompiledMiddlewares =
    CompiledMiddlewares(
      requestMiddleware = this.requestMiddleware >>> that,
      responseMiddleware = this.responseMiddleware,
      endpointMiddleware = this.endpointMiddleware,
    )

  def >>>(that: ResponseMiddleware): CompiledMiddlewares =
    CompiledMiddlewares(
      requestMiddleware = this.requestMiddleware,
      responseMiddleware = this.responseMiddleware >>> that,
      endpointMiddleware = this.endpointMiddleware,
    )

  def >>>(that: EndpointMiddleware): CompiledMiddlewares =
    CompiledMiddlewares(
      requestMiddleware = this.requestMiddleware,
      responseMiddleware = this.responseMiddleware,
      endpointMiddleware = this.endpointMiddleware >>> that,
    )

}
object CompiledMiddlewares {

  val empty: CompiledMiddlewares =
    CompiledMiddlewares(
      requestMiddleware = RequestMiddleware.Empty,
      responseMiddleware = ResponseMiddleware.Empty,
      endpointMiddleware = EndpointMiddleware.Empty,
    )

}
