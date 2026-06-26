package oxygen.http.server

import zio.*

final class Middlewares[-Env] private (private val build: URIO[Env & Scope, CompiledMiddlewares]) {

  def >>>[Env2](that: Middlewares[Env2]): Middlewares[Env & Env2] =
    new Middlewares[Env & Env2](
      for {
        thisMiddlewares <- this.build
        thatMiddlewares <- that.build
      } yield thisMiddlewares >>> thatMiddlewares,
    )

  def addMiddlewares[A: Tag](f: A => URIO[Scope, CompiledMiddlewares]): Middlewares[Env & A] =
    this >>> Middlewares.makeMiddlewares[A](f)

  def addRequest[A: Tag](f: A => URIO[Scope, RequestMiddleware]): Middlewares[Env & A] =
    this >>> Middlewares.makeRequest[A](f)

  def addResponse[A: Tag](f: A => URIO[Scope, ResponseMiddleware]): Middlewares[Env & A] =
    this >>> Middlewares.makeResponse[A](f)

  def addEndpoints[A: Tag](f: A => URIO[Scope, EndpointMiddleware]): Middlewares[Env & A] =
    this >>> Middlewares.makeEndpoints[A](f)

  def toLayer: URLayer[Env, CompiledMiddlewares] =
    ZLayer.scoped { build }

}
object Middlewares {

  val empty: Middlewares[Any] = new Middlewares[Any](ZIO.succeed(CompiledMiddlewares.empty))

  def middlewaresFromZIO[R](f: URIO[R & Scope, CompiledMiddlewares]): Middlewares[R] = new Middlewares[R](f)
  def requestMiddlewareFromZIO[R](f: URIO[R & Scope, RequestMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.map(CompiledMiddlewares.empty >>> _))
  def responseMiddlewareFromZIO[R](f: URIO[R & Scope, ResponseMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.map(CompiledMiddlewares.empty >>> _))
  def endpointMiddlewareFromZIO[R](f: URIO[R & Scope, EndpointMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.map(CompiledMiddlewares.empty >>> _))

  def middlewaresFromZLayer[R](f: URLayer[R, CompiledMiddlewares]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.build.map(_.get[CompiledMiddlewares]))
  def requestMiddlewareFromZLayer[R](f: URLayer[R, RequestMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.build.map(m => CompiledMiddlewares.empty >>> m.get[RequestMiddleware]))
  def responseMiddlewareFromZLayer[R](f: URLayer[R, ResponseMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.build.map(m => CompiledMiddlewares.empty >>> m.get[ResponseMiddleware]))
  def endpointMiddlewareFromZLayer[R](f: URLayer[R, EndpointMiddleware]): Middlewares[R] = Middlewares.middlewaresFromZIO(f.build.map(m => CompiledMiddlewares.empty >>> m.get[EndpointMiddleware]))

  def make[A: {Tag, Middlewares.Make as m}]: Middlewares[A] =
    Middlewares.makeMiddlewares[A] { a => ZIO.succeed(m.middlewares(a)) }

  def makeMiddlewares[A: Tag](f: A => URIO[Scope, CompiledMiddlewares]): Middlewares[A] =
    new Middlewares[A](ZIO.serviceWithZIO[A](f))

  def makeRequest[A: Tag](f: A => URIO[Scope, RequestMiddleware]): Middlewares[A] =
    Middlewares.makeMiddlewares[A] { f(_).map(CompiledMiddlewares.empty >>> _) }

  def makeResponse[A: Tag](f: A => URIO[Scope, ResponseMiddleware]): Middlewares[A] =
    Middlewares.makeMiddlewares[A] { f(_).map(CompiledMiddlewares.empty >>> _) }

  def makeEndpoints[A: Tag](f: A => URIO[Scope, EndpointMiddleware]): Middlewares[A] =
    Middlewares.makeMiddlewares[A] { f(_).map(CompiledMiddlewares.empty >>> _) }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Type Classes
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait Make[A] {
    def middlewares(in: A): CompiledMiddlewares
  }
  object Make {

    def middlewares[A](f: A => CompiledMiddlewares): Middlewares.Make[A] = f(_)
    def request[A](f: A => RequestMiddleware): Middlewares.Make[A] = a => CompiledMiddlewares.empty >>> f(a)
    def response[A](f: A => ResponseMiddleware): Middlewares.Make[A] = a => CompiledMiddlewares.empty >>> f(a)
    def endpoints[A](f: A => EndpointMiddleware): Middlewares.Make[A] = a => CompiledMiddlewares.empty >>> f(a)

  }

}
