package oxygen.http.server

import oxygen.http.core.BodyUtil
import oxygen.http.schema.*
import oxygen.http.schema.compiled.*
import oxygen.json.JsonCodec
import oxygen.predef.core.*
import zio.*
import zio.http.{Server as _, *}

/**
  * An [[EndpointMiddleware]] that compiles the schemas of all endpoints it is applied to into a
  * [[RawCompiledApiSpec]] and appends a new endpoint that serves it as JSON.
  *
  * The compilation happens once, when the middleware is applied (server startup). The served spec
  * describes every endpoint present at that point but, by construction, not the spec endpoint itself.
  */
final case class ApiSpecEndpointMiddleware(
    config: ApiSpecEndpointMiddleware.Config,
) extends EndpointMiddleware {

  override def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints] =
    ZIO.succeed {
      val spec: RawCompiledApiSpec = CompiledApiSpec.compile(endpoints.arraySeq.map(_.schema).toSeq)
      val specJson: String = JsonCodec[RawCompiledApiSpec].encoder.encodeJsonStringCompact(spec)
      AppliedEndpoints(endpoints.endpoints ++ Growable.single(makeEndpoint(specJson)))
    }

  private def makeEndpoint(specJson: String): AppliedEndpoint = {
    val requestSchema: RequestSchema =
      RequestSchema(
        method = Method.GET.some,
        paths = NonEmptyList.one(RequestPathsSchema(config.path.map(RequestPathsSchema.Const(_)).toArraySeq, None)),
        queryParams = ArraySeq.empty,
        headers = ArraySeq.empty,
        body = RequestBodySchema.Empty,
      )

    val schema: EndpointSchema =
      EndpointSchema(
        apiName = config.apiName,
        endpointName = config.endpointName,
        requestSchema = requestSchema,
        successResponseSchema = ResponseSchema(ExpectedStatuses.Exact(Status.Ok), ArraySeq.empty, ResponseBodySchema.Empty),
        errorResponseSchema = ResponseSchema(ExpectedStatuses.None, ArraySeq.empty, ResponseBodySchema.Empty),
        doc = s"Serves the compiled oxygen-http API spec as JSON.".some,
        mcp = None,
      )

    // Lift the body's media type into a real Content-Type header — building a Response directly
    // bypasses ResponseCodec.encodeResponse, which is what normally does this.
    val response: Response =
      Response(status = Status.Ok, body = BodyUtil.fromString(specJson, MediaType.application.json))
        .addHeader(Header.ContentType(MediaType.application.json))

    AppliedEndpoint(
      schema = schema,
      handle = input =>
        // routing already filters by method+path, but self-check keeps this correct under any scan strategy
        if input.request.method == Method.GET && input.request.fullPath == config.path then Some(ZIO.succeed(Some(response)))
        else None,
      mcp = None,
    )
  }

}
object ApiSpecEndpointMiddleware {

  val layer: URLayer[ApiSpecEndpointMiddleware.Config, ApiSpecEndpointMiddleware] =
    ZLayer.fromFunction { ApiSpecEndpointMiddleware.apply }

  def middleware: Middlewares[ApiSpecEndpointMiddleware.Config] =
    Middlewares.endpointMiddlewareFromZLayer(layer)

  def defaultMiddleware: Middlewares[Any] =
    Middlewares.endpointMiddlewareFromZLayer(ZLayer.succeed(Config.default) >>> layer)

  final case class Config(
      path: List[String],
      apiName: Option[String],
      endpointName: String,
  )
  object Config {

    val default: Config =
      Config(
        path = List("oxygen", "api-spec"),
        apiName = "oxygen".some,
        endpointName = "apiSpec",
      )

  }

}
