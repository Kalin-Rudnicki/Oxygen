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
    path: List[String] = ApiSpecEndpointMiddleware.defaultPath,
    apiName: Option[String] = "oxygen".some,
    endpointName: String = "apiSpec",
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
        paths = NonEmptyList.one(RequestPathsSchema(path.map(RequestPathsSchema.Const(_)).toArraySeq, None)),
        queryParams = ArraySeq.empty,
        headers = ArraySeq.empty,
        body = RequestBodySchema.Empty,
      )

    val schema: EndpointSchema =
      EndpointSchema(
        apiName = apiName,
        endpointName = endpointName,
        requestSchema = requestSchema,
        successResponseSchema = ResponseSchema(ExpectedStatuses.Exact(Status.Ok), ArraySeq.empty, ResponseBodySchema.Empty),
        errorResponseSchema = ResponseSchema(ExpectedStatuses.None, ArraySeq.empty, ResponseBodySchema.Empty),
        doc = s"Serves the compiled oxygen-http API spec as JSON.".some,
      )

    val response: Response = Response(status = Status.Ok, body = BodyUtil.fromString(specJson, MediaType.application.json))

    AppliedEndpoint(
      schema = schema,
      handle = input =>
        // routing already filters by method+path, but self-check keeps this correct under any scan strategy
        if input.request.method == Method.GET && input.request.fullPath == path then Some(ZIO.succeed(Some(response)))
        else None,
    )
  }

}
object ApiSpecEndpointMiddleware {
  val defaultPath: List[String] = List("oxygen", "api-spec")
}
