package oxygen.http.server

import oxygen.http.schema.*
import oxygen.predef.core.*
import zio.*
import zio.http.{Method, Response, Status}

/**
  * A PUBLIC factory for a hand-built, const-path oxygen-http endpoint backed by a raw handler.
  *
  * It generalizes the schema construction that [[ApiSpecEndpointMiddleware]] already does internally
  * (building a [[RequestSchema]] / [[EndpointSchema]] / [[AppliedEndpoint]] by hand behind the
  * `private[http]` schema constructors), so that a module outside `oxygen-http` can mount a raw route
  * as a real oxygen-http endpoint (a route in the endpoint tree) rather than a standalone zio-http
  * `Routes` or an [[EndpointMiddleware]].
  *
  *   - The [[RequestSchema]] is the const path (`method` + one [[RequestPathsSchema.Const]] per
  *     segment), no query params / headers / body schema.
  *   - The [[EndpointSchema]] is trivial: an `Ok` success response and a `None` error response schema
  *     (matching [[ApiSpecEndpointMiddleware]]).
  *   - The [[AppliedEndpoint.handle]] matches on `method` + full path: it returns
  *     `Some(handle(input).map(_.some))` only when the request matches, and `None` otherwise — so an
  *     unmatched request falls through to the next endpoint (it does NOT succeed-with-404).
  */
object HttpRoute {

  def apply(
      apiName: Option[String],
      endpointName: String,
      method: Method,
      path: List[String],
      doc: Option[String],
  )(
      handle: EndpointInput => URIO[Scope, Response],
  ): AppliedEndpoint = {
    val requestSchema: RequestSchema =
      RequestSchema(
        method = method.some,
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
        doc = doc,
      )

    AppliedEndpoint(
      schema = schema,
      handle = input =>
        // routing already filters by method+path, but the self-check keeps this correct under any scan strategy
        if input.request.method == method && input.request.fullPath == path then Some(handle(input).map(_.some))
        else None,
    )
  }

}
