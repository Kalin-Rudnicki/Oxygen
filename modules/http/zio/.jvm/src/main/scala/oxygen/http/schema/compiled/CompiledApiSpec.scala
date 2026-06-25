package oxygen.http.schema.compiled

import oxygen.http.schema.*
import oxygen.http.server.EndpointSchema
import oxygen.predef.core.*
import oxygen.schema.{AnySchema, JsonSchema, PlainTextSchema}
import oxygen.schema.compiled.{Compiled, CompiledSchemaRef}

/**
  * Runtime compiler: in-memory `EndpointSchema`s (produced by macro derivation) → the serializable
  * [[RawCompiledApiSpec]]. Pure runtime, no macros — the analogue of `oxygen.schema.compiled.SchemaCompiler`.
  *
  * Every `AnySchema` across every endpoint is threaded through a single `Compiled` program, so all
  * referenced types land in one shared, deduplicated `RawCompiledSchemas` bundle and the spec itself
  * holds only `CompiledSchemaRef`s.
  *
  * Input is just `Seq[EndpointSchema]` so this is independent of any running server — both the Phase 2
  * serving middleware and the Phase 4 compatibility test feed it the same way.
  */
object CompiledApiSpec {

  def compile(endpoints: Seq[EndpointSchema]): RawCompiledApiSpec = {
    val program: Compiled[ArraySeq[(Option[String], RawCompiledEndpoint)]] =
      traverse(endpoints.toList) { es => compileEndpoint(es).map(ep => (es.apiName, ep)) }

    val out: Compiled.Output[ArraySeq[(Option[String], RawCompiledEndpoint)]] = program.compiled

    // group by apiName, preserving first-seen order
    val apiNamesInOrder: ArraySeq[Option[String]] = out.value.map(_._1).distinct
    val apis: ArraySeq[RawCompiledApi] =
      apiNamesInOrder.map { name =>
        RawCompiledApi(name, None, out.value.filter(_._1 == name).map(_._2))
      }

    RawCompiledApiSpec(apis, out.schemas)
  }

  /** Strip source line numbers for a stable, diff-friendly artifact (persistence / compat checking). */
  def compileWithoutLineNos(endpoints: Seq[EndpointSchema]): RawCompiledApiSpec =
    compile(endpoints).withoutLineNos

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** The one bridge from a live `AnySchema` to a compiled ref, dispatching the plain/json union. */
  private def compileAny(schema: AnySchema): Compiled[CompiledSchemaRef] =
    schema match
      case p: PlainTextSchema[?] => Compiled.plain(p)
      case j: JsonSchema[?]      => Compiled.json(j)

  private def compileEndpoint(es: EndpointSchema): Compiled[RawCompiledEndpoint] =
    for {
      request <- compileRequest(es.requestSchema)
      success <- compileResponse(es.successResponseSchema)
      error <- compileResponse(es.errorResponseSchema)
    } yield RawCompiledEndpoint(es.endpointName, es.doc, request, success, error)

  private def compileRequest(req: RequestSchema): Compiled[RawCompiledRequest] =
    for {
      paths <- traverse(req.paths.toList)(compilePaths)
      queryParams <- traverse(req.queryParams.toList)(compileParam(_))
      headers <- traverse(req.headers.toList)(compileParam(_))
      body <- compileRequestBody(req.body)
    } yield RawCompiledRequest(
      method = req.method,
      paths = NonEmptyList.fromList(paths.toList).getOrElse(throw new RuntimeException("RequestSchema.paths was empty")),
      queryParams = queryParams,
      headers = headers,
      body = body,
    )

  private def compilePaths(p: RequestPathsSchema): Compiled[RawCompiledPaths] =
    for {
      segments <- traverse(p.singles.toList)(compileSingle)
      rest <- p.rest match
        case Some(r) => compileAny(r.schema).map(ref => RawCompiledRestParam(r.name, r.doc, ref, r.required).some)
        case None    => Compiled.succeed(Option.empty[RawCompiledRestParam])
    } yield RawCompiledPaths(segments, rest)

  private def compileSingle(s: RequestPathsSchema.Single): Compiled[RawCompiledPathSegment] =
    s match
      case RequestPathsSchema.Const(path)                 => Compiled.succeed(RawCompiledPathSegment.Const(path))
      case RequestPathsSchema.SingleParam(name, sch, doc) => compileAny(sch).map(ref => RawCompiledPathSegment.Param(name, doc, ref))

  private def compileRequestBody(body: RequestBodySchema): Compiled[RawCompiledRequestBody] =
    body match
      case RequestBodySchema.Empty                  => Compiled.succeed(RawCompiledRequestBody.Empty)
      case RequestBodySchema.Single(name, sch, doc) => compileAny(sch).map(ref => RawCompiledRequestBody.Single(name, doc, ref))

  private def compileResponse(resp: ResponseSchema): Compiled[RawCompiledResponse] =
    for {
      headers <- traverse(resp.headers.toList)(compileResponseHeader)
      body <- compileResponseBody(resp.body)
    } yield RawCompiledResponse(
      headers,
      body,
      compileStatuses(resp.expectedStatuses),
      resp.caseStatuses.map { case (name, status) => RawCompiledCaseStatus(name, status.code) },
    )

  private def compileResponseBody(body: ResponseBodySchema): Compiled[RawCompiledResponseBody] =
    body match
      case ResponseBodySchema.Empty               => Compiled.succeed(RawCompiledResponseBody.Empty)
      case ResponseBodySchema.Single(sch)         => compileAny(sch).map(RawCompiledResponseBody.Single(_))
      case ResponseBodySchema.ServerSentEvents(s) => compileAny(s).map(RawCompiledResponseBody.ServerSentEvents(_))
      case ResponseBodySchema.LineStream(s)       => compileAny(s).map(RawCompiledResponseBody.LineStream(_))

  private def compileParam(p: RequestQueryParamSchema): Compiled[RawCompiledParam] =
    compileAny(p.schema).map(ref => RawCompiledParam(p.name, p.doc, paramType(p.tpe), ref))

  private def compileParam(p: RequestHeaderSchema): Compiled[RawCompiledParam] =
    compileAny(p.schema).map(ref => RawCompiledParam(p.name, p.doc, paramType(p.tpe), ref))

  private def compileResponseHeader(p: ResponseHeaderSchema): Compiled[RawCompiledParam] =
    compileAny(p.schema).map(ref => RawCompiledParam(p.name, p.doc, paramType(p.tpe), ref))

  private def paramType(p: ParamType.Param): CompiledParamType =
    p match
      case ParamType.Param.Required     => CompiledParamType.Required
      case ParamType.Param.Optional     => CompiledParamType.Optional
      case ParamType.Param.ManyRequired => CompiledParamType.ManyRequired
      case ParamType.Param.ManyOptional => CompiledParamType.ManyOptional

  private def compileStatuses(es: ExpectedStatuses): CompiledExpectedStatuses =
    es match
      case ExpectedStatuses.None            => CompiledExpectedStatuses.None
      case ExpectedStatuses.All             => CompiledExpectedStatuses.All
      case ExpectedStatuses.Exact(status)   => CompiledExpectedStatuses.Exact(status.code)
      case ExpectedStatuses.OneOf(statuses) => CompiledExpectedStatuses.OneOf(statuses.toList.map(_.code).toArraySeq)
      case ExpectedStatuses.Or(a, b)        => CompiledExpectedStatuses.Or(compileStatuses(a), compileStatuses(b))
      case r: ExpectedStatuses.StatusRange  => CompiledExpectedStatuses.Range(r.name, r.min, r.max)

  private def traverse[A, B: scala.reflect.ClassTag](as: List[A])(f: A => Compiled[B]): Compiled[ArraySeq[B]] =
    as.foldRight(Compiled.succeed(List.empty[B])) { (a, acc) => f(a).flatMap(b => acc.map(b :: _)) }
      .map(_.toArraySeq)

}
