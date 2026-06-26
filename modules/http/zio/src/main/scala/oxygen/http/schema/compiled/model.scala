package oxygen.http.schema.compiled

import oxygen.http.schema.McpEndpointSchema
import oxygen.json.*
import oxygen.predef.core.*
import oxygen.schema.compiled.{CompiledSchemaRef, RawCompiledSchemas}
import zio.http.Method

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Shared givens
//////////////////////////////////////////////////////////////////////////////////////////////////////

// zio-http's `Method` has no json codec of its own; the compiled spec encodes it by name.
private[compiled] given methodJsonCodec: JsonCodec[Method] =
  JsonCodec.string.transform(Method.fromString, _.name)

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Param kinds
//////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * Mirrors `oxygen.http.schema.ParamType.Param`, but without the behavioral fields — the
  * compiled spec only needs to describe a param's cardinality/optionality.
  */
enum CompiledParamType derives JsonCodec {
  case Required
  case Optional
  case ManyRequired
  case ManyOptional
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Status (deliberately de-emphasized)
//////////////////////////////////////////////////////////////////////////////////////////////////////

/**
  * Status codes are an afterthought in oxygen-http (decoding relies on body discriminators,
  * not status branching). This is a faithful-but-minimal mirror of `ExpectedStatuses`,
  * carried only as a descriptive annotation on a compiled response.
  */
@jsonDiscriminator("type")
enum CompiledExpectedStatuses derives JsonCodec {
  case None
  case All
  case Exact(code: Int)
  case OneOf(codes: ArraySeq[Int])
  case Range(name: String, min: Int, max: Int)
  case Or(a: CompiledExpectedStatuses, b: CompiledExpectedStatuses)
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Request
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class RawCompiledParam(
    name: String,
    doc: Option[String],
    kind: CompiledParamType,
    schema: CompiledSchemaRef,
) derives JsonCodec

@jsonDiscriminator("type")
sealed trait RawCompiledPathSegment derives JsonCodec
object RawCompiledPathSegment {
  final case class Const(path: String) extends RawCompiledPathSegment
  final case class Param(name: String, doc: Option[String], schema: CompiledSchemaRef) extends RawCompiledPathSegment
}

final case class RawCompiledRestParam(
    name: String,
    doc: Option[String],
    schema: CompiledSchemaRef,
    required: Boolean,
) derives JsonCodec

/** One alternative path shape: a sequence of segments, plus an optional trailing rest-param. */
final case class RawCompiledPaths(
    segments: ArraySeq[RawCompiledPathSegment],
    rest: Option[RawCompiledRestParam],
) derives JsonCodec

@jsonDiscriminator("type")
sealed trait RawCompiledRequestBody derives JsonCodec
object RawCompiledRequestBody {
  case object Empty extends RawCompiledRequestBody
  final case class Single(name: String, doc: Option[String], schema: CompiledSchemaRef) extends RawCompiledRequestBody
}

final case class RawCompiledRequest(
    method: Option[Method],
    paths: NonEmptyList[RawCompiledPaths],
    queryParams: ArraySeq[RawCompiledParam],
    headers: ArraySeq[RawCompiledParam],
    body: RawCompiledRequestBody,
) derives JsonCodec

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Response
//////////////////////////////////////////////////////////////////////////////////////////////////////

@jsonDiscriminator("type")
sealed trait RawCompiledResponseBody derives JsonCodec
object RawCompiledResponseBody {
  case object Empty extends RawCompiledResponseBody
  final case class Single(schema: CompiledSchemaRef) extends RawCompiledResponseBody
  final case class ServerSentEvents(schema: CompiledSchemaRef) extends RawCompiledResponseBody
  final case class LineStream(schema: CompiledSchemaRef) extends RawCompiledResponseBody
}

/** For sum-typed responses: which case (by name) is returned under which status code. */
final case class RawCompiledCaseStatus(
    caseName: String,
    code: Int,
) derives JsonCodec

/** Near-identical to a request, minus path/query, plus the de-emphasized status annotation. */
final case class RawCompiledResponse(
    headers: ArraySeq[RawCompiledParam],
    body: RawCompiledResponseBody,
    status: CompiledExpectedStatuses,
    caseStatuses: ArraySeq[RawCompiledCaseStatus],
) derives JsonCodec

//////////////////////////////////////////////////////////////////////////////////////////////////////
//      Endpoint / Api / Spec
//////////////////////////////////////////////////////////////////////////////////////////////////////

final case class RawCompiledEndpoint(
    name: String,
    doc: Option[String],
    request: RawCompiledRequest,
    successResponse: RawCompiledResponse,
    errorResponse: RawCompiledResponse,
    mcp: Option[McpEndpointSchema],
) derives JsonCodec

final case class RawCompiledApi(
    name: Option[String],
    doc: Option[String],
    endpoints: ArraySeq[RawCompiledEndpoint],
) derives JsonCodec

/**
  * The serializable HTTP API spec — the analogue of `oxygen.schema.compiled.RawCompiledSchemas`,
  * but for endpoints. All type references are `CompiledSchemaRef`s into the single shared
  * `schemas` bundle. This is what gets served (Phase 2), rendered (Phase 3), and diffed (Phase 4).
  */
final case class RawCompiledApiSpec(
    apis: ArraySeq[RawCompiledApi],
    schemas: RawCompiledSchemas,
) derives JsonCodec {

  def withoutLineNos: RawCompiledApiSpec = copy(schemas = schemas.withoutLineNos)

  def toFullCompiledApiSpec: FullCompiledApiSpec = FullCompiledApiSpec(this)

}
