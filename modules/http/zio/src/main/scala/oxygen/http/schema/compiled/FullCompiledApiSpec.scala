package oxygen.http.schema.compiled

import oxygen.predef.core.*
import oxygen.schema.compiled.{CompiledSchemaRef, FullCompiledSchema, FullCompiledSchemas}

/**
  * The reference-resolved view of a [[RawCompiledApiSpec]], analogous to
  * `oxygen.schema.compiled.FullCompiledSchemas`. The endpoint structure is unchanged (it still
  * holds `CompiledSchemaRef`s), but the shared type bundle is resolved into a navigable graph so
  * consumers (the Phase 3 UI renderer, `show`) can walk a ref to its full definition.
  */
final case class FullCompiledApiSpec(raw: RawCompiledApiSpec) {

  val schemas: FullCompiledSchemas = raw.schemas.toFullCompiledSchemas

  def apis: ArraySeq[RawCompiledApi] = raw.apis

  def resolve(ref: CompiledSchemaRef): FullCompiledSchema = schemas.resolve(ref)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Show
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def showParam(label: String, p: RawCompiledParam): String =
    s"    - $label ${p.name} (${p.kind}): ${p.schema.showCore}${p.doc.fold("")(d => s"  // $d")}"

  private def showBody(body: RawCompiledRequestBody): String = body match
    case RawCompiledRequestBody.Empty             => "    - body: <empty>"
    case RawCompiledRequestBody.Single(n, _, ref) => s"    - body $n: ${ref.showCore}"

  private def showBody(body: RawCompiledResponseBody): String = body match
    case RawCompiledResponseBody.Empty               => "    - body: <empty>"
    case RawCompiledResponseBody.Single(ref)         => s"    - body: ${ref.showCore}"
    case RawCompiledResponseBody.ServerSentEvents(r) => s"    - body (SSE): ${r.showCore}"
    case RawCompiledResponseBody.LineStream(r)       => s"    - body (lines): ${r.showCore}"

  private def showSegment(seg: RawCompiledPathSegment): String = seg match
    case RawCompiledPathSegment.Const(path)      => path
    case RawCompiledPathSegment.Param(n, _, ref) => s"{$n: ${ref.showBase}}"

  private def showPaths(paths: RawCompiledPaths): String = {
    val base: String = paths.segments.map(showSegment).mkString("/", "/", "")
    paths.rest.fold(base)(r => s"$base/{${r.name}...: ${r.schema.showBase}}")
  }

  private def showEndpoint(ep: RawCompiledEndpoint): String = {
    val method: String = ep.request.method.fold("<any>")(_.name)
    val pathLines: String = ep.request.paths.toList.map(p => s"    - $method ${showPaths(p)}").mkString("\n")
    List[Option[String]](
      Some(s"  [${ep.name}]${ep.doc.fold("")(d => s"  // $d")}"),
      Some(pathLines),
      Option.when(ep.request.queryParams.nonEmpty)(ep.request.queryParams.map(showParam("query", _)).mkString("\n")),
      Option.when(ep.request.headers.nonEmpty)(ep.request.headers.map(showParam("header", _)).mkString("\n")),
      Some(showBody(ep.request.body)),
      Some(s"    - success [${ep.successResponse.status}]: ${showBody(ep.successResponse.body).trim}"),
      Some(s"    - error [${ep.errorResponse.status}]: ${showBody(ep.errorResponse.body).trim}"),
    ).flatten.mkString("\n")
  }

  def show: String = {
    val apiSections: String =
      raw.apis
        .map { api =>
          val header: String = api.name.getOrElse("<root>")
          s"$header:\n${api.endpoints.map(showEndpoint).mkString("\n")}"
        }
        .mkString("\n\n")

    s"""=== Endpoints ===
       |$apiSections
       |
       |=== Schemas ===${schemas.show}""".stripMargin
  }

  override def toString: String = show

}
