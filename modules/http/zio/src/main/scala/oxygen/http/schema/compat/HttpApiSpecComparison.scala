package oxygen.http.schema.compat

import oxygen.http.schema.compiled.*
import oxygen.predef.core.*
import oxygen.schema.compiled.{CompiledSchemaRef, FullCompiledJsonSchema, FullCompiledPlainSchema, FullCompiledSchema, FullCompiledSchemas}
import scala.collection.immutable.ArraySeq

/**
  * Endpoint/API-layer compatibility comparison for the compiled HTTP spec. Diffs two
  * [[RawCompiledApiSpec]]s (an old committed spec vs the current-code spec) and collapses the structural
  * difference to the binary gate the CI check needs, retaining a list of human-readable breaking changes
  * for the failure message.
  *
  * The one non-obvious rule (from the ticket): **request and response compatibility are opposites.** A
  * request is written by the (old) client and read by the (new) server, so it is compatible iff the new
  * server accepts a superset of what the old client could send. A response is the mirror image -- written
  * by the new server, read by the old client -- so every structural rule (added/removed field, required
  * vs optional, nullable, added/removed sum case) flips. Both directions are derived from one
  * [[Dir]]-parameterised set of rules, so they can never drift out of sync.
  *
  * Note: this resolves each side's schema refs against **its own** compiled-schema bundle and walks the
  * structure directly, rather than delegating to `oxygen.schema.compat` -- whose comparison assumes both
  * refs live in a single bundle and so cannot compare two independently-compiled specs where a type keeps
  * its name but changes in place (exactly the common case here). The per-type-shape verdicts mirror that
  * engine's; unhandled/mismatched shapes fall back to structural equality (never a false "compatible").
  */
object HttpApiSpecComparison {

  enum Compatibility {
    case Compatible
    case Incompatible
  }

  /** One breaking change, located for an actionable failure message. */
  final case class BreakingChange(location: String, detail: String)

  final case class Result(breakingChanges: ArraySeq[BreakingChange]) {

    def isCompatible: Boolean = breakingChanges.isEmpty

    def compatibility: Compatibility = if isCompatible then Compatibility.Compatible else Compatibility.Incompatible

    def describe: String =
      if breakingChanges.isEmpty then "(no breaking changes)"
      else breakingChanges.map(c => s"  - [${c.location}] ${c.detail}").mkString("\n")

  }

  /** Diff `from` (old / committed) against `to` (new / current-code). */
  def compare(from: RawCompiledApiSpec, to: RawCompiledApiSpec): Result = {
    given ctx: Ctx = Ctx(from.toFullCompiledApiSpec.schemas, to.toFullCompiledApiSpec.schemas)
    Result(ArraySeq.from(diffApis(from.apis, to.apis)))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal -- direction + resolved-schema context
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private enum Dir {
    case Request
    case Response
  }

  private final case class Ctx(fromSchemas: FullCompiledSchemas, toSchemas: FullCompiledSchemas)

  private def refCompatible(dir: Dir, fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef)(using ctx: Ctx): Boolean =
    compatSchema(dir, ctx.fromSchemas.resolve(fromRef), ctx.toSchemas.resolve(toRef), Set.empty)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal -- structural schema comparison (reader-accepts-superset for requests; mirrored for responses)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def compatSchema(dir: Dir, from: FullCompiledSchema, to: FullCompiledSchema, seen: Set[(CompiledSchemaRef, CompiledSchemaRef)]): Boolean = {
    val key: (CompiledSchemaRef, CompiledSchemaRef) = (from.ref, to.ref)
    if seen.contains(key) then true // a recursive back-edge -- its verdict is decided where it was first entered
    else compatSchemaChecked(dir, from, to, seen + key)
  }

  private def compatSchemaChecked(dir: Dir, from: FullCompiledSchema, to: FullCompiledSchema, seen: Set[(CompiledSchemaRef, CompiledSchemaRef)]): Boolean =
    (from, to) match
      case (f: FullCompiledJsonSchema.Transformed, _)                                     => compatSchema(dir, f.underlyingType.value, to, seen)
      case (_, t: FullCompiledJsonSchema.Transformed)                                     => compatSchema(dir, from, t.underlyingType.value, seen)
      case (f: FullCompiledPlainSchema.Transformed, _)                                    => compatSchema(dir, f.underlyingType.value, to, seen)
      case (_, t: FullCompiledPlainSchema.Transformed)                                    => compatSchema(dir, from, t.underlyingType.value, seen)
      case (f: FullCompiledJsonSchema.JsonProduct, t: FullCompiledJsonSchema.JsonProduct) => compatProduct(dir, f, t, seen)
      case (f: FullCompiledJsonSchema.JsonSum, t: FullCompiledJsonSchema.JsonSum)         => compatSum(dir, f, t, seen)
      case (f: FullCompiledJsonSchema.JsonArray, t: FullCompiledJsonSchema.JsonArray)     => compatSchema(dir, f.elemType.value, t.elemType.value, seen)
      case (f: FullCompiledJsonSchema.JsonMap, t: FullCompiledJsonSchema.JsonMap)         =>
        compatSchema(dir, f.keyType.value, t.keyType.value, seen) && compatSchema(dir, f.valueType.value, t.valueType.value, seen)
      case (f: FullCompiledJsonSchema.JsonString, t: FullCompiledJsonSchema.JsonString) => compatSchema(dir, f.elemType.value, t.elemType.value, seen)
      case (f: FullCompiledJsonSchema.JsonNumber, t: FullCompiledJsonSchema.JsonNumber) => f.numberFormat == t.numberFormat
      case (f: FullCompiledJsonSchema.JsonAST, t: FullCompiledJsonSchema.JsonAST)       => f.jsonType == t.jsonType
      case (f: FullCompiledPlainSchema.Enum, t: FullCompiledPlainSchema.Enum)           => compatEnum(dir, f, t)
      case (_: FullCompiledPlainSchema.PlainText, _: FullCompiledPlainSchema.PlainText) => true
      case _ => from.ref.showBase == to.ref.showBase // unhandled/mismatched shapes: compatible only if structurally identical

  /////// Products ///////////////////////////////////////////////////////////////

  private def fieldRequired(field: FullCompiledJsonSchema.ProductField): Boolean = field.onMissing.isEmpty

  private def compatProduct(dir: Dir, from: FullCompiledJsonSchema.JsonProduct, to: FullCompiledJsonSchema.JsonProduct, seen: Set[(CompiledSchemaRef, CompiledSchemaRef)]): Boolean = {
    val fromByName: Map[String, FullCompiledJsonSchema.ProductField] = from.fields.map(f => f.fieldName -> f).toMap
    val toByName: Map[String, FullCompiledJsonSchema.ProductField] = to.fields.map(f => f.fieldName -> f).toMap
    val addedOk: Boolean = to.fields.filterNot(f => fromByName.contains(f.fieldName)).forall(addedFieldOk(dir, _))
    val removedOk: Boolean = from.fields.filterNot(f => toByName.contains(f.fieldName)).forall(removedFieldOk(dir, _))
    val bothOk: Boolean = from.fields.forall(ff => toByName.get(ff.fieldName).forall(tf => bothFieldOk(dir, ff, tf, seen)))
    addedOk && removedOk && bothOk
  }

  private def addedFieldOk(dir: Dir, field: FullCompiledJsonSchema.ProductField): Boolean =
    dir match
      case Dir.Request  => !fieldRequired(field) // adding a required request field is breaking
      case Dir.Response => true // a new response field is ignorable by the old client

  private def removedFieldOk(dir: Dir, field: FullCompiledJsonSchema.ProductField): Boolean =
    dir match
      case Dir.Request  => true // the server ignores a field the old client still sends
      case Dir.Response => !fieldRequired(field) // removing a required response field breaks the old client

  private def bothFieldOk(dir: Dir, from: FullCompiledJsonSchema.ProductField, to: FullCompiledJsonSchema.ProductField, seen: Set[(CompiledSchemaRef, CompiledSchemaRef)]): Boolean = {
    val fromReq: Boolean = fieldRequired(from)
    val toReq: Boolean = fieldRequired(to)
    val presenceOk: Boolean =
      dir match
        case Dir.Request  => !(!fromReq && toReq) // optional -> required is breaking for requests
        case Dir.Response => !(fromReq && !toReq) // required -> optional is breaking for responses
    val nullableOk: Boolean =
      dir match
        case Dir.Request  => !(from.nullable && !to.nullable) // nullable -> non-nullable is breaking for requests
        case Dir.Response => !(!from.nullable && to.nullable) // non-nullable -> nullable is breaking for responses
    presenceOk && nullableOk && compatSchema(dir, from.fieldType.value, to.fieldType.value, seen)
  }

  /////// Sums ///////////////////////////////////////////////////////////////

  private def compatSum(dir: Dir, from: FullCompiledJsonSchema.JsonSum, to: FullCompiledJsonSchema.JsonSum, seen: Set[(CompiledSchemaRef, CompiledSchemaRef)]): Boolean = {
    val fromByName: Map[String, FullCompiledJsonSchema.SumCase] = from.cases.map(c => c.caseName -> c).toMap
    val toByName: Map[String, FullCompiledJsonSchema.SumCase] = to.cases.map(c => c.caseName -> c).toMap
    val added: Boolean = to.cases.forall(c => fromByName.contains(c.caseName))
    val removed: Boolean = from.cases.forall(c => toByName.contains(c.caseName))
    val addedOk: Boolean = dir match
      case Dir.Request  => true // the old client never sends a newly-added case
      case Dir.Response => added // a newly-added response case breaks the old client's decoder
    val removedOk: Boolean = dir match
      case Dir.Request  => removed // the old client may still send a removed case
      case Dir.Response => true
    val bothOk: Boolean = from.cases.forall(fc => toByName.get(fc.caseName).forall(tc => compatSchema(dir, fc.caseType.value, tc.caseType.value, seen)))
    (from.discriminator == to.discriminator) && addedOk && removedOk && bothOk
  }

  /////// Enums ///////////////////////////////////////////////////////////////

  private def compatEnum(dir: Dir, from: FullCompiledPlainSchema.Enum, to: FullCompiledPlainSchema.Enum): Boolean = {
    val fromValues: Set[String] = from.values.toSet
    val toValues: Set[String] = to.values.toSet
    val setOk: Boolean = dir match
      case Dir.Request  => (fromValues -- toValues).isEmpty // removing an accepted value breaks old requests
      case Dir.Response => (toValues -- fromValues).isEmpty // adding an emitted value breaks the old client
    (from.caseSensitive == to.caseSensitive) && (from.exhaustive == to.exhaustive) && setOk
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal -- structural walk of the spec
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def apiLabel(name: Option[String]): String = name.getOrElse("<root>")
  private def epLabel(apiName: Option[String], endpointName: String): String = s"${apiLabel(apiName)}.$endpointName"

  private def diffApis(from: ArraySeq[RawCompiledApi], to: ArraySeq[RawCompiledApi])(using Ctx): Seq[BreakingChange] = {
    val toByName: Map[Option[String], RawCompiledApi] = to.map(a => a.name -> a).toMap
    val removed: Seq[BreakingChange] = from.filterNot(a => toByName.contains(a.name)).map(a => BreakingChange(apiLabel(a.name), "API removed"))
    val matched: Seq[BreakingChange] = from.flatMap(fa => toByName.get(fa.name).toList.flatMap(ta => diffApi(fa, ta)))
    removed ++ matched
  }

  private def diffApi(from: RawCompiledApi, to: RawCompiledApi)(using Ctx): Seq[BreakingChange] = {
    val toByName: Map[String, RawCompiledEndpoint] = to.endpoints.map(e => e.name -> e).toMap
    val removed: Seq[BreakingChange] = from.endpoints.filterNot(e => toByName.contains(e.name)).map(e => BreakingChange(epLabel(from.name, e.name), "endpoint removed"))
    val matched: Seq[BreakingChange] = from.endpoints.flatMap(fe => toByName.get(fe.name).toList.flatMap(te => diffEndpoint(from.name, fe, te)))
    removed ++ matched
  }

  private def diffEndpoint(apiName: Option[String], from: RawCompiledEndpoint, to: RawCompiledEndpoint)(using Ctx): Seq[BreakingChange] = {
    val loc: String = epLabel(apiName, from.name)
    diffRequest(loc, from.request, to.request) ++
      diffResponse(s"$loc success-response", from.successResponse, to.successResponse) ++
      diffResponse(s"$loc error-response", from.errorResponse, to.errorResponse)
  }

  /////// Request ///////////////////////////////////////////////////////////////

  private def diffRequest(loc: String, from: RawCompiledRequest, to: RawCompiledRequest)(using Ctx): Seq[BreakingChange] = {
    val methodChange: Seq[BreakingChange] =
      if from.method == to.method then Nil
      else Seq(BreakingChange(loc, s"request method changed ${from.method.fold("<any>")(_.name)} -> ${to.method.fold("<any>")(_.name)}"))
    methodChange ++
      diffPaths(loc, from.paths, to.paths) ++
      diffParams(Dir.Request, s"$loc query", from.queryParams, to.queryParams) ++
      diffParams(Dir.Request, s"$loc header", from.headers, to.headers) ++
      diffRequestBody(loc, from.body, to.body)
  }

  private def pathSignature(paths: NonEmptyList[RawCompiledPaths]): List[String] =
    paths.toList.map { p =>
      val segs: String = p.segments.map {
        case RawCompiledPathSegment.Const(c)       => s"/$c"
        case RawCompiledPathSegment.Param(_, _, _) => "/{}"
      }.mkString
      segs + p.rest.fold("")(_ => "/{...}")
    }

  private def pathParamRefs(paths: NonEmptyList[RawCompiledPaths]): List[(String, CompiledSchemaRef)] =
    paths.toList.flatMap { p =>
      val singles: List[(String, CompiledSchemaRef)] = p.segments.toList.collect { case RawCompiledPathSegment.Param(n, _, ref) => (n, ref) }
      singles ++ p.rest.toList.map(r => (r.name, r.schema))
    }

  private def diffPaths(loc: String, from: NonEmptyList[RawCompiledPaths], to: NonEmptyList[RawCompiledPaths])(using Ctx): Seq[BreakingChange] =
    if pathSignature(from) != pathSignature(to) then Seq(BreakingChange(loc, "request path shape changed"))
    else
      pathParamRefs(from).zip(pathParamRefs(to)).flatMap { case ((n, fRef), (_, tRef)) =>
        if refCompatible(Dir.Request, fRef, tRef) then Nil
        else Seq(BreakingChange(s"$loc path '$n'", "incompatible path-param schema change"))
      }

  private def diffRequestBody(loc: String, from: RawCompiledRequestBody, to: RawCompiledRequestBody)(using Ctx): Seq[BreakingChange] =
    (from, to) match
      case (RawCompiledRequestBody.Empty, RawCompiledRequestBody.Empty)                           => Nil
      case (RawCompiledRequestBody.Empty, RawCompiledRequestBody.Single(_, _, _))                 => Seq(BreakingChange(s"$loc body", "added a required request body"))
      case (RawCompiledRequestBody.Single(_, _, _), RawCompiledRequestBody.Empty)                 => Nil // dropping a request body requirement is non-breaking for the server
      case (RawCompiledRequestBody.Single(_, _, fRef), RawCompiledRequestBody.Single(_, _, tRef)) =>
        if refCompatible(Dir.Request, fRef, tRef) then Nil
        else Seq(BreakingChange(s"$loc body", "incompatible request body schema change"))

  /////// Response ///////////////////////////////////////////////////////////////

  private def diffResponse(loc: String, from: RawCompiledResponse, to: RawCompiledResponse)(using Ctx): Seq[BreakingChange] =
    // status codes are deliberately de-emphasised in oxygen-http (decoding keys off body discriminators) -- not gated here.
    diffParams(Dir.Response, s"$loc header", from.headers, to.headers) ++
      diffResponseBody(loc, from.body, to.body)

  private def diffResponseBody(loc: String, from: RawCompiledResponseBody, to: RawCompiledResponseBody)(using Ctx): Seq[BreakingChange] =
    (from, to) match
      case (RawCompiledResponseBody.Empty, RawCompiledResponseBody.Empty)                                   => Nil
      case (RawCompiledResponseBody.Empty, _)                                                               => Nil // the client can ignore a newly-present response body
      case (_, RawCompiledResponseBody.Empty)                                                               => Seq(BreakingChange(s"$loc body", "removed the response body"))
      case (RawCompiledResponseBody.Single(fRef), RawCompiledResponseBody.Single(tRef))                     => respBodySchema(loc, fRef, tRef)
      case (RawCompiledResponseBody.ServerSentEvents(fRef), RawCompiledResponseBody.ServerSentEvents(tRef)) => respBodySchema(loc, fRef, tRef)
      case (RawCompiledResponseBody.LineStream(fRef), RawCompiledResponseBody.LineStream(tRef))             => respBodySchema(loc, fRef, tRef)
      case (_, _)                                                                                           => Seq(BreakingChange(s"$loc body", "response body kind changed"))

  private def respBodySchema(loc: String, fromRef: CompiledSchemaRef, toRef: CompiledSchemaRef)(using Ctx): Seq[BreakingChange] =
    if refCompatible(Dir.Response, fromRef, toRef) then Nil
    else Seq(BreakingChange(s"$loc body", "incompatible response body schema change"))

  /////// Params (query / header) ///////////////////////////////////////////////////////////////

  private def paramRequired(kind: CompiledParamType): Boolean =
    kind match
      case CompiledParamType.Required | CompiledParamType.ManyRequired => true
      case CompiledParamType.Optional | CompiledParamType.ManyOptional => false

  private def paramMany(kind: CompiledParamType): Boolean =
    kind match
      case CompiledParamType.ManyRequired | CompiledParamType.ManyOptional => true
      case CompiledParamType.Required | CompiledParamType.Optional         => false

  private def diffParams(dir: Dir, loc: String, from: ArraySeq[RawCompiledParam], to: ArraySeq[RawCompiledParam])(using Ctx): Seq[BreakingChange] = {
    val fromByName: Map[String, RawCompiledParam] = from.map(p => p.name -> p).toMap
    val toByName: Map[String, RawCompiledParam] = to.map(p => p.name -> p).toMap
    val added: Seq[BreakingChange] = to.filterNot(p => fromByName.contains(p.name)).flatMap(p => addedParam(dir, loc, p))
    val removed: Seq[BreakingChange] = from.filterNot(p => toByName.contains(p.name)).flatMap(p => removedParam(dir, loc, p))
    val both: Seq[BreakingChange] = from.flatMap(fp => toByName.get(fp.name).toList.flatMap(tp => bothParam(dir, loc, fp, tp)))
    added ++ removed ++ both
  }

  private def addedParam(dir: Dir, loc: String, param: RawCompiledParam): Seq[BreakingChange] =
    dir match
      case Dir.Request  => if paramRequired(param.kind) then Seq(BreakingChange(s"$loc '${param.name}'", "added a required param")) else Nil
      case Dir.Response => Nil // a new response value is ignorable by the old client

  private def removedParam(dir: Dir, loc: String, param: RawCompiledParam): Seq[BreakingChange] =
    dir match
      case Dir.Request  => Nil // the server ignores a param the old client still sends
      case Dir.Response => if paramRequired(param.kind) then Seq(BreakingChange(s"$loc '${param.name}'", "removed a required response value")) else Nil

  private def bothParam(dir: Dir, loc: String, from: RawCompiledParam, to: RawCompiledParam)(using Ctx): Seq[BreakingChange] = {
    val where: String = s"$loc '${from.name}'"
    val cardinalityChange: Seq[BreakingChange] =
      if paramMany(from.kind) != paramMany(to.kind) then Seq(BreakingChange(where, "param cardinality changed (single <-> many)")) else Nil
    val requiredChange: Seq[BreakingChange] =
      dir match
        case Dir.Request  => if !paramRequired(from.kind) && paramRequired(to.kind) then Seq(BreakingChange(where, "param became required")) else Nil
        case Dir.Response => if paramRequired(from.kind) && !paramRequired(to.kind) then Seq(BreakingChange(where, "response value became optional")) else Nil
    val schemaChange: Seq[BreakingChange] =
      if refCompatible(dir, from.schema, to.schema) then Nil else Seq(BreakingChange(where, "incompatible param schema change"))
    cardinalityChange ++ requiredChange ++ schemaChange
  }

}
