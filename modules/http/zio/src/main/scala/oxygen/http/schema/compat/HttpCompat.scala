package oxygen.http.schema.compat

import oxygen.http.schema.compiled.*
import oxygen.predef.core.*
import oxygen.schema.compat.*
import oxygen.schema.compiled.{CompiledSchemaRef, FullCompiledJsonSchema, FullCompiledSchemas, RawCompiledSchemas}

/**
  * Backwards-compatibility diff for the compiled HTTP API spec — the HTTP-layer analogue of
  * `oxygen.schema.compat` (which diffs type schemas). Given two
  * [[oxygen.http.schema.compiled.RawCompiledApiSpec]]s it returns an [[HttpComparisonResult]]:
  * the structured set of breaking / backwards-compatible changes evolving `from` → `to`.
  *
  * '''Reuse, not reinvention.''' The HTTP-structural rules (endpoints / methods / paths / param
  * cardinality / body presence / body kind) live here; every ''type-reference'' transition is
  * delegated to the type-level `Compared.compareRoot` and its [[oxygen.schema.compat.ComparisonResult]]
  * is interpreted directionally by [[HttpVariance]] (see [[isBreaking]]).
  *
  * '''Variance is the axis.''' Request inputs (path / query / header / request-body) are
  * '''contravariant''' — compatible evolution ''widens'' what is accepted. Responses
  * (response-body / response-header) are '''covariant''' — compatible evolution ''narrows'' what is
  * produced.
  *
  * '''Scope (per OFF-369 / OXY-29).''' Spec-only: this compares two already-compiled specs and returns
  * a result. Persisting a spec to a file and failing CI on a breaking result is OXY-38's job, not this.
  * Status codes are treated as '''descriptive, non-breaking''' (matching the compiled model's own
  * "de-emphasized" stance).
  */
object HttpCompat {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      API
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** The `Compared` program (composes with the type-level machinery); `eval` it with two schema bundles. */
  def compareCompiled(from: RawCompiledApiSpec, to: RawCompiledApiSpec): Compared[HttpComparisonResult] =
    compareSpec(from, to)

  /**
    * Compare two compiled specs, normalizing line numbers first (stable / diff-friendly). This is the
    * one-shot entry point most callers want.
    */
  def compare(from: RawCompiledApiSpec, to: RawCompiledApiSpec): HttpComparisonResult = {
    val f: RawCompiledApiSpec = from.withoutLineNos
    val t: RawCompiledApiSpec = to.withoutLineNos
    // The type-level `Compared` resolves BOTH refs against a single bundle (see `Comparison.comparePostCheck`,
    // and `CompatSpec` which evals with `full, full`). So diff the two specs against ONE merged bundle
    // (deduped by ref); every `from`/`to` type ref must be resolvable there. Same-name-but-changed types
    // collide by ref — a known limit of the shared machinery; version-distinct type names diff exactly.
    val schemas: FullCompiledSchemas = mergedSchemas(f, t)
    compareSpec(f, t).eval(schemas, schemas).result
  }

  private def mergedSchemas(from: RawCompiledApiSpec, to: RawCompiledApiSpec): FullCompiledSchemas =
    RawCompiledSchemas(
      plain = (from.schemas.plain ++ to.schemas.plain).distinctBy(_.ref),
      json = (from.schemas.json ++ to.schemas.json).distinctBy(_.ref),
    ).toFullCompiledSchemas

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Structural — apis / endpoints
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def compareSpec(from: RawCompiledApiSpec, to: RawCompiledApiSpec): Compared[HttpComparisonResult] =
    AddedRemovedBoth.Many.fromSeqs(from.apis, to.apis, _.name.getOrElse("")) { (f, t) => compareApi(HttpLocation.root, f, t) }.map { many =>
      val added: List[HttpChange] = many.added.map(a => HttpChange(HttpLocation.root, s"api '${a.name.getOrElse("<root>")}' was added")).toList
      val removed: List[HttpBreak] = many.removed.map(r => HttpBreak.ApiRemoved(HttpLocation.root, r.name.getOrElse("<root>"))).toList
      HttpComparisonResult(removed, added) ++ HttpComparisonResult.combineAll(many.both)
    }

  private def compareApi(parent: HttpLocation, from: RawCompiledApi, to: RawCompiledApi): Compared[HttpComparisonResult] = {
    val loc: HttpLocation = parent / from.name.getOrElse("<root>")
    AddedRemovedBoth.Many.fromSeqs(from.endpoints, to.endpoints, _.name) { (f, t) => compareEndpoint(loc, f, t) }.map { many =>
      val added: List[HttpChange] = many.added.map(a => HttpChange(loc, s"endpoint '${a.name}' was added")).toList
      val removed: List[HttpBreak] = many.removed.map(r => HttpBreak.EndpointRemoved(loc, r.name)).toList
      HttpComparisonResult(removed, added) ++ HttpComparisonResult.combineAll(many.both)
    }
  }

  private def compareEndpoint(parent: HttpLocation, from: RawCompiledEndpoint, to: RawCompiledEndpoint): Compared[HttpComparisonResult] = {
    val loc: HttpLocation = parent / from.name
    for {
      request <- compareRequest(loc / "request", from.request, to.request)
      success <- compareResponse(loc / "successResponse", from.successResponse, to.successResponse)
      error <- compareResponse(loc / "errorResponse", from.errorResponse, to.errorResponse)
    } yield request ++ success ++ error
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Request
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def compareRequest(loc: HttpLocation, from: RawCompiledRequest, to: RawCompiledRequest): Compared[HttpComparisonResult] = {
    val method: HttpComparisonResult =
      if from.method == to.method then HttpComparisonResult.empty
      else HttpComparisonResult.break(HttpBreak.MethodChanged(loc, from.method, to.method))
    for {
      paths <- comparePaths(loc / "path", from.paths, to.paths)
      query <- compareParams(loc / "query", from.queryParams, to.queryParams, HttpVariance.Contravariant)
      headers <- compareParams(loc / "header", from.headers, to.headers, HttpVariance.Contravariant)
      body <- compareRequestBody(loc / "body", from.body, to.body)
    } yield method ++ paths ++ query ++ headers ++ body
  }

  private def pathShape(p: RawCompiledPaths): String = {
    val segs: String =
      p.segments.iterator.map {
        case RawCompiledPathSegment.Const(path)       => path
        case RawCompiledPathSegment.Param(name, _, _) => s"{$name}"
      }.mkString("/", "/", "")
    p.rest.fold(segs)(r => s"$segs/{${r.name}...}")
  }

  private def pathParams(paths: NonEmptyList[RawCompiledPaths]): Map[String, CompiledSchemaRef] =
    paths.toList.iterator.flatMap { p =>
      val singles: Iterator[(String, CompiledSchemaRef)] = p.segments.iterator.collect { case RawCompiledPathSegment.Param(name, _, ref) => name -> ref }
      val rest: Iterator[(String, CompiledSchemaRef)] = p.rest.iterator.map(r => r.name -> r.schema)
      singles ++ rest
    }.toMap

  private def comparePaths(loc: HttpLocation, from: NonEmptyList[RawCompiledPaths], to: NonEmptyList[RawCompiledPaths]): Compared[HttpComparisonResult] = {
    val fromShapes: Set[String] = from.toList.iterator.map(pathShape).toSet
    val toShapes: Set[String] = to.toList.iterator.map(pathShape).toSet
    val removed: List[HttpBreak] = (fromShapes &~ toShapes).toList.sorted.map(s => HttpBreak.PathRemoved(loc, s))
    val added: List[HttpChange] = (toShapes &~ fromShapes).toList.sorted.map(s => HttpChange(loc, s"path shape '$s' was added"))
    val structural: HttpComparisonResult = HttpComparisonResult(removed, added)
    compareSharedRefs(loc, pathParams(from), pathParams(to), HttpVariance.Contravariant, "path param").map(structural ++ _)
  }

  private def compareRequestBody(loc: HttpLocation, from: RawCompiledRequestBody, to: RawCompiledRequestBody): Compared[HttpComparisonResult] =
    (from, to) match {
      case (RawCompiledRequestBody.Empty, RawCompiledRequestBody.Empty)         => Compared.done(HttpComparisonResult.empty)
      case (RawCompiledRequestBody.Empty, _: RawCompiledRequestBody.Single)     => Compared.done(HttpComparisonResult.break(HttpBreak.RequestBodyAdded(loc)))
      case (_: RawCompiledRequestBody.Single, RawCompiledRequestBody.Empty)     => Compared.done(HttpComparisonResult.change(HttpChange(loc, "request body was removed (input relaxed)")))
      case (f: RawCompiledRequestBody.Single, t: RawCompiledRequestBody.Single) => compareRef(loc, "request body", f.schema, t.schema, HttpVariance.Contravariant)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Response
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def compareResponse(loc: HttpLocation, from: RawCompiledResponse, to: RawCompiledResponse): Compared[HttpComparisonResult] =
    // status + caseStatuses are deliberately descriptive-only (see model docs) — not compared.
    for {
      headers <- compareParams(loc / "header", from.headers, to.headers, HttpVariance.Covariant)
      body <- compareResponseBody(loc / "body", from.body, to.body)
    } yield headers ++ body

  private def responseBodyKind(b: RawCompiledResponseBody): String =
    b match
      case RawCompiledResponseBody.Empty               => "empty"
      case _: RawCompiledResponseBody.Single           => "single"
      case _: RawCompiledResponseBody.ServerSentEvents => "server-sent-events"
      case _: RawCompiledResponseBody.LineStream       => "line-stream"

  private def compareResponseBody(loc: HttpLocation, from: RawCompiledResponseBody, to: RawCompiledResponseBody): Compared[HttpComparisonResult] =
    (from, to) match {
      case (RawCompiledResponseBody.Empty, RawCompiledResponseBody.Empty)                             => Compared.done(HttpComparisonResult.empty)
      case (RawCompiledResponseBody.Empty, _)                                                         => Compared.done(HttpComparisonResult.change(HttpChange(loc, "response body was added")))
      case (_, RawCompiledResponseBody.Empty)                                                         => Compared.done(HttpComparisonResult.break(HttpBreak.ResponseBodyRemoved(loc)))
      case (f: RawCompiledResponseBody.Single, t: RawCompiledResponseBody.Single)                     => compareRef(loc, "response body", f.schema, t.schema, HttpVariance.Covariant)
      case (f: RawCompiledResponseBody.ServerSentEvents, t: RawCompiledResponseBody.ServerSentEvents) => compareRef(loc, "response body", f.schema, t.schema, HttpVariance.Covariant)
      case (f: RawCompiledResponseBody.LineStream, t: RawCompiledResponseBody.LineStream)             => compareRef(loc, "response body", f.schema, t.schema, HttpVariance.Covariant)
      case (f, t) => Compared.done(HttpComparisonResult.break(HttpBreak.BodyKindChanged(loc, responseBodyKind(f), responseBodyKind(t))))
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Params (query / header / path)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def isRequired(kind: CompiledParamType): Boolean =
    kind match
      case CompiledParamType.Required | CompiledParamType.ManyRequired => true
      case CompiledParamType.Optional | CompiledParamType.ManyOptional => false

  private def compareParams(loc: HttpLocation, from: Seq[RawCompiledParam], to: Seq[RawCompiledParam], variance: HttpVariance): Compared[HttpComparisonResult] =
    AddedRemovedBoth.Many.fromSeqs(from, to, _.name) { (f, t) => compareParamBoth(loc, f, t, variance) }.map { many =>
      val addRem: List[HttpComparisonResult] = many.added.map(paramAdded(loc, _, variance)).toList ++ many.removed.map(paramRemoved(loc, _, variance)).toList
      HttpComparisonResult.combineAll(addRem) ++ HttpComparisonResult.combineAll(many.both)
    }

  private def paramAdded(loc: HttpLocation, p: RawCompiledParam, variance: HttpVariance): HttpComparisonResult =
    variance match {
      case HttpVariance.Contravariant =>
        if isRequired(p.kind) then HttpComparisonResult.break(HttpBreak.RequiredParamAdded(loc, p.name))
        else HttpComparisonResult.change(HttpChange(loc, s"optional param '${p.name}' was added"))
      case HttpVariance.Covariant =>
        HttpComparisonResult.change(HttpChange(loc, s"response param '${p.name}' was added"))
    }

  private def paramRemoved(loc: HttpLocation, p: RawCompiledParam, variance: HttpVariance): HttpComparisonResult =
    variance match {
      case HttpVariance.Contravariant =>
        HttpComparisonResult.change(HttpChange(loc, s"param '${p.name}' was removed (input relaxed)"))
      case HttpVariance.Covariant =>
        if isRequired(p.kind) then HttpComparisonResult.break(HttpBreak.RequiredResponseHeaderRemoved(loc, p.name))
        else HttpComparisonResult.change(HttpChange(loc, s"optional response param '${p.name}' was removed"))
    }

  private def compareParamBoth(loc: HttpLocation, from: RawCompiledParam, to: RawCompiledParam, variance: HttpVariance): Compared[HttpComparisonResult] = {
    val requiredness: HttpComparisonResult =
      (isRequired(from.kind), isRequired(to.kind), variance) match {
        case (false, true, HttpVariance.Contravariant) => HttpComparisonResult.break(HttpBreak.ParamRequirednessTightened(loc / from.name, from.name))
        case (true, false, HttpVariance.Covariant)     => HttpComparisonResult.break(HttpBreak.ParamRequirednessTightened(loc / from.name, from.name))
        case (a, b, _) if a != b                       => HttpComparisonResult.change(HttpChange(loc / from.name, s"param '${from.name}' requiredness relaxed"))
        case _                                         => HttpComparisonResult.empty
      }
    compareRef(loc / from.name, "param", from.schema, to.schema, variance).map(requiredness ++ _)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Type refs — delegate to the type-level machinery, interpret by variance
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def compareSharedRefs(loc: HttpLocation, from: Map[String, CompiledSchemaRef], to: Map[String, CompiledSchemaRef], variance: HttpVariance, kind: String): Compared[HttpComparisonResult] = {
    val shared: List[String] = (from.keySet & to.keySet).toList.sorted
    Compared.traverse(shared) { name => compareRef(loc / name, kind, from(name), to(name), variance) }.map(HttpComparisonResult.combineAll)
  }

  private def compareRef(loc: HttpLocation, kind: String, from: CompiledSchemaRef, to: CompiledSchemaRef, variance: HttpVariance): Compared[HttpComparisonResult] =
    Compared.compareRoot(from, to).map { concrete =>
      val pruned: ComparisonResult = concrete.pruned
      if isBreaking(pruned, variance) then HttpComparisonResult.break(HttpBreak.IncompatibleType(loc, variance, pruned))
      else if pruned.isDifferent then HttpComparisonResult.change(HttpChange(loc, s"$kind type changed (compatible)"))
      else HttpComparisonResult.empty
    }

  /** A product field / sum case is '''required''' when it has no fallback for a missing value. */
  private def fieldRequired(f: FullCompiledJsonSchema.ProductField): Boolean = f.onMissing.isEmpty

  private def requirednessBreak(f: ComparisonResult.FieldComparison, variance: HttpVariance): Boolean =
    f.onMissing match {
      case FromToValues.Same(_)                               => false
      case FromToValues.Different(fromOnMissing, toOnMissing) =>
        val wasRequired: Boolean = fromOnMissing.isEmpty
        val isRequiredNow: Boolean = toOnMissing.isEmpty
        variance match
          case HttpVariance.Contravariant => !wasRequired && isRequiredNow // input field became required
          case HttpVariance.Covariant     => wasRequired && !isRequiredNow // output field became optional
    }

  /**
    * Interpret a type-level [[oxygen.schema.compat.ComparisonResult]] (already `.pruned`) as a
    * breaking-or-not verdict in the given [[HttpVariance]] position. Additions to a closed set
    * (enum value / sum case / optional product field) are treated as compatible; removals break the
    * '''input''' side; the two "more specific" verdicts are the directional core.
    *
    * NOTE (Open-Q-1, `🤖` proposal — NOT a `👤` hard table): this default table is grounded in the
    * ticket's stated examples. The subtle output-widening cases (e.g. adding a case to a response sum)
    * follow the ticket's "widen sum response = compatible" rule; see `http-feature-plan.md` Phase 4.
    */
  private def isBreaking(cmp: ComparisonResult, variance: HttpVariance): Boolean =
    cmp match {
      case _: ComparisonResult.ExactEqual         => false
      case _: ComparisonResult.RecursiveReference => false
      case _: ComparisonResult.FromIsMoreSpecific => variance == HttpVariance.Covariant // `to` is wider — breaks an output
      case _: ComparisonResult.ToIsMoreSpecific   => variance == HttpVariance.Contravariant // `to` is narrower — breaks an input
      case _: ComparisonResult.NotComparable      => true
      case c: ComparisonResult.Transformed        => isBreaking(c.underlying, variance)
      case c: ComparisonResult.FormattedText      => c.formats.nonEmpty || isBreaking(c.underlying, variance)
      case c: ComparisonResult.EncodedText        => c.encoding.isDifferent || isBreaking(c.underlying, variance)
      case c: ComparisonResult.BearerToken        => isBreaking(c.underlying, variance)
      case c: ComparisonResult.JsonString         => isBreaking(c.underlying, variance)
      case c: ComparisonResult.JsonArray          => isBreaking(c.underlying, variance)
      case c: ComparisonResult.JsonMap            => isBreaking(c.keyUnderlying, variance) || isBreaking(c.valueUnderlying, variance)
      case c: ComparisonResult.JsonNumber         => c.numberFormat.isDifferent
      case c: ComparisonResult.JsonAST            => c.jsonType.isDifferent
      case c: ComparisonResult.Enum               => enumBreaking(c, variance)
      case c: ComparisonResult.JsonProduct        => productBreaking(c, variance)
      case c: ComparisonResult.JsonSum            => sumBreaking(c, variance)
    }

  private def enumBreaking(c: ComparisonResult.Enum, variance: HttpVariance): Boolean = {
    val removedBreak: Boolean = c.values.removed.nonEmpty && variance == HttpVariance.Contravariant
    removedBreak || c.caseSensitive.isDifferent || c.exhaustive.isDifferent
  }

  private def sumBreaking(c: ComparisonResult.JsonSum, variance: HttpVariance): Boolean = {
    val removedCaseBreak: Boolean = c.cases.removed.nonEmpty && variance == HttpVariance.Contravariant
    val bothBreak: Boolean = c.cases.both.exists(kase => isBreaking(kase.typeComparison, variance))
    removedCaseBreak || c.discriminator.isDifferent || bothBreak
  }

  private def productBreaking(c: ComparisonResult.JsonProduct, variance: HttpVariance): Boolean = {
    val addedBreak: Boolean = variance == HttpVariance.Contravariant && c.fields.added.exists(fieldRequired) // new required input field
    val removedBreak: Boolean = variance == HttpVariance.Covariant && c.fields.removed.exists(fieldRequired) // dropped required output field
    val bothBreak: Boolean = c.fields.both.exists { f => isBreaking(f.typeComparison, variance) || requirednessBreak(f, variance) }
    addedBreak || removedBreak || bothBreak
  }

}
