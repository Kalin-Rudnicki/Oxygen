package oxygen.http

import oxygen.http.schema.compat.*
import oxygen.http.schema.compiled.*
import oxygen.http.server.DeriveEndpoints
import oxygen.json.JsonCodec
import oxygen.predef.core.*
import oxygen.predef.test.*
import oxygen.schema.compiled.CompiledSchemaRef
import scala.annotation.experimental
import scala.collection.immutable.ArraySeq as IArraySeq
import zio.http.Method

/**
  * Tests for `oxygen.http.schema.compat.HttpCompat` — the HTTP-schema compatibility diff (OFF-369).
  *
  * Fixtures are built from a real compiled [[RawCompiledApiSpec]] ([[CompatFixtureApi]]); the `to` side
  * of each case is a structural mutation of the base endpoints over the '''shared''' schema bundle, so
  * every rule from the ticket is asserted directly.
  */
@experimental
object HttpCompatSpec extends OxygenSpecDefault {

  private val base: RawCompiledApiSpec =
    CompiledApiSpec.compileWithoutLineNos(summon[DeriveEndpoints[CompatFixtureApi]].endpoints.toArraySeq.map(_.schema))

  private val endpoints: ArraySeq[RawCompiledEndpoint] = base.apis.flatMap(_.endpoints)
  private val small: RawCompiledEndpoint = endpoints.find(_.name == "takesSmall").get
  private val big: RawCompiledEndpoint = endpoints.find(_.name == "takesBig").get

  private val smallBodyRef: CompiledSchemaRef =
    small.request.body match {
      case RawCompiledRequestBody.Single(_, _, ref) => ref
      case RawCompiledRequestBody.Empty             => sys.error("fixture defect: takesSmall has no request body")
    }

  /** Build a single-api spec from the given endpoints, reusing the full shared bundle. */
  private def spec(es: RawCompiledEndpoint*): RawCompiledApiSpec =
    RawCompiledApiSpec(IArraySeq(RawCompiledApi(None, None, IArraySeq.from(es))), base.schemas)

  private def withMethod(ep: RawCompiledEndpoint, method: Method): RawCompiledEndpoint =
    ep.copy(request = ep.request.copy(method = Some(method)))

  private def addQueryParam(ep: RawCompiledEndpoint, name: String, kind: CompiledParamType): RawCompiledEndpoint =
    ep.copy(request = ep.request.copy(queryParams = ep.request.queryParams :+ RawCompiledParam(name, None, kind, smallBodyRef)))

  private def withRequestBody(ep: RawCompiledEndpoint, body: RawCompiledRequestBody): RawCompiledEndpoint =
    ep.copy(request = ep.request.copy(body = body))

  private def withSuccessBody(ep: RawCompiledEndpoint, body: RawCompiledResponseBody): RawCompiledEndpoint =
    ep.copy(successResponse = ep.successResponse.copy(body = body))

  override def testSpec: TestSpec =
    suite("HttpCompatSpec")(
      test("a spec compared to itself is exact-equal") {
        val res = HttpCompat.compare(spec(small, big), spec(small, big))
        assertTrue(res.isCompatible, res.compatibility == HttpCompatibility.ExactEqual, res.breaks.isEmpty, res.changes.isEmpty)
      },
      test("removing an endpoint is breaking") {
        val res = HttpCompat.compare(spec(small, big), spec(small))
        assertTrue(
          !res.isCompatible,
          res.compatibility == HttpCompatibility.Breaking,
          res.breaks.exists { case b: HttpBreak.EndpointRemoved => b.name == "takesBig"; case _ => false },
        )
      },
      test("adding an endpoint is backwards-compatible") {
        val res = HttpCompat.compare(spec(small), spec(small, big))
        assertTrue(res.isCompatible, res.compatibility == HttpCompatibility.BackwardsCompatible)
      },
      test("changing the HTTP method is breaking") {
        val res = HttpCompat.compare(spec(small), spec(withMethod(small, Method.GET)))
        assertTrue(
          !res.isCompatible,
          res.breaks.exists { case _: HttpBreak.MethodChanged => true; case _ => false },
        )
      },
      test("adding an optional query param is backwards-compatible") {
        val res = HttpCompat.compare(spec(small), spec(addQueryParam(small, "extra", CompiledParamType.Optional)))
        assertTrue(res.isCompatible, res.compatibility == HttpCompatibility.BackwardsCompatible)
      },
      test("adding a required query param is breaking") {
        val res = HttpCompat.compare(spec(small), spec(addQueryParam(small, "extra", CompiledParamType.Required)))
        assertTrue(
          !res.isCompatible,
          res.breaks.exists { case b: HttpBreak.RequiredParamAdded => b.name == "extra"; case _ => false },
        )
      },
      test("narrowing the request body (extra required field) is breaking") {
        // takesSmall's body goes Small -> Big; as an input, requiring an extra field is breaking.
        val narrowed = withRequestBody(small, big.request.body)
        val res = HttpCompat.compare(spec(small), spec(narrowed))
        assertTrue(
          !res.isCompatible,
          res.breaks.exists {
            case b: HttpBreak.IncompatibleType => b.variance == HttpVariance.Contravariant
            case _                             => false
          },
        )
      },
      test("widening a sum response (extra case) is backwards-compatible") {
        // takesSmall's success body goes Shape -> ShapeWide; as an output, an extra case is compatible.
        val widened = withSuccessBody(small, big.successResponse.body)
        val res = HttpCompat.compare(spec(small), spec(widened))
        assertTrue(res.isCompatible, res.compatibility == HttpCompatibility.BackwardsCompatible)
      },
      test("the compiled spec round-trips through JSON (the diffed artifact is serializable)") {
        val codec: JsonCodec[RawCompiledApiSpec] = JsonCodec[RawCompiledApiSpec]
        val json: String = codec.encoder.encodeJsonStringCompact(base)
        val decoded: Option[RawCompiledApiSpec] = codec.decoder.decodeJsonString(json).toOption
        assertTrue(decoded == Some(base))
      },
      test("comparison is line-number stable (normalized both sides)") {
        // `compare` applies `withoutLineNos`; comparing the raw spec to its normalized form is exact-equal.
        val res = HttpCompat.compare(base, base.withoutLineNos)
        assertTrue(res.compatibility == HttpCompatibility.ExactEqual)
      },
    )

}
