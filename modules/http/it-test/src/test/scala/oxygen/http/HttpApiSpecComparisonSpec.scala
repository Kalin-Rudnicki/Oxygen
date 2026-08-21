package oxygen.http

import oxygen.http.schema.compat.HttpApiSpecComparison
import oxygen.predef.test.*
import scala.annotation.experimental

/**
  * Validates the endpoint/API-layer compatibility classifier against the ticket's two tables -- and in
  * particular that **request and response compatibility are exact opposites** for the same structural
  * change (`compare` takes `from` = old, `to` = new).
  */
@experimental
object HttpApiSpecComparisonSpec extends OxygenSpecDefault {

  import ApiSpecCompatFixtures.*

  override def testSpec: TestSpec =
    suite("HttpApiSpecComparisonSpec")(
      test("identical specs are compatible with no breaking changes") {
        val result = HttpApiSpecComparison.compare(reqThingV1, reqThingV1)
        assertTrue(result.isCompatible, result.breakingChanges.isEmpty)
      },
      suite("request body -- client writes, server reads")(
        test("adding a required field is breaking") {
          assertTrue(!HttpApiSpecComparison.compare(reqThingV1, reqThingV2Req).isCompatible)
        },
        test("adding an optional field is compatible") {
          assertTrue(HttpApiSpecComparison.compare(reqThingV1, reqThingV2Opt).isCompatible)
        },
        test("removing a required field is compatible") {
          assertTrue(HttpApiSpecComparison.compare(reqThingV2Req, reqThingV1).isCompatible)
        },
        test("adding a sum-type case is compatible") {
          assertTrue(HttpApiSpecComparison.compare(reqShapeV2, reqShapeV3).isCompatible)
        },
        test("removing a sum-type case is breaking") {
          assertTrue(!HttpApiSpecComparison.compare(reqShapeV3, reqShapeV2).isCompatible)
        },
      ),
      suite("response body -- server writes, client reads (the mirror image)")(
        test("adding a required field is compatible") {
          assertTrue(HttpApiSpecComparison.compare(respThingV1, respThingV2Req).isCompatible)
        },
        test("removing a required field is breaking") {
          assertTrue(!HttpApiSpecComparison.compare(respThingV2Req, respThingV1).isCompatible)
        },
        test("adding a sum-type case is breaking") {
          assertTrue(!HttpApiSpecComparison.compare(respShapeV2, respShapeV3).isCompatible)
        },
        test("removing a sum-type case is compatible") {
          assertTrue(HttpApiSpecComparison.compare(respShapeV3, respShapeV2).isCompatible)
        },
      ),
      test("removing an endpoint is breaking") {
        val result = HttpApiSpecComparison.compare(multiV2, multiV1)
        assertTrue(!result.isCompatible, result.breakingChanges.exists(_.detail == "endpoint removed"))
      },
      test("adding an endpoint is compatible") {
        assertTrue(HttpApiSpecComparison.compare(multiV1, multiV2).isCompatible)
      },
    )

}
