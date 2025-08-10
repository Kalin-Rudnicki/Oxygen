package oxygen.sql

import oxygen.predef.test.*
import oxygen.test.container.*

object HealthCheckSpec extends OxygenSpec[TestContainerService] {

  override def testSpec: TestSpec =
    suite("HealthCheckSpec")(
      test("Is able to successfully execute health check") {
        Database.healthCheck.as(assertCompletes)
      }.provideSome[R](Helpers.databaseLayer),
    )

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[R](
      Helpers.testContainerLayer,
    )

}
