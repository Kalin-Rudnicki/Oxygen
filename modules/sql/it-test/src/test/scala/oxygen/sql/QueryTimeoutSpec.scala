package oxygen.sql

import oxygen.predef.test.*
import oxygen.sql.query.{QueryContext, QueryO}
import oxygen.sql.schema.RowRepr
import zio.*

object QueryTimeoutSpec extends OxygenSpec[Database] {

  // `pg_sleep(n)` blocks for `n` seconds server-side; `::text` makes its `void` result decodable.
  private def sleep(seconds: Int): ZIO[Database, Throwable, String] =
    QueryO
      .simple[String]("PgSleep", QueryContext.QueryType.Select)(RowRepr.string.decoder)(s"SELECT pg_sleep($seconds)::text")
      .execute()
      .single

  override def testSpec: TestSpec =
    suite("QueryTimeoutSpec")(
      test("queryTimeout defaults to None") {
        assertTrue(DbConfig.Execution.default.queryTimeout.isEmpty)
      },
      test("withQueryTimeout aborts a query that exceeds the timeout") {
        for {
          exit <- (sleep(30) @@ Database.withQueryTimeout(1.second)).exit
        } yield assertTrue(exit.isFailure)
      },
      test("a query that completes within the timeout succeeds") {
        for {
          res <- sleep(1) @@ Database.withQueryTimeout(30.seconds)
        } yield assertTrue(res == "")
      },
    ) @@ TestAspect.withLiveClock @@ TestAspect.timeout(1.minute) @@ TestAspect.sequential

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
    )

}
