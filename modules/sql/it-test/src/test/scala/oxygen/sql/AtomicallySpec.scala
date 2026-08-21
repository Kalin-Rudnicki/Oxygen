package oxygen.sql

import oxygen.predef.test.*
import zio.*

object AtomicallySpec extends OxygenSpec[Database] {

  private val transactionless: Database.ConnectionState.ConnectionType.Any = Database.ConnectionState.ConnectionType.Transactionless
  private val transaction: Database.ConnectionState.ConnectionType.Any = Database.ConnectionState.ConnectionType.Transaction

  private val observeConnectionType: URIO[Database, Database.ConnectionState.ConnectionType.Any] =
    ZIO.serviceWithZIO[Database](_.currentConnectionType)

  private def isSavepoint(ct: Database.ConnectionState.ConnectionType.Any): Boolean =
    ct match {
      case _: Database.ConnectionState.ConnectionType.Savepoint => true
      case _                                                    => false
    }

  override def testSpec: TestSpec =
    suite("AtomicallySpec")(
      test("baseline : outside any block, the connection is transactionless") {
        for {
          ct <- observeConnectionType
        } yield assertTrue(ct == transactionless)
      },
      test("ensureAtomic outermost opens a real transaction") {
        for {
          ct <- observeConnectionType @@ Atomically.LiveDB.ensureAtomic
        } yield assertTrue(ct == transaction)
      },
      test("ensureAtomic nested inside a transaction does NOT open a savepoint") {
        for {
          ct <- (observeConnectionType @@ Atomically.LiveDB.ensureAtomic) @@ Atomically.LiveDB.atomically
        } yield assertTrue(ct == transaction)
      },
      test("ensureAtomic nested inside a savepoint stays that savepoint (still a no-op)") {
        for {
          ct <- ((observeConnectionType @@ Atomically.LiveDB.ensureAtomic) @@ Atomically.LiveDB.atomically) @@ Atomically.LiveDB.atomically
        } yield assertTrue(isSavepoint(ct))
      },
      test("contrast : nested plain atomically DOES open a savepoint") {
        for {
          ct <- (observeConnectionType @@ Atomically.LiveDB.atomically) @@ Atomically.LiveDB.atomically
        } yield assertTrue(isSavepoint(ct))
      },
    ) @@ TestAspect.sequential

  override def layerProvider: LayerProvider[R] =
    LayerProvider.provideShared[Env](
      Helpers.testContainerLayer,
      Helpers.databaseLayer,
    )

}
