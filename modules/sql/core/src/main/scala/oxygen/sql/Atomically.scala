package oxygen.sql

import oxygen.predef.zio.*
import oxygen.sql.query.{Query, QueryContext}
import zio.Exit

type Atomically = oxygen.storage.Atomically
object Atomically {

  val atomically: ZIOAspectAtLeastR[Atomically] = oxygen.storage.Atomically.atomically

  /**
    * TLDR : Transaction(commit/rollback) -> Savepoint(commit/rollback) -> Savepoint(commit/rollback) -> ...
    *
    * When executed outside any other `atomically` blocks,
    * this will execute the provided effect within a transaction, and if it succeeds, commit, and if it fails, roll back.
    *
    * When executed within another `atomically` block or blocks (multiple levels deep),
    * this will execute the provided effect within a savepoint, and if it success, releases the savepoint, and if it fails, roll back the savepoint.
    */
  final case class LiveDB(db: Database) extends Atomically {

    override def atomicallyScoped: URIO[Scope, Unit] =
      db.getAtomicChild.orDie
        .withFinalizerExit {
          case (con, Exit.Success(_)) => queries.commit(con.connectionType).execute().unit.orDie
          case (con, Exit.Failure(_)) => queries.rollback(con.connectionType).execute().unit.orDie
        }
        .tap { con => queries.begin(con.connectionType).execute().unit.orDie }
        .unit
        .uninterruptible
        .usingDb(db)

    override def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.scoped { atomicallyScoped *> effect }

  }
  object LiveDB {

    val atomically: ZIOAspectAtLeastR[Database] =
      new ZIOAspectAtLeastR.Impl[Database] {
        override def apply[R <: Database, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
          (effect @@ Atomically.atomically).provideSomeLayer[R](LiveDB.layer)
      }

    val layer: URLayer[Database, Atomically] =
      ZLayer.fromFunction { LiveDB.apply }

  }

  /**
    * TLDR : Transaction(always rollback) -> Savepoint(commit/rollback) -> Savepoint(commit/rollback) -> ...
    *
    * When executed outside any other `atomically` blocks,
    * this will execute the provided effect within a transaction, and no matter the result, will roll back the transaction.
    * Useful for testing.
    *
    * When executed within another `atomically` block or blocks (multiple levels deep),
    * this will execute the provided effect within a savepoint, and if it success, releases the savepoint, and if it fails, roll back the savepoint.
    */
  final case class RollbackDB(db: Database) extends Atomically {

    override def atomicallyScoped: URIO[Scope, Unit] =
      db.getAtomicChild.orDie
        .withFinalizerExit {
          case (con, Exit.Success(_)) => queries.commitSavepointRollbackTransaction(con.connectionType).execute().unit.orDie
          case (con, Exit.Failure(_)) => queries.rollback(con.connectionType).execute().unit.orDie
        }
        .tap { con => queries.begin(con.connectionType).execute().unit.orDie }
        .unit
        .uninterruptible
        .usingDb(db)

    override def atomically[R, E, A](effect: ZIO[R, E, A]): ZIO[R, E, A] =
      ZIO.scoped { atomicallyScoped *> effect }

  }
  object RollbackDB {

    val atomically: ZIOAspectAtLeastR[Database] =
      new ZIOAspectAtLeastR.Impl[Database] {
        override def apply[R <: Database, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
          (effect @@ Atomically.atomically).provideSomeLayer[R](RollbackDB.layer)
      }

    val layer: URLayer[Database, Atomically] =
      ZLayer.fromFunction { RollbackDB.apply }

  }

  private object queries {

    def begin(ct: Database.ConnectionState.ConnectionType.Transactional): Query = ct match
      case Database.ConnectionState.ConnectionType.Transaction   => raw.begin
      case sp: Database.ConnectionState.ConnectionType.Savepoint => raw.savepoint(sp.ref)

    def commit(ct: Database.ConnectionState.ConnectionType.Transactional): Query = ct match
      case Database.ConnectionState.ConnectionType.Transaction   => raw.commit
      case sp: Database.ConnectionState.ConnectionType.Savepoint => raw.releaseSavepoint(sp.ref)

    def rollback(ct: Database.ConnectionState.ConnectionType.Transactional): Query = ct match
      case Database.ConnectionState.ConnectionType.Transaction   => raw.rollback
      case sp: Database.ConnectionState.ConnectionType.Savepoint => raw.rollbackSavepoint(sp.ref)

    def commitSavepointRollbackTransaction(ct: Database.ConnectionState.ConnectionType.Transactional): Query = ct match
      case Database.ConnectionState.ConnectionType.Transaction   => raw.rollback
      case sp: Database.ConnectionState.ConnectionType.Savepoint => raw.releaseSavepoint(sp.ref)

    private object raw {

      val begin: Query = Query.simple("BEGIN", QueryContext.QueryType.Transaction)("BEGIN")
      val commit: Query = Query.simple("COMMIT", QueryContext.QueryType.Transaction)("COMMIT")
      val rollback: Query = Query.simple("ROLLBACK", QueryContext.QueryType.Transaction)("ROLLBACK")

      def savepoint(ref: String): Query = Query.simple("SAVEPOINT", QueryContext.QueryType.Transaction)(s"SAVEPOINT $ref")
      def releaseSavepoint(ref: String): Query = Query.simple("RELEASE SAVEPOINT", QueryContext.QueryType.Transaction)(s"RELEASE SAVEPOINT $ref")
      def rollbackSavepoint(ref: String): Query = Query.simple("ROLLBACK TO SAVEPOINT", QueryContext.QueryType.Transaction)(s"ROLLBACK TO SAVEPOINT $ref")

    }

  }

}
