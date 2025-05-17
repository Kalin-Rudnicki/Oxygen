package oxygen.sql

import java.util.UUID
import oxygen.predef.zio.*
import oxygen.sql.error.*
import oxygen.sql.query.*
import oxygen.sql.schema.*
import zio.{Semaphore, ZPool}
import zio.stream.*

final class Database(config: DbConfig, ref: FiberRef[Database.ConnectionState]) {

  def use[R, E, A](effect: ZIO[R & Database, E, A]): ZIO[R, E, A] =
    effect.provideSomeEnvironment(_.add(this))

  def use[R, E, A](effect: ZStream[R & Database, E, A]): ZStream[R, E, A] =
    effect.provideSomeEnvironment(_.add(this))

  // TODO (KR) : Improve the way atomicity is achieved.
  //           : This is pretty good, but might be susceptible to connections leaking when forked.
  //           : Something like FiberRef[Option[Ref[CurrentTransaction]]] might be the move.
  //           : Or maybe, keeping track of transaction/savepoints in the db state is not necessary.
  //           : This might be prevented by the mutex, but should be tested.

  private[sql] def getConnection: ZIO[Scope, ConnectionError, Connection] =
    ref.getWith(_.getConnection)

  private[sql] def getAtomicChild: ZIO[Scope, ConnectionError, Database.ConnectionState.SingleConnection] =
    ref.getWith(_.getAtomicChild).tap { ref.locallyScoped(_) }

  private[sql] val logQuery: QueryContext => UIO[Unit] =
    config.logging match {
      case DbConfig.Logging(queryLogLevel, true)  => ctx => ZIO.logAtLevel(queryLogLevel)(s"Executing query: ${ctx.queryName}\n\n${ctx.sql}", Cause.Empty)
      case DbConfig.Logging(queryLogLevel, false) => ctx => ZIO.logAtLevel(queryLogLevel)(s"Executing query: ${ctx.queryName}", Cause.Empty)
    }

}
object Database {

  def make(config: DbConfig): URIO[Scope, Database] =
    for {
      pool <- ZPool.make(Connection.acquire(config), config.pool.minConnections to config.pool.maxConnections, config.pool.duration)
      ref <- FiberRef.make[ConnectionState](ConnectionState.Pool(pool))
    } yield Database(config, ref)

  val layer: URLayer[DbConfig, Database] =
    ZLayer.scoped { ZIO.serviceWithZIO[DbConfig](make(_)) }

  val healthCheck: RIO[Database, Unit] =
    QueryO[Int](QueryContext("HealthCheck", "SELECT 1", QueryContext.QueryType.Select), RowRepr.int.decoder)
      .execute()
      .single
      .flatMap {
        case 1   => ZIO.unit
        case res => ZIO.fail(new RuntimeException(s"Healthcheck returned something other than `1` : $res"))
      }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Connection State
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[sql] sealed trait ConnectionState {

    def getConnection: ZIO[Scope, ConnectionError, Connection]
    def getAtomicChild: ZIO[Scope, ConnectionError, ConnectionState.SingleConnection]

  }
  private[sql] object ConnectionState {

    sealed trait ConnectionType
    object ConnectionType {
      case object Transaction extends ConnectionType
      final case class Savepoint(id: UUID) extends ConnectionType {
        val ref: String = s"sp_${id.toString.filterNot(_ == '-')}"
      }
    }

    sealed trait SingleConnection extends ConnectionState {

      val connection: Connection
      val mutex: Semaphore
      val connectionType: ConnectionType

      override final def getConnection: ZIO[Scope, ConnectionError, Connection] =
        mutex.withPermitScoped.as(connection)

      override final def getAtomicChild: ZIO[Scope, ConnectionError, SingleConnection] =
        for {
          _ <- mutex.withPermitScoped
          mutex2 <- Semaphore.make(1L)
          savepointId <- Random.nextUUID
        } yield InSavepoint(connection, mutex2, savepointId)

    }

    final case class Pool(pool: ZPool[ConnectionError, Connection]) extends ConnectionState {

      override def getConnection: ZIO[Scope, ConnectionError, Connection] =
        pool.get

      override def getAtomicChild: ZIO[Scope, ConnectionError, SingleConnection] =
        for {
          connection <- pool.get
          mutex <- Semaphore.make(1L)
          transactionId <- Random.nextUUID
        } yield InTransaction(connection, mutex, transactionId)

    }

    final case class InTransaction(connection: Connection, mutex: Semaphore, transactionId: UUID) extends ConnectionState.SingleConnection {
      override val connectionType: ConnectionType = ConnectionType.Transaction
    }

    final case class InSavepoint(connection: Connection, mutex: Semaphore, savepointId: UUID) extends ConnectionState.SingleConnection {
      override val connectionType: ConnectionType = ConnectionType.Savepoint(savepointId)
    }

  }

}
