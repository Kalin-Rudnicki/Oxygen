package oxygen.sql

import java.util.UUID
import oxygen.predef.zio.*
import oxygen.sql.error.*
import oxygen.sql.query.*
import oxygen.sql.schema.*
import zio.*
import zio.stream.*

final class Database(
    private val executionConfig: DbConfig.Execution,
    private val logConfigRef: FiberRef[DbConfig.Logging],
    connectionStateRef: FiberRef[Database.ConnectionState],
) {

  def use[R, E, A](effect: ZIO[R & Database, E, A]): ZIO[R, E, A] =
    effect.provideSomeEnvironment(_.add(this))

  def use[R, E, A](effect: ZStream[R & Database, E, A]): ZStream[R, E, A] =
    effect.provideSomeEnvironment(_.add(this))

  // TODO (KR) : Improve the way atomicity is achieved.
  //           : This is pretty good, but might be susceptible to connections leaking when forked.
  //           : Something like FiberRef[Option[Ref[CurrentTransaction]]] might be the move.
  //           : Or maybe, keeping track of transaction/savepoints in the db state is not necessary.
  //           : This might be prevented by the mutex, but should be tested.

  private[sql] def getConnection: ZIO[Scope, ConnectionError, Connection] = connectionStateRef.getWith(_.getConnection)
  private[sql] def getConnectionAndType: ZIO[Scope, ConnectionError, (Connection, Database.ConnectionState.ConnectionType.Any)] = connectionStateRef.getWith(_.getConnectionAndType)

  private[sql] def getAtomicChild: ZIO[Scope, ConnectionError, Database.ConnectionState.SingleConnection] =
    connectionStateRef.getWith(_.getAtomicChild).tap { connectionStateRef.locallyScoped(_) }

  private[sql] def logQuery(ctx: QueryContext)(using Trace): UIO[Unit] =
    logConfigRef.get.flatMap {
      case DbConfig.Logging(queryLogLevel, true)  => ZIO.logAtLevel(queryLogLevel)(s"Executing query: ${ctx.queryContextHeader}\n\n${ctx.sql}", Cause.Empty)
      case DbConfig.Logging(queryLogLevel, false) => ZIO.logAtLevel(queryLogLevel)(s"Executing query: ${ctx.queryContextHeader}", Cause.Empty)
    }

}
object Database {

  def make(config: DbConfig): URIO[Scope, Database] =
    for {
      driver <- Driver.makePSQL
      connect = driver.getConnection(config.target, config.credentials)
      pool <- ConnectionPool.makeZPool(connect, config.pool)
      logConfigRef <- FiberRef.make(config.logging)
      connectionStateRef <- FiberRef.make[ConnectionState](ConnectionState.Pool(pool))
    } yield Database(config.execution, logConfigRef, connectionStateRef)

  val baseLayer: URLayer[ConnectionPool & DbConfig.Logging & DbConfig.Execution, Database] =
    ZLayer.scoped {
      for {
        pool <- ZIO.service[ConnectionPool]
        logConfig <- ZIO.service[DbConfig.Logging]
        executionConfig <- ZIO.service[DbConfig.Execution]
        logConfigRef <- FiberRef.make(logConfig)
        connectionStateRef <- FiberRef.make[ConnectionState](ConnectionState.Pool(pool))
      } yield Database(executionConfig, logConfigRef, connectionStateRef)
    }

  val layer: URLayer[DbConfig, Database] =
    ZLayer.makeSome[DbConfig, Database](
      ZLayer.service[DbConfig].project(_.target),
      ZLayer.service[DbConfig].project(_.credentials),
      ZLayer.service[DbConfig].project(_.pool),
      ZLayer.service[DbConfig].project(_.logging),
      ZLayer.service[DbConfig].project(_.execution),
      Driver.psql,
      Driver.GetConnection.layer,
      ConnectionPool.zPool,
      Database.baseLayer,
    )

  val healthCheck: RIO[Database, Unit] =
    QueryO
      .simple[Int]("HealthCheck", QueryContext.QueryType.Select)(RowRepr.int.decoder)("SELECT 1")
      .execute()
      .single
      .flatMap {
        case 1   => ZIO.unit
        case res => ZIO.fail(new RuntimeException(s"Healthcheck returned something other than `1` : $res"))
      }

  private[sql] val executionConfig: URIO[Database, DbConfig.Execution] =
    ZIO.serviceWith[Database](_.executionConfig)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Connection State
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private[sql] sealed trait ConnectionState {

    val connectionType: ConnectionState.ConnectionType.Any

    def getConnection: ZIO[Scope, ConnectionError, Connection]
    def getAtomicChild: ZIO[Scope, ConnectionError, ConnectionState.SingleConnection]

    final def getConnectionAndType: ZIO[Scope, ConnectionError, (Connection, ConnectionState.ConnectionType.Any)] =
      getConnection.map((_, connectionType))

  }
  private[sql] object ConnectionState {

    object ConnectionType {

      sealed trait Any
      case object Transactionless extends Any

      sealed trait Transactional extends Any

      case object Transaction extends Transactional
      final case class Savepoint(id: UUID) extends Transactional {
        val ref: String = s"sp_${id.toString.filterNot(_ == '-')}"
      }
    }

    sealed trait SingleConnection extends ConnectionState {

      val connection: Connection
      val mutex: Semaphore
      override val connectionType: ConnectionType.Transactional

      override final def getConnection: ZIO[Scope, ConnectionError, Connection] =
        mutex.withPermitScoped.as(connection)

      override final def getAtomicChild: ZIO[Scope, ConnectionError, SingleConnection] =
        for {
          _ <- mutex.withPermitScoped
          mutex2 <- Semaphore.make(1L)
          savepointId <- Random.nextUUID
        } yield InSavepoint(connection, mutex2, savepointId)

    }

    final case class Pool(pool: ConnectionPool) extends ConnectionState {

      override val connectionType: ConnectionType.Any = ConnectionType.Transactionless

      override def getConnection: ZIO[Scope, ConnectionError, Connection] =
        pool.get

      override def getAtomicChild: ZIO[Scope, ConnectionError, SingleConnection] =
        for {
          connection <- pool.get
          mutex <- Semaphore.make(1L)
          transactionId <- Random.nextUUID
          _ <- connection.disableAutoCommitScoped
        } yield InTransaction(connection, mutex, transactionId)

    }

    final case class InTransaction(connection: Connection, mutex: Semaphore, transactionId: UUID) extends ConnectionState.SingleConnection {
      override val connectionType: ConnectionType.Transactional = ConnectionType.Transaction
    }

    final case class InSavepoint(connection: Connection, mutex: Semaphore, savepointId: UUID) extends ConnectionState.SingleConnection {
      override val connectionType: ConnectionType.Transactional = ConnectionType.Savepoint(savepointId)
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Annotations
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private final class ModifyLogConfig(f: DbConfig.Logging => DbConfig.Logging) extends ZIOAspectAtLeastR.Impl[Database] {

    override def apply[R <: Database, E, A](effect: ZIO[R, E, A])(using trace: Trace): ZIO[R, E, A] =
      ZIO.serviceWithZIO[Database](_.logConfigRef.locallyWith(f)(effect))

  }

  object withQueryLogLevel {

    def apply(logLevel: LogLevel): ZIOAspectAtLeastR[Database] = ModifyLogConfig(_.copy(queryLogLevel = logLevel))

    val all: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.All)
    val fatal: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Fatal)
    val error: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Error)
    val warning: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Warning)
    val important: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Important)
    val info: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Info)
    val detailed: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Detailed)
    val debug: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Debug)
    val trace: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.Trace)
    val none: ZIOAspectAtLeastR[Database] = withQueryLogLevel(LogLevels.None)

  }

}
