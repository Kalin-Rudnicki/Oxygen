package oxygen.sql

import oxygen.sql.error.*
import zio.*

trait ConnectionPool {

  def get: ZIO[Scope, ConnectionError, Connection]

}
object ConnectionPool {

  def makeZPool(connect: Driver.GetConnection, cfg: DbConfig.Pool): URIO[Scope, ConnectionPool] =
    ZPool.make(connect.getConnection, cfg.minConnections to cfg.maxConnections, cfg.duration).map(ZConnectionPool(_))

  val zPool: URLayer[Driver.GetConnection & DbConfig.Pool, ConnectionPool] =
    ZLayer.scoped {
      for {
        connect <- ZIO.service[Driver.GetConnection]
        cfg <- ZIO.service[DbConfig.Pool]
        pool <- makeZPool(connect, cfg)
      } yield pool
    }

  final case class ZConnectionPool(pool: ZPool[ConnectionError, Connection]) extends ConnectionPool {

    private val maxAttempts: Int = 10

    private def getConnectionLoop(attemptNo: Int): ZIO[Scope, ConnectionError, Connection] =
      pool.get.flatMap { con =>
        if con.connection.isClosed then
          if attemptNo < maxAttempts then
            ZIO.logDebug(s"Attempt to acquire open connection #$attemptNo returned closed connection, invalidating connection and trying again") *>
              pool.invalidate(con) *>
              getConnectionLoop(attemptNo + 1)
          else ZIO.logWarning(s"Max attempts to acquire an open connection reached ($attemptNo), returning closed connection").as(con)
        else ZIO.succeed(con)
      }

    override def get: ZIO[Scope, ConnectionError, Connection] =
      getConnectionLoop(0)

  }

}
