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

    override def get: ZIO[Scope, ConnectionError, Connection] =
      pool.get

  }

}
