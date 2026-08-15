package oxygen.sql

import oxygen.sql.error.*
import zio.*

trait Driver {

  def getConnection(target: DbConfig.Target, credentials: Option[DbConfig.Credentials], connection: DbConfig.Connection): Driver.GetConnection

}
object Driver {

  def makeJdbc(dbUrlPrefix: String, driver: => java.sql.Driver): UIO[Driver] =
    ZIO.succeed { JdbcDriver(dbUrlPrefix, driver) }

  def jdbcLayer(dbUrlPrefix: String, driver: => java.sql.Driver): ULayer[Driver] =
    ZLayer { makeJdbc(dbUrlPrefix, driver) }

  val makePSQL: UIO[Driver] = makeJdbc("postgresql", new org.postgresql.Driver())
  val psql: ULayer[Driver] = ZLayer { makePSQL }

  final case class JdbcDriver(dbUrlPrefix: String, driver: java.sql.Driver) extends Driver {

    override def getConnection(target: DbConfig.Target, credentials: Option[DbConfig.Credentials], connection: DbConfig.Connection): Driver.GetConnection =
      Driver.GetConnection {
        Connection.wrapUnsafeJdbc { driver.connect(target.jdbcUrl(dbUrlPrefix), JdbcDriver.buildProperties(credentials, connection)) }
      }

  }
  object JdbcDriver {

    /**
      * Assemble the JDBC `Properties` passed to `driver.connect`.
      *
      * Application order (later wins on key collision):
      *   1. `user`/`password` from credentials
      *   2. typed connection settings ([[DbConfig.Connection]])
      *   3. `extraProperties` escape hatch (copied verbatim)
      */
    def buildProperties(credentials: Option[DbConfig.Credentials], connection: DbConfig.Connection): java.util.Properties = {
      val props = new java.util.Properties
      credentials.foreach { credentials =>
        props.put("user", credentials.username)
        props.put("password", credentials.password)
      }
      connection.properties.foreach { case (k, v) => props.put(k, v) }
      props
    }

  }

  final case class GetConnection(getConnection: ZIO[Scope, ConnectionError, Connection])
  object GetConnection {

    val layer: URLayer[Driver & DbConfig.Target & Option[DbConfig.Credentials] & DbConfig.Connection, Driver.GetConnection] =
      ZLayer.fromZIO {
        for {
          driver <- ZIO.service[Driver]
          target <- ZIO.service[DbConfig.Target]
          credentials <- ZIO.service[Option[DbConfig.Credentials]]
          connection <- ZIO.service[DbConfig.Connection]
        } yield driver.getConnection(target, credentials, connection)
      }

  }

}
