package oxygen.sql

import oxygen.predef.test.*
import oxygen.schema.JsonSchema
import scala.jdk.CollectionConverters.*
import zio.*

object DbConfigSpec extends OxygenSpecDefault {

  private def propsToMap(p: java.util.Properties): Map[String, String] =
    p.stringPropertyNames().asScala.iterator.map(k => k -> p.getProperty(k)).toMap

  private def sslModeSpec: TestSpec =
    suite("SslMode")(
      test("encodes to postgres sslmode spellings") {
        assertTrue(
          DbConfig.SslMode.Disable.sslmode == "disable",
          DbConfig.SslMode.Allow.sslmode == "allow",
          DbConfig.SslMode.Prefer.sslmode == "prefer",
          DbConfig.SslMode.Require.sslmode == "require",
          DbConfig.SslMode.VerifyCa.sslmode == "verify-ca",
          DbConfig.SslMode.VerifyFull.sslmode == "verify-full",
        )
      },
      test("decodes (case-insensitively) via JsonSchema") {
        assertTrue(
          JsonSchema[DbConfig.SslMode].decode("\"prefer\"") == Right(DbConfig.SslMode.Prefer),
          JsonSchema[DbConfig.SslMode].decode("\"verify-full\"") == Right(DbConfig.SslMode.VerifyFull),
          JsonSchema[DbConfig.SslMode].decode("\"REQUIRE\"") == Right(DbConfig.SslMode.Require),
          JsonSchema[DbConfig.SslMode].decode("\"nonsense\"").isLeft,
        )
      },
    )

  private def propertiesSpec: TestSpec =
    suite("Connection.properties")(
      test("default is empty") {
        assertTrue(DbConfig.Connection.default.properties.isEmpty)
      },
      test("fully-populated typed settings translate to postgres property pairs") {
        val conn = DbConfig.Connection(
          sslMode = DbConfig.SslMode.Require.some,
          sslRootCert = "/certs/root.crt".some,
          sslCert = "/certs/client.crt".some,
          sslKey = "/certs/client.key".some,
          connectTimeout = 10.seconds.some,
          socketTimeout = 30.seconds.some,
          applicationName = "oxygen-app".some,
          extraProperties = Map("tcpKeepAlive" -> "true"),
        )
        assertTrue(
          conn.properties.toMap == Map(
            "sslmode" -> "require",
            "sslrootcert" -> "/certs/root.crt",
            "sslcert" -> "/certs/client.crt",
            "sslkey" -> "/certs/client.key",
            "connectTimeout" -> "10",
            "socketTimeout" -> "30",
            "ApplicationName" -> "oxygen-app",
            "tcpKeepAlive" -> "true",
          ),
        )
      },
    )

  private def preparedStatementCacheSpec: TestSpec =
    suite("PreparedStatementCache")(
      test("default emits no properties (leaves pgjdbc defaults in force)") {
        assertTrue(DbConfig.PreparedStatementCache.default.properties.isEmpty)
      },
      test("populated settings translate to pgjdbc property pairs") {
        val cache = DbConfig.PreparedStatementCache(
          prepareThreshold = 3.some,
          cacheQueries = 512.some,
          cacheSizeMiB = 10.some,
        )
        assertTrue(
          cache.properties.toMap == Map(
            "prepareThreshold" -> "3",
            "preparedStatementCacheQueries" -> "512",
            "preparedStatementCacheSizeMiB" -> "10",
          ),
        )
      },
      test("zeros are emitted verbatim (disable caching)") {
        val cache = DbConfig.PreparedStatementCache(cacheQueries = 0.some, cacheSizeMiB = 0.some)
        assertTrue(
          cache.properties.toMap == Map(
            "preparedStatementCacheQueries" -> "0",
            "preparedStatementCacheSizeMiB" -> "0",
          ),
        )
      },
      test("cache pairs surface in Connection.properties, before the extraProperties escape hatch") {
        val conn = DbConfig.Connection(
          preparedStatementCache = DbConfig.PreparedStatementCache(cacheQueries = 256.some),
          extraProperties = Map("preparedStatementCacheQueries" -> "1"),
        )
        val collapsed: Map[String, String] = conn.properties.toMap
        assertTrue(
          // typed cache pair is present, and extraProperties (applied last) wins on collision
          conn.properties.contains("preparedStatementCacheQueries" -> "256"),
          collapsed("preparedStatementCacheQueries") == "1",
        )
      },
      test("cache pairs are threaded through buildProperties") {
        val conn = DbConfig.Connection(preparedStatementCache = DbConfig.PreparedStatementCache(prepareThreshold = 2.some))
        val props = Driver.JdbcDriver.buildProperties(DbConfig.Credentials("u", "p").some, conn)
        assertTrue(
          propsToMap(props) == Map(
            "user" -> "u",
            "password" -> "p",
            "prepareThreshold" -> "2",
          ),
        )
      },
      test("preparedStatementCache block decodes from JSON") {
        val json =
          """{
            |  "preparedStatementCache": { "prepareThreshold": 3, "cacheQueries": 512, "cacheSizeMiB": 10 }
            |}""".stripMargin
        assertTrue(
          JsonSchema[DbConfig.Connection].decode(json) == Right(
            DbConfig.Connection(
              preparedStatementCache = DbConfig.PreparedStatementCache(
                prepareThreshold = 3.some,
                cacheQueries = 512.some,
                cacheSizeMiB = 10.some,
              ),
            ),
          ),
        )
      },
      test("omitting the block defaults to the empty cache") {
        assertTrue(JsonSchema[DbConfig.Connection].decode("{}").map(_.preparedStatementCache) == Right(DbConfig.PreparedStatementCache.default))
      },
    )

  private def buildPropertiesSpec: TestSpec =
    suite("JdbcDriver.buildProperties")(
      test("only user/password when connection is unset (preserves historical behavior)") {
        val props = Driver.JdbcDriver.buildProperties(DbConfig.Credentials("u", "p").some, DbConfig.Connection.default)
        assertTrue(propsToMap(props) == Map("user" -> "u", "password" -> "p"))
      },
      test("no credentials + no connection settings => empty props") {
        val props = Driver.JdbcDriver.buildProperties(None, DbConfig.Connection.default)
        assertTrue(propsToMap(props).isEmpty)
      },
      test("credentials + typed connection settings are all present") {
        val conn = DbConfig.Connection(sslMode = DbConfig.SslMode.Prefer.some, applicationName = "svc".some)
        val props = Driver.JdbcDriver.buildProperties(DbConfig.Credentials("u", "p").some, conn)
        assertTrue(
          propsToMap(props) == Map(
            "user" -> "u",
            "password" -> "p",
            "sslmode" -> "prefer",
            "ApplicationName" -> "svc",
          ),
        )
      },
      test("extraProperties are applied last and override typed + credentials on collision") {
        val conn = DbConfig.Connection(
          sslMode = DbConfig.SslMode.Prefer.some,
          extraProperties = Map("sslmode" -> "require", "password" -> "override"),
        )
        val props = Driver.JdbcDriver.buildProperties(DbConfig.Credentials("u", "p").some, conn)
        assertTrue(
          props.getProperty("sslmode") == "require",
          props.getProperty("password") == "override",
          props.getProperty("user") == "u",
        )
      },
    )

  private def decodingSpec: TestSpec =
    suite("JSON decoding")(
      test("connection block decodes from JSON") {
        val json =
          """{
            |  "sslMode": "verify-full",
            |  "sslRootCert": "/certs/root.crt",
            |  "connectTimeout": "PT10S",
            |  "applicationName": "oxygen-app",
            |  "extraProperties": { "tcpKeepAlive": "true" }
            |}""".stripMargin
        assertTrue(
          JsonSchema[DbConfig.Connection].decode(json) == Right(
            DbConfig.Connection(
              sslMode = DbConfig.SslMode.VerifyFull.some,
              sslRootCert = "/certs/root.crt".some,
              connectTimeout = 10.seconds.some,
              applicationName = "oxygen-app".some,
              extraProperties = Map("tcpKeepAlive" -> "true"),
            ),
          ),
        )
      },
      test("full DbConfig decodes with connection omitted (defaults to empty)") {
        val json =
          """{
            |  "target": { "database": "db", "host": "localhost", "port": 5432 },
            |  "credentials": { "username": "u", "password": "p" }
            |}""".stripMargin
        assertTrue(JsonSchema[DbConfig].decode(json).map(_.connection) == Right(DbConfig.Connection.default))
      },
      test("full DbConfig decodes with a connection block") {
        val json =
          """{
            |  "target": { "database": "db", "host": "localhost", "port": 5432 },
            |  "credentials": { "username": "u", "password": "p" },
            |  "connection": { "sslMode": "require", "socketTimeout": "PT30S" }
            |}""".stripMargin
        assertTrue(
          JsonSchema[DbConfig].decode(json).map(_.connection) == Right(
            DbConfig.Connection(sslMode = DbConfig.SslMode.Require.some, socketTimeout = 30.seconds.some),
          ),
        )
      },
    )

  override def testSpec: TestSpec =
    suite("DbConfigSpec")(
      sslModeSpec,
      propertiesSpec,
      preparedStatementCacheSpec,
      buildPropertiesSpec,
      decodingSpec,
    )

}
