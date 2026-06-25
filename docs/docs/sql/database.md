# Database & connection

`Database` owns the JDBC connection pool, transaction/savepoint state, and query logging. Queries
run in `ZIO[Database, QueryError, …]`; you supply a `Database` via its `ZLayer`, built from a
`DbConfig`.

## DbConfig

`DbConfig` (`oxygen.sql.DbConfig`) `derives JsonCodec`, so it drops straight into an app config:

```scala
final case class DbConfig(
    target: DbConfig.Target,                 // database, host, port
    credentials: Option[DbConfig.Credentials], // username, password
    pool: DbConfig.Pool,                     // minConnections, maxConnections, duration
    logging: DbConfig.Logging,               // queryLogLevel, logSql
    execution: DbConfig.Execution,           // result-buffer tuning
)
```

As JSON (from the example app's `local.json`):

```json
{
  "target":      { "database": "oxygen_example", "host": "localhost", "port": 5210 },
  "credentials": { "username": "oxygen_username", "password": "oxygen_password" },
  "pool":        { "minConnections": 2, "maxConnections": 16, "duration": "PT5M" },
  "logging":     { "queryLogLevel": "Trace", "logSql": true },
  "execution":   { "bufferChunkSize": [16, 64, 64, 256], "bufferNumChunks": 2 }
}
```

- `duration` is a ZIO `Duration` (ISO-8601, e.g. `PT5M`) — the pool idle timeout.
- `queryLogLevel` is a ZIO `LogLevel`; `logSql` toggles logging the SQL text.
- `DbConfig.Execution.default` provides sensible buffer defaults.
- Credentials are marked `@jsonSecret`, so they're redacted in safe serialization.

## The Database layer

`Database.layer: URLayer[DbConfig, Database]` wires the driver, connection pool, and base database
from a `DbConfig` in the environment:

```scala
ZLayer.succeed(dbConfig) >>> Database.layer
```

`Database.healthCheck: RIO[Database, Unit]` runs `SELECT 1` to confirm connectivity.

## Transactions — `Atomically`

`Atomically` is a ZIO aspect. Apply `@@ atomically` to wrap an effect in a transaction; nested
applications become **savepoints**.

```scala
import oxygen.sql.*

val tx: ZIO[Database & Atomically, QueryError, Unit] =
  (for {
    _ <- Account.update.execute(debited)
    _ <- Account.update.execute(credited)
  } yield ()) @@ atomically     // COMMIT on success, ROLLBACK on failure
```

Provide one of:

| Layer | Behavior |
|-------|----------|
| `Atomically.LiveDB.layer` | real transactions — commit on success, roll back on failure |
| `Atomically.RollbackDB.layer` | always rolls back the outermost transaction (for tests) |

`.usingDb(db)` is the lower-level escape hatch to provide a `Database` to an effect or stream
without going through the environment — handy inside a repo that already holds a `db`.

## Wiring the stack

A typical app builds the DB stack as part of its `ZLayer.make`:

```scala
ZLayer.make[Env](
  // … app layers …
  ZLayer.succeed(config.db),     // DbConfig
  Database.layer,
  Atomically.LiveDB.layer,
  // … migration layers (see Migrations) …
)
```

In tests, `oxygen-sql-test` provides a Postgres container:

```scala
val databaseLayer: ZLayer[TestContainerService, TestContainerError, Database] =
  ZLayer.makeSome[TestContainerService, Database](
    ZLayer.succeed(DbConfig.Pool(8, 16, 5.minutes)),
    ZLayer.succeed(DbConfig.Logging(LogLevel.Trace, true)),
    ZLayer.succeed(DbConfig.Execution.default),
    PostgresTestContainer.layer,   // supplies DbConfig.Target + Credentials
    Database.layer,
  )
```

## Test isolation

`SqlAspects.isolateTestsInRollbackTransaction` (in `oxygen-sql-test`) wraps each test in a
rollback transaction (`@@ Atomically.RollbackDB.atomically`), so parallel tests don't see each
other's writes:

```scala
override def testAspects = Chunk(SqlAspects.isolateTestsInRollbackTransaction)
```

## Query logging

Temporarily change the query log level for a scoped effect with the `Database.withQueryLogLevel`
aspects:

```scala
noisyQuery.execute().unit @@ Database.withQueryLogLevel.debug
```
