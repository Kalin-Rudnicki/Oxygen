# Oxygen SQL

`oxygen-sql` is a typed, macro-driven PostgreSQL layer for ZIO. You define tables as case classes;
macros derive the column schema, generate CRUD queries, and compile hand-written queries to SQL at
**compile time**. A filesystem-first migration system keeps your schema in sync.

Depends on:

- `oxygen-sql` — schema derivation, query DSL, `Database`, connection pooling, transactions
- `oxygen-sql-migration` — the migration generator / applier (separate artifact)
- `oxygen-sql-test` — test utilities (`PostgresTestContainer`, `DbMigrationSpec`)

## Quick start

Define a table:

```scala
import oxygen.sql.query.*
import oxygen.sql.schema.*
import java.util.UUID

@tableName("person")
final case class Person(
    @primaryKey id: UUID,
    name: String,
    email: Option[String],
)
object Person extends TableCompanion[Person, UUID](TableRepr.derived[Person])
```

Run queries against a `Database`:

```scala
import oxygen.sql.*
import zio.*

val program: ZIO[Database, QueryError, Unit] =
  for {
    _      <- Person.insert.execute(Person(UUID.randomUUID(), "Ada", Some("ada@example.com")))
    people <- Person.selectAll.execute().to[List]
    _      <- ZIO.foreachDiscard(people)(p => ZIO.logInfo(p.name))
  } yield ()
```

Provide the stack as layers (see [Database & connection](database.md)):

```scala
program.provide(
  ZLayer.succeed(dbConfig),   // DbConfig
  Database.layer,
  Atomically.LiveDB.layer,
)
```

## Sections

- **[Models](models.md)** — defining tables: annotations, primary keys, column types, custom
  types, JSON, nested columns.
- **[Queries](queries.md)** — generated CRUD queries, the compile-time query DSL, and executing
  queries / shaping results.
- **[Database & connection](database.md)** — `DbConfig`, the `Database` layer, transactions via
  `Atomically`, and wiring the stack.
- **[Migrations](migrations.md)** — the filesystem-first migration workflow: generate, verify,
  apply.

## Mental model

| Concept | Type | Where it comes from |
|---------|------|---------------------|
| A table's schema | `TableRepr[A]` | `TableRepr.derived[A]` (macro) |
| A row's column mapping | `RowRepr[A]` | derived or `given` (custom types) |
| Generated CRUD | `QueryI` / `QueryO` / `QueryIO` | `extends TableCompanion[A, K]` |
| A hand-written query | `QueryIO[I, O]` | `@compile` / `QueryIO.compile { … }` |
| Running a query | `ZIO[Database, QueryError, …]` | `.execute(…)` + result combinators |
| Transactions | `@@ atomically` | `Atomically` layer |
| Schema history | `PersistedMigrationFile`s on disk | `MigrationSchema` + the generator |
