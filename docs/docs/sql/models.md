# Models

A table is a `final case class` plus a companion that extends `TableCompanion`. The macro
`TableRepr.derived[A]` inspects the case class and its annotations to produce the column schema,
primary key, foreign keys, and indices.

```scala
import oxygen.sql.query.*
import oxygen.sql.schema.*
import java.time.Instant

@tableName("user")
final case class UserRow(
    @primaryKey @columnName("user_id") id: UserId,
    email: Email,
    firstName: String,
    lastName: String,
    createdAt: Instant,
)
object UserRow extends TableCompanion[UserRow, UserId](TableRepr.derived[UserRow])
```

## Annotations

Defined in `oxygen.sql.schema`:

| Annotation | Effect | Default when omitted |
|------------|--------|----------------------|
| `@tableName("name")` | Table name | class name, camelCase → snake_case |
| `@schemaName("name")` | Schema name | `"public"` |
| `@columnName("name")` | Column name | field name, camelCase → snake_case |
| `@primaryKey` | Field is part of the primary key (composite if on several fields) | — |
| `@inlineColumnNames` | For an embedded case class, drop the field-name prefix on its columns | — |

## Companions & primary keys

`TableCompanion[A, K]` takes the derived repr and is parameterized by the **primary key type** `K`:

```scala
// single-column key
object UserRow extends TableCompanion[UserRow, UserId](TableRepr.derived[UserRow])

// composite key -> tuple
object ConnectionRequestRow
    extends TableCompanion[ConnectionRequestRow, (UserId, UserId)](TableRepr.derived[ConnectionRequestRow])
```

Extending `TableCompanion` is what gives you the generated CRUD queries (see [Queries](queries.md))
and an in-scope `given tableRepr` / `given rowRepr`.

> **No primary key?** Extend `TableCompanion.NoKey[A]` instead — it provides only `selectAll`.

## Column types

Each field type needs a `RowRepr[A]`, which maps it to one or more SQL columns. Built-in instances:

| Scala type | SQL column type |
|------------|-----------------|
| `Short` | `SMALLINT` |
| `Int` | `INT` |
| `Long` | `BIGINT` |
| `Float` | `REAL` |
| `Double` | `DOUBLE PRECISION` |
| `String` | `TEXT` |
| `Boolean` | `BOOLEAN` |
| `UUID` | `UUID` |
| `Instant` | `TIMESTAMP WITH TIME ZONE` |
| `LocalDateTime` | `TIMESTAMP` |
| `LocalDate` | `DATE` |
| `LocalTime` | `TIME` |
| `ArraySeq[A]` | `A[]` (array of the element's type) |
| any `derives StrictEnum` | `TEXT` |

PostgreSQL-specific types also exist: `Json` / `Jsonb`, `Vector(size)` (pgvector, needs the
`vector` extension), and `LTree` (needs the `ltree` extension). Required extensions are detected from
the columns and created by [migrations](migrations.md).

## Nullability

Wrap a field in `Option[A]` to make its column(s) nullable:

```scala
final case class Person(
    @primaryKey id: UUID,
    name: String,
    email: Option[String],   // -> nullable TEXT
)
```

## Custom types

Map a domain type onto an existing column type with `RowRepr.transform` (or `transformOrFail` for
fallible decoding). Provide it as a `given` so derivation finds it:

```scala
given userIdRowRepr: RowRepr[UserId] = RowRepr.uuid.transform(UserId(_), _.id)
given emailRowRepr:  RowRepr[Email]  = RowRepr.string.transform(Email.wrap, _.value)
```

`transform[B](ab: A => B, ba: B => A)` is bidirectional (decode / encode). `transformOrFail[B](ab: A
=> Either[String, B], ba: B => A)` lets decoding reject bad values.

## JSON columns

Store any `JsonCodec` type in a `jsonb` (or `json`) column with `TypedJsonb[A]` / `TypedJson[A]`
from `oxygen.sql.model` — a `given JsonCodec[A]` in scope is all that's needed:

```scala
import oxygen.sql.model.TypedJsonb

final case class EventRow(
    @primaryKey id: UUID,
    payload: TypedJsonb[Payload],   // requires given JsonCodec[Payload]
)
```

> **Watch typeless JSON columns.** A JSON column's *inner* shape is opaque to the schema today —
> changing `Payload` won't (yet) produce a migration, so a mismatch surfaces at runtime. Granular
> JSON-type diffing is planned; see the migration plan.

## Embedded (nested) case classes

A field whose type is itself a `RowRepr` product becomes multiple columns, prefixed by the field
name:

```scala
final case class Name(first: String, last: String) derives RowRepr.ProductRepr
final case class Person(@primaryKey id: UUID, name: Name)
// columns: id, name_first, name_last
```

Use `@inlineColumnNames` to drop the prefix (columns become `first`, `last`):

```scala
final case class Person(@primaryKey id: UUID, @inlineColumnNames name: Name)
```

## Deriving a standalone `RowRepr`

For a non-table value (e.g. an embedded type or a query result row), derive `RowRepr.ProductRepr`
directly:

```scala
final case class Stats(count: Int, total: Long) derives RowRepr.ProductRepr
```
