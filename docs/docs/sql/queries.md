# Queries

Queries come in two flavors: the **CRUD set generated** on every `TableCompanion`, and
**hand-written queries** compiled to SQL at compile time via the query DSL. Both produce the same
`Query` / `QueryI` / `QueryO` / `QueryIO` types and run the same way.

## Query types

| Type | Input | Output | Run with |
|------|-------|--------|----------|
| `Query` | — | — | `.execute()` → `Update` |
| `QueryI[I]` | `I` | — | `.execute(i: I)` → `Update` |
| `QueryO[O]` | — | `O` rows | `.execute()` → `Returning` |
| `QueryIO[I, O]` | `I` | `O` rows | `.execute(i: I)` → `Returning` |

## Generated CRUD

Extending `TableCompanion[A, K]` gives you:

| Member | Type | SQL |
|--------|------|-----|
| `insert` | `QueryI[A]` | `INSERT INTO …` |
| `selectAll` | `QueryO[A]` | `SELECT … FROM …` |
| `selectByPK` | `QueryIO[K, A]` | `SELECT … WHERE pk = ?` |
| `update` | `QueryI[A]` | `UPDATE … SET <non-pk> WHERE pk = ?` |
| `deleteByPK` | `QueryI[K]` | `DELETE … WHERE pk = ?` |
| `upsert` | `QueryI[A]` | `INSERT … ON CONFLICT (pk) DO UPDATE …` |
| `insertOrDoNothing` | `QueryI[A]` | `INSERT … ON CONFLICT (pk) DO NOTHING` |
| `truncate` / `truncateCascade` | `Query` | `TRUNCATE …` |
| `select_*` | `QueryO[Long]` | `SELECT COUNT(*) …` |

For high-volume inserts, `batchOptimizedInsert` (and `…Upsert` / `…InsertOrDoNothing`) build
multi-row `VALUES (…), (…), …` statements, auto-chunked under the JDBC parameter limit.

> `upsert` / `insertOrDoNothing` require a primary key — they throw at construction on a key-less table.

## Executing & shaping results

Running a query yields a `ZIO[Database, QueryError, …]`. Pick a combinator for the shape you want.

**`Returning` (from `QueryO` / `QueryIO`):**

| Combinator | Result |
|------------|--------|
| `.single` | exactly one row (fails on 0 or >1) |
| `.option` | zero or one row (fails on >1) |
| `.to[S]` | all rows as `S[A]` (`List`, `Seq`, …) |
| `.chunk` / `.arraySeq` | all rows as `Chunk` / `ArraySeq` |
| `.stream` / `.streamWithFetchSize(n)` | lazy `ZStream` of rows |

**`Update` (from `Query` / `QueryI`):**

| Combinator | Result |
|------------|--------|
| `.updated` | number of rows affected (`Int`) |
| `.unit` | discard the count |

```scala
import oxygen.sql.*

// single row by PK
val one: ZIO[Database, QueryError, Option[Person]] =
  Person.selectByPK.execute(id).option

// all rows
val all: ZIO[Database, QueryError, List[Person]] =
  Person.selectAll.execute().to[List]

// write
val ins: ZIO[Database, QueryError, Unit] =
  Person.insert.execute(person).unit
```

`.usingDb(db)` provides a `Database` directly when you already hold one (common in repos):

```scala
Person.selectByPK.execute(id).option.orDie.usingDb(db)   // ZIO[Any, Nothing, Option[Person]]
```

## Hand-written queries

Write a query as a `for`-comprehension over the DSL and compile it. Two equivalent forms:

```scala
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*

// builder form
val userByEmail: QueryIO[Email, UserRow] =
  QueryIO.compile("userByEmail") {
    for {
      email <- input[Email]
      u     <- select[UserRow]
      _     <- where if u.referenceEmail == email
    } yield u
  }
```

```scala
import oxygen.sql.query.dsl.compile

// annotation form (identical result)
@compile
val userByEmail: QueryIO[Email, UserRow] =
  for {
    email <- input[Email]
    u     <- select[UserRow]
    _     <- where if u.referenceEmail == email
  } yield u
```

The shape of the comprehension determines the type: an `input[I]` makes it a `QueryIO`/`QueryI`; no
input makes it a `QueryO`/`Query`. Pass `debug = true` (`@compile(debug = true)` /
`QueryIO.compile("name", true)`) to print the generated SQL at compile time.

### DSL vocabulary

| Form | Purpose |
|------|---------|
| `input[I]` / `input.optional[I]` / `input.const(i)` | bind a runtime / optional / compile-time-constant parameter |
| `input.array[I]` / `input.set[I]` + `ids.contains(col)` | bind a whole `Seq` / `Set` as one array parameter, expanded to `col = ANY(?)` |
| `select.unnest(ids)` | use a collection input as a `UNNEST(?)` join/table source (one row per element) |
| `select[A]` | select all columns of table `A` |
| `join[A] if <cond>` / `leftJoin[A] if <cond>` | inner / left join (`leftJoin` yields `Option[A]`) |
| `where if <cond>` | filter |
| `s.like(p)` / `s.ilike(p)` / `s.notLike(p)` / `s.notILike(p)` | string pattern match (see below) |
| `orderBy(a.field.asc, …)`, `limit(n)`, `offset(n)` | ordering / paging |
| `Q.insert[A]` / `Q.update[A]` / `Q.delete[A]` | begin an insert / update / delete |
| `set(_.field := value)` | assignment in an update |
| `count.*` / `count(a.field)` | aggregate |
| `a.tablePK` / `a.tableNPK` | the row's PK / non-PK columns |

A join example returning a tuple:

```scala
@compile
val personJoinNotes: QueryIO[UUID, (Person, Note)] =
  for {
    i <- input[UUID]
    p <- select[Person]
    n <- join[Note] if n.personId == p.id
    _ <- where if p.groupId == i
  } yield (p, n)
```

### String pattern matching (`LIKE` / `ILIKE`)

On `String` columns, four predicates map to SQL's pattern-matching operators. The pattern binds as a
single scalar `String` parameter, so the generated SQL stays static (`col LIKE ?`):

| Form | SQL | Notes |
|------|-----|-------|
| `col.like(pattern)` | `col LIKE ?` | case-sensitive |
| `col.ilike(pattern)` | `col ILIKE ?` | case-insensitive (Postgres-specific) |
| `col.notLike(pattern)` | `col NOT LIKE ?` | |
| `col.notILike(pattern)` | `col NOT ILIKE ?` | case-insensitive (Postgres-specific) |

The pattern uses the usual SQL wildcards: `%` matches any sequence of characters and `_` matches
exactly one. Predicates compose with `&&` / `||` like any other condition.

```scala
@compile
val searchByName: QueryIO[String, Person] =
  for {
    pattern <- input[String]
    p       <- select[Person]
    _       <- where if p.first.ilike(pattern)   // e.g. "al%" matches "Alice", "alfred", …
  } yield p
```

> Custom column types flow through automatically: `input[Email]` and `select[UserRow]` use the
> `RowRepr`/encoder/decoder for `Email` you defined in [Models](models.md).

### Array input (`= ANY(?)`)

To filter against a collection of values, bind the whole collection as a **single** array parameter
with `input.array[I]` (a `Seq[I]`) or `input.set[I]` (a `Set[I]`) and test membership with
`ids.contains(col)`. This generates Postgres `col = ANY(?)` — one bind parameter carrying a
`java.sql.Array`, instead of expanding an `IN (…)` list to N placeholders (which hits the JDBC
~32767-param limit and blows up the plan cache).

```scala
@compile
val selectByIdArray: QueryIO[Seq[UUID], Person] =
  for {
    ids <- input.array[UUID]
    p   <- select[Person]
    _   <- where if ids.contains(p.id)
  } yield p
```

```scala
selectByIdArray.execute(Seq(id1, id2, id3))
```

Notes:

- `input.array[I]` takes a `Seq[I]`, `input.set[I]` takes a `Set[I]`; either is bound as one JDBC
  array, so callers pass their collection directly (no `ArraySeq.from(…)` conversion needed).
- An empty collection yields `col = ANY('{}')`, i.e. no matches.
- `I` must be a single-column type (e.g. `UUID`, `Long`, `String`, or a `RowRepr` newtype over one).
  Composite/multi-column element types are not supported yet.
- Array inputs compose with other inputs, e.g. `where if ids.contains(p.id) && p.groupId == groupId`.
- No explicit `::type[]` cast is emitted: the JDBC driver builds a typed array via
  `createArrayOf(<base type>, …)`, so `= ANY(?)` already knows the element type.

### Array input as a join table (`UNNEST(?)`)

The primary way to use a collection input is as a **table source** you can join against: `select.unnest(ids)`
turns an `input.array[I]` / `input.set[I]` collection into a `UNNEST(?)` from-item that yields one row
per element, aliased so it can be joined/filtered/selected like any other table. The whole collection
still binds as a single `java.sql.Array` parameter.

```scala
@compile
val notesByPersonIdUnnest: QueryIO[Seq[UUID], Note] =
  for {
    ids <- input.array[UUID]
    id  <- select.unnest(ids)          // FROM UNNEST(?::uuid[]) id(id)
    n   <- join[Note] if n.personId == id
  } yield n
```

generates (roughly):

```sql
SELECT n.id, n.person_id, n.note
    FROM UNNEST(?::uuid[]) id(id)
    JOIN note n ON n.person_id = id.id
```

Notes:

- The unnested element is a normal query variable — reference it in the join/where (`n.personId == id`)
  or select it (`yield (id, n)`). It is emitted as a named, qualified column (`id.id`) so it never
  collides with a same-named column of a joined table.
- An empty array produces zero rows (the join matches nothing).
- `I` must be a single-column type, same restriction as `input.array` / `input.set`.
- Composes with other inputs/joins/wheres, e.g. an extra `input[String]` used in a `where`.
- Currently supported as the root `FROM` source; `JOIN UNNEST(?)` (unnest as a non-root join item)
  is not yet supported.

## A real repo method

```scala
override def findUserByEmail(email: Email): UIO[Option[FullUser]] =
  UserRow.userByEmail
    .map(_.toDomain)        // transform each row
    .execute(email)         // bind the input
    .option                 // 0-or-1
    .orDie                  // QueryError -> defect
    .usingDb(db)            // provide the Database
```
