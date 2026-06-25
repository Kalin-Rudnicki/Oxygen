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
| `select[A]` | select all columns of table `A` |
| `join[A] if <cond>` / `leftJoin[A] if <cond>` | inner / left join (`leftJoin` yields `Option[A]`) |
| `where if <cond>` | filter |
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

> Custom column types flow through automatically: `input[Email]` and `select[UserRow]` use the
> `RowRepr`/encoder/decoder for `Email` you defined in [Models](models.md).

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
