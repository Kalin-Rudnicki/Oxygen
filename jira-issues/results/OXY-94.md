# OXY-94 — Add `on conflict` support to query dsl

## Original
- **Key:** OXY-94
- **Checklist line:** `- [ ] [OXY-94](https://kr-oxygen.atlassian.net/browse/OXY-94) — **Task** · Lower — Add `on conflict` support to query dsl`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add `on conflict` support to query dsl
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-94
- **Checklist section:** To Do
- **Epic (inferred):** OXY-1 `oxygen-sql` (In Progress) — title `on conflict` is Postgres `INSERT ... ON CONFLICT` and module `query dsl` is `modules/sql` (`oxygen-sql`). No explicit epic link in `checklist.md`, but `oxygen-sql` is the only module with `TableCompanion`, `Q.insert`, and `ParsedQuery.InsertQuery`.

## Expanded Description

**What this likely is:** Add `ON CONFLICT` support to the hand-written query DSL (`modules/sql` — `@compile` / `QueryIO.compile` + `Q.insert[A]` / `Q.insert.fromSelect[A]`) so inserts can be written with Postgres upsert semantics at compile time.

Today `ON CONFLICT` only exists in the **generated CRUD** layer (`modules/sql/core/src/main/scala/oxygen/sql/query/TableCompanion.scala:24-75`): `TableCompanion.upsert` builds `INSERT ... ON CONFLICT (pk) DO UPDATE SET <non-pk> = EXCLUDED.<non-pk>` and `insertOrDoNothing` builds `INSERT ... ON CONFLICT (pk) DO NOTHING`, both by string-concatenating `onConflict` onto `insert.ctx.sql`. `batchOptimizedUpsert`/`batchOptimizedInsertOrDoNothing` reuse the same `QueryI`. There is **no `ON CONFLICT` in the hand-written DSL** — verified by grep: `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/InsertPart.scala`, `ParsedQuery.scala:40-155`, `FragmentBuilder.scala:279-430`, `Q.scala`, `T.scala`, and `docs/docs/sql/queries.md:116-129` have no `onConflict`/`ON_CONFLICT`/`OnConflict` case, no `OnConflictPart`, and DSL vocabulary lists only `input`/`select`/`join`/`where`/`orderBy`/`limit`/`offset`/`Q.insert[A]`/`set`/`count`.

That means a hand-written insert cannot specify conflict handling:

```scala
// today: no way to express ON CONFLICT
@compile
val insertPerson: QueryI[Person] =
  for {
    p <- input[Person]
    (_, into) <- Q.insert[Person]
    _ <- into(p)
  } yield ()
// would need something like:
@compile
val upsertPerson: QueryI[Person] =
  for {
    p <- input[Person]
    (_, into) <- Q.insert[Person]
    _ <- into(p)
    _ <- onConflict(_.id) doUpdate _.tableNPK  // or: onConflict.doNothing / onConflictDoUpdate
  } yield ()
```

In Postgres `ON CONFLICT` has several forms — the title's brevity leaves which subset is intended ambiguous, but all reduce to the same missing DSL+SQL-generation gap:

1.  **PK-based `DO NOTHING` (minimal):** `INSERT ... ON CONFLICT (pk) DO NOTHING` — dedup on primary key, ignore duplicate.
2.  **PK-based `DO UPDATE` (upsert):** `INSERT ... ON CONFLICT (pk) DO UPDATE SET col = EXCLUDED.col, ...` — current `TableCompanion.upsert` semantics but exposed in DSL so any hand-written insert (including `insert.fromSelect`) can upsert.
3.  **Arbitrary conflict target:** `ON CONFLICT (col)` / `ON CONFLICT (col1, col2)` / `ON CONFLICT ON CONSTRAINT constraint_name` where the target is a unique index/constraint, not necessarily the PK. Needed when the duplicate key is not the PK (e.g., `email` UNIQUE).
4.  **Partial index predicate:** `ON CONFLICT (col) WHERE <pred> DO ...` — Postgres `WHERE` clause on the conflict target (rare, follow-up).
5.  **Conditional update:** `DO UPDATE SET ... WHERE <pred>` — only update if predicate holds.

**Who it affects:** Every service using `oxygen-sql` hand-written inserts (`@compile` / `QueryIO.compile`) that needs idempotent writes or upserts with custom logic — `insertPersonOrDoNothing`, `upsertPersonFromSelect`, `insertNoteForPeople` with dedup, or any `Q.insert.fromSelect` that currently has no conflict handling. Current workaround is to use `TableCompanion.upsert`/`insertOrDoNothing` (PK-only, no custom SET logic, no `fromSelect`, no arbitrary unique constraint), fall back to raw string SQL, or catch `PSQLCode.unique_violation` (`23505`) and retry — all defeat the DSL's type-safe, compiled-SQL promise.

**Why it matters (Priority Lower):** `ON CONFLICT` is the idiomatic Postgres idempotency mechanism; without it in the DSL, hand-written inserts that need dedup must bypass the DSL or restrict themselves to the generated `upsert` which only covers PK + `SET <non-pk> = EXCLUDED.<non-pk>` (TableCompanion.scala:32-36) and cannot express `DO NOTHING` vs `DO UPDATE` choice per query, non-PK unique constraints (e.g., `email`), or `insert.fromSelect` + conflict handling. Priority `Lower` (vs OXY-17 High) suggests this is ergonomic/nice-to-have, not blocking — generated CRUD already covers the common PK upsert path.

**Inferred acceptance criteria:**

1.  DSL syntax for `ON CONFLICT` usable after `Q.insert[A]` / `Q.insert.fromSelect[A]` + `into(...)` in a `for`-comprehension, at minimum:
    ```scala
    // DO NOTHING on PK (minimal viable)
    @compile
    val insertDoNothing: QueryI[Person] =
      for {
        p <- input[Person]
        (_, into) <- Q.insert[Person]
        _ <- into(p)
        _ <- onConflictDoNothing   // or: Q.onConflict.doNothing / into.onConflict.doNothing
      } yield ()

    // DO UPDATE on PK (upsert) — mirrors TableCompanion.upsert
    @compile
    val upsert: QueryI[Person] =
      for {
        p <- input[Person]
        (_, into) <- Q.insert[Person]
        _ <- into(p)
        _ <- onConflictDoUpdate(_.tableNPK) // sets non-pk = EXCLUDED.non-pk
      } yield ()

    // arbitrary target
    @compile
    val upsertByEmail: QueryI[UserRow] =
      for {
        u <- input[UserRow]
        (_, into) <- Q.insert[UserRow]
        _ <- into(u)
        _ <- onConflict(_.email) doNothing
        // or: onConflict(_.email) doUpdate (_.name := excluded(_.name))
      } yield ()
    ```
    Exact spelling is unspecified — any of the above satisfies "on conflict", but it must be documented. Must compose with `Q.insert.fromSelect` as well.

2.  Generated SQL appends ` ON CONFLICT (...) DO NOTHING` or ` ON CONFLICT (...) DO UPDATE SET ...` to the `INSERT` statement. Conflict target derived from `TableRepr` columns (PK by default, or explicit column selectors like `_.email`, `_.id1, _.id2` for composite PK/unique). If `ON CONSTRAINT` form is supported, accept a constraint name string: `onConflict(constraint("uq_email"))`.

3.  `DO UPDATE SET` semantics: at minimum `SET <non-pk> = EXCLUDED.<non-pk>` (mirrors `TableCompanion.upsert:32-36`) or explicit `EXCLUDED` reference: `set(_.field := excluded(_.field))`. Must handle the `npkCols.isEmpty` edge (table with only PK columns) → `DO NOTHING` fallback (TableCompanion.scala:37-39).

4.  Compile-time macro support: new `OnConflictPart` (or `ConflictClause` / `OnConflictExpr`) in `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/`, new `ParsedQuery.InsertQuery` field `onConflict: Option[OnConflictPart]`, parsing in `PartialQuery.InsertQuery.Basic/FromSelect` chain, and a `FragmentBuilder.onConflict` branch that emits the clause with correct column names (`TableRepr` → `RowRepr.columns.columns.map(_.name)`) and `EXCLUDED.<col>` refs.

5.  Works as `Query` / `QueryI[A]` via `Q.insert[A]` + `into` + `onConflict`; batch path `BatchOptimizedInsert.unsafeParse` must continue to work (it parses `QueryI.ctx.sql` string — the generated SQL with `ON CONFLICT` suffix must still be sliceable into `SqlParts`).

6.  Tests + docs: `docs/docs/sql/queries.md` DSL vocabulary updated with `onConflict` rows and cross-link to `models.md` primaryKey/unique notes; `it-test` (`modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala` + `TableCompanionQuerySpec`) compiles the new syntax (`debug = true` snapshot shows `ON CONFLICT ...`) and integration-tests against real Postgres (`PostgresTestContainer` + `DbMigrationSpec`) covering: insert + duplicate → `DO NOTHING` returns 0 updated, `DO UPDATE` overwrites non-PK, composite PK target, `insert.fromSelect` + conflict, and batchOptimizedInsert with conflict.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title "`on conflict` + query dsl" has a single dominant meaning in Postgres/`oxygen-sql`: `INSERT ... ON CONFLICT`. No other module uses "on conflict" as a feature name, and `modules/sql` is the only place where it would be a missing DSL keyword. Existing CRUD proves the feature exists (`TableCompanion.upsert`/`insertOrDoNothing` with `ON CONFLICT (${pkCols.map(_.name).mkString(", ")})`) while the DSL chain (`InsertPart` → `ParsedQuery` → `FragmentBuilder` → `Q.insert`) has zero `ON CONFLICT` support — the gap is verified, not inferred.
  - Pattern matches sibling DSL-extension tasks (OXY-6 array+unnest, OXY-17 IN, OXY-100 group by) where the missing SQL syntax maps 1:1 to the title.
  - Downgraded from 5/6 because title is 2 words "`on conflict`" with no Jira body fetched and no `TODO`/`FIXME`/`skip` comment mentioning `onConflict`/`ON CONFLICT` in the DSL code. Exact DSL spelling (`onConflictDoNothing` vs `onConflict(_.col).doNothing` vs `Q.onConflict` vs chain on `into`), conflict-target selection (PK-default vs explicit column vs constraint name), and whether `WHERE` predicates / `DO UPDATE WHERE` are in scope are unspecified — so implementation details remain inferred.
  - Remaining alternative (ON CONFLICT for UPDATE/DELETE, or optimistic-locking `ON CONFLICT` for `UPDATE`) is materially less likely — Postgres `ON CONFLICT` only applies to `INSERT` — but cannot be ruled out without the Jira body.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted.

- [ ] **DSL surface — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala` + `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/T.scala` (Verified)**
  - Add `ON CONFLICT` entry point usable in the `for`-comprehension after `into`. Options (pick one, document choice):
    - Chain on `InsertValues`: `into(p).onConflictDoNothing` / `into(p).onConflictDoUpdate(_.tableNPK)` — analogous to `set(_.field := value)` for updates, OR
    - Top-level step: `onConflict` / `onConflictDoNothing` / `onConflictDoUpdate` as a new `T.OnConflict` / `T.Partial.OnConflict` step (similar to `where`, `orderBy`, `limit`, `offset`, `join`), OR
    - Helper in `Q`: `Q.onConflict[A](_.col)` returning a clause builder with `doNothing`/`doUpdate`.
  - Support conflict-target selector: `onConflict(_.field)` for single column, `onConflict(_.col1, _.col2)` for composite, `onConflict(_.tablePK)` for PK shorthand, and `onConflictConstraint("uq_name")` for named constraint. If unspecified, default to PK columns (mirrors `TableCompanion.scala:30` `s"ON CONFLICT (${pkCols.map(_.name).mkString(", ")})"`).
  - Support action: `DO NOTHING` vs `DO UPDATE SET ...`. For `DO UPDATE`, need `EXCLUDED` ref syntax: `excluded(_.field)` or implicit `SET <non-pk> = EXCLUDED.<non-pk>` shorthand. Add `T.Excluded[A]` or similar for `EXCLUDED.<col>` references.
  - For `Q.insert.fromSelect[A]`, same `onConflict` step applies after `into(...)` that takes the `SELECT` subquery.
  - Verified: `Q.scala:27-52` defines `insert`, `insert.fromSelect`, `update`, `delete`, `where`, `join`, `leftJoin` — no `onConflict`; `T.scala:161` TODO notes type safety gap — new `OnConflict` types would address it.

- [ ] **New part — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/OnConflictPart.scala` (new file, Inferred)**
  - Define `OnConflictPart { target: ConflictTarget, action: ConflictAction }` where:
    - `target = TargetColumns(cols: List[Column]) | TargetConstraint(name: String) | TargetPK` (default),
    - `action = DoNothing | DoUpdate(setExprs: List[SetExprLike])` with optional `where: Option[QueryExpr]` for `DO UPDATE ... WHERE`.
  - If `WHERE` predicate on conflict target (`ON CONFLICT (...) WHERE ...`) is deemed in scope, add `targetWhere: Option[QueryExpr]`.
  - Provide `MapChainParser[OnConflictPart]` that matches the chosen DSL term shape (e.g., `Select(Ident("onConflict"), ...)` or `Apply(Select(Ident("doNothing"), ...))`). Mirror `SetPart.parse` / `WherePart.parse` precedent for how `Term` shape is matched.

- [ ] **Partial query — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/PartialQuery.scala` (Verified)**
  - Extend `PartialQuery.InsertQuery.Basic` and `PartialQuery.InsertQuery.FromSelect` to carry `onConflict: Option[OnConflictPart]`:
    ```scala
    final case class Basic(insert: InsertPart.Basic, into: IntoPart, onConflict: Option[OnConflictPart]) extends InsertQuery
    final case class FromSelect(insert: InsertPart.FromSelect, select: PartialQuery.SelectQuery, into: IntoPart.FromSelect, onConflict: Option[OnConflictPart]) extends InsertQuery
    ```
  - Update `parser` chains: `InsertPart.Basic.withContext("Insert") >>> IntoPart.withContext("Into") >>> OnConflictPart.maybe.withContext("OnConflict")`. For `FromSelect`, the conflict clause comes after `into`: `InsertPart.FromSelect >>> SelectQuery >>> IntoPart.FromSelect >>> OnConflictPart.maybe`.
  - Verified: current `PartialQuery.InsertQuery.Basic` is `InsertPart.Basic >>> IntoPart` with no `onConflict`; `SelectQuery`/`UpdateQuery` show `maybe` pattern for optional parts (`WherePart.maybe` etc.).

- [ ] **Parsed query — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/ParsedQuery.scala` (Verified)**
  - Add `onConflict: Option[OnConflictPart]` to `ParsedQuery.InsertQuery.Basic` and `FromSelect`, include in `allQueryRefs` (conflict target cols + `DO UPDATE` set exprs may reference `EXCLUDED` + optional `WHERE`).
  - Update `toTerm` to build fragment with conflict: after `insertFrag` + `(...columns...)` + `valuesFrag` / `selectFrag`, append `conflictFrag` from `fragmentBuilder.onConflict(insert, onConflict)`.
  - Update `FullQueryResult` mapping (`ParsedQuery.scala:381-385`) to thread `onConflict` through.
  - Verified: `ParsedQuery.InsertQuery.Basic.toTerm` currently assembles `insertFrag` + `columns` + `valuesFrag` + `returningFrag` with no conflict; `makeFragment` for `SelectQuery` shows how optional parts are appended.

- [ ] **SQL generation — `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala` (Verified)**
  - Add `def onConflict(ins: InsertPart, oc: OnConflictPart): ParseResult[GeneratedFragment]` that emits:
    - Target: ` ON CONFLICT (<colNames>)` where `colNames` from `ins.tableRepr` PK (default) or explicit `TargetColumns`/`TargetConstraint`. For constraint: ` ON CONFLICT ON CONSTRAINT <name>`.
    - Target WHERE (if supported): ` WHERE <predFrag>` (emit via `queryExprToFragment` with appropriate `RowRepr` context).
    - Action: ` DO NOTHING` or ` DO UPDATE SET <assignments>` where each assignment is `col = EXCLUDED.col` (for PK-default shorthand) or explicit `col = <expr>` where `<expr>` may reference `EXCLUDED` via new `QueryExpr.ExcludedRef` case.
  - Handle `npkCols.isEmpty` → `DO NOTHING` fallback (same guard as `TableCompanion.scala:37-39`). For `FromSelect` inserts, same logic but `EXCLUDED` refs still valid.
  - Reuse existing `setPart` / `qMarksAndInputEncoderResult` pattern (`FragmentBuilder.scala:419-470`) for how `SET` assignments bind `?` and encoders — `DO UPDATE SET` shares that logic but RHS may be `EXCLUDED.<col>` (no bind) vs input/const.
  - Add `GeneratedFragment` glue: `GeneratedFragment.of(conflictFrag)` appended before `returningFrag`.
  - Verified: `FragmentBuilder` currently has `insert`, `values`, `select`, `join`, `where`, `set`, `limit`, `offset` but no `onConflict`; `values` shows how `IntoPart` is turned into `VALUES (...)`; `setPart` shows `EXCLUDED` vs input handling needed.

- [ ] **QueryExpr — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/QueryExpr.scala` + `modules/sql/core/src/main/scala/oxygen/sql/generic/parsing/RawQueryExpr.scala` + `BinOp.scala` (Inferred)**
  - If `excluded(col)` syntax is chosen, add `RawQueryExpr.Excluded` / `QueryExpr.ExcludedRef` that parses `excluded(p.field)` or `EXCLUDED.field` and lowers to `GeneratedFragment.sql("EXCLUDED." + colName)` with no encoder (similar to `QueryVariableReferenceLike` but for `EXCLUDED` pseudo-table).
  - No `BinOp` change needed — `SET col = EXCLUDED.col` is handled via `SetPart.SetExpr`, not `BinOp.Comp`. Verify `BinOp.Comp` still covers any `WHERE` predicates inside `ON CONFLICT ... WHERE` or `DO UPDATE ... WHERE`.
  - Verified: current `QueryExpr` has `QueryVariableReferenceLike`, `InputVariableReferenceLike`, `Binary`, `BuiltIn`, `Composite` — no `Excluded` case; `RawQueryExpr` parsing for built-ins like `OptionNullability` (`RawQueryExpr.scala:205-206`) shows how to add a new built-in term shape.

- [ ] **Encoding / type support — `modules/sql/schema/RowRepr.scala` + `TableRepr.scala` (Verified — no change expected)**
  - No new `InputEncoder` needed — `ON CONFLICT` adds no bind params beyond existing `into(p)` values (and optional `WHERE` predicate inputs). Derive column names from `ins.tableRepr.rowRepr.columns.columns` and `ins.tableRepr.pk.rowRepr.columns` for default target.
  - If supporting `ON CONFLICT (indexed_expr)` like `ON CONFLICT (lower(email))`, would need `QueryExpr` for target expression — defer.
  - Verified: `TableCompanion.upsert:25-26` already derives `pkCols`/`npkCols` from `tableRepr.pk`/`tableRepr.npk`.

- [ ] **Batch layer — `modules/sql/core/src/main/scala/oxygen/sql/query/BatchOptimizedInsert.scala` (Verified — no code change expected, but verify)**
  - `BatchOptimizedInsert.unsafeParse` slices `insert.ctx.sql` into `SqlParts` for `INSERT ... VALUES (...)` multi-row expansion. Appending ` ON CONFLICT ...` suffix does not affect the `VALUES` slice point, but verify `SqlParts.toQueryOfSize(numValues)` still works when `onConflict` suffix is present (it should — suffix is after `VALUES`).
  - Add test: `BatchOptimizedInsert.unsafeParse(upsertQuery).builtQuery.ofNumValues(3)` produces `INSERT INTO t (...) VALUES (...) , (...) , (...) ON CONFLICT ... DO ...`.
  - Verified: `BatchOptimizedInsert.scala:23-35` derives `maxAllowedValues` and `sqlParts.toQueryOfSize` — suffix not yet tested.

- [ ] **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala` + `modules/sql/it-test/src/test/scala/oxygen/sql/TableCompanionQuerySpec.scala` + `modules/sql/core/src/test` (Verified — patterns exist)**
  - Compile-time: `@compile(debug = true)` queries that use `onConflict` compile and snapshot expected SQL:
    - `Q.insert[Person] + into(p) + onConflictDoNothing` → `INSERT INTO "person" (...) VALUES (...) ON CONFLICT ("id") DO NOTHING`
    - `Q.insert[Person] + into(p) + onConflictDoUpdate` → `... ON CONFLICT ("id") DO UPDATE SET "first" = EXCLUDED."first", ...`
    - Explicit target: `onConflict(_.email) doNothing` → `ON CONFLICT ("email") DO NOTHING`
    - Composite PK: `onConflict(_.id1, _.id2)` for `MultiPK1`.
    - `Q.insert.fromSelect[Note] + onConflictDoNothing`.
  - Integration (via `DbMigrationSpec` + `PostgresTestContainer` under `modules/sql/test-utils`):
    - Insert row, insert duplicate PK → `DO NOTHING` returns 0 `updated`, row unchanged; `DO UPDATE` overwrites non-PK and returns 1.
    - `insert.fromSelect` + `ON CONFLICT DO NOTHING` with mixed new/duplicate source rows.
    - Batch `insertOrDoNothing` equivalence check: hand-written `ON CONFLICT DO NOTHING` vs `TableCompanion.insertOrDoNothing` produce same result.
    - Edge: table with no PK (`Ints`, `Arrays`) → `onConflict` should error at macro time ("no PK to derive conflict target") or require explicit target.
    - Edge: `ON CONFLICT DO NOTHING` + `RETURNING` generates `INSERT ... ON CONFLICT DO NOTHING RETURNING ...` and correctly returns 0 rows on conflict.

- [ ] **Docs — `docs/docs/sql/queries.md` + `docs/docs/sql/models.md` (Verified — section exists)**
  - Update DSL vocabulary table (`queries.md:116-129`) with `Q.insert[A]` + `onConflict` rows: `onConflictDoNothing`, `onConflictDoUpdate`, `onConflict(col).doNothing/doUpdate`. Add short example for `Q.insert.fromSelect` + `ON CONFLICT`.
  - Cross-link from `queries.md:27` upsert/insertOrDoNothing table: note that generated CRUD's `upsert`/`insertOrDoNothing` are PK-only sugar; hand-written `ON CONFLICT` supports arbitrary targets and custom `EXCLUDED` `SET` logic.
  - Optionally note `OXY-97` (explicit FK+IDX naming) interaction: `ON CONSTRAINT "uq_..."` form benefits from explicitly named unique constraints.

- [ ] **Out of scope / follow-ups**
  - `ON CONFLICT (expr) WHERE predicate` (partial-index where) and `DO UPDATE SET ... WHERE predicate` — defer unless spec demands; document as not supported.
  - Row-level `ON CONFLICT` for `COPY` / `MERGE` — out of scope.
  - `ON CONFLICT` with `DO UPDATE` + `RETURNING` distinguishing inserted vs updated rows — follow-up.

- **Verified vs. inferred:** Absence of `onConflict`/`OnConflictPart`/`ON CONFLICT` in `InsertPart`/`ParsedQuery`/`PartialQuery`/`FragmentBuilder`/`Q.scala`/`T.scala`/`queries.md` was verified by grep + file reads. That this maps to `INSERT ... ON CONFLICT` with `DO NOTHING`/`DO UPDATE` semantics (vs any other "conflict" meaning like HTTP 409 or transform conflicts) is inferred from Postgres context and `TableCompanion.upsert` precedent — no Jira body or TODO comment was found to lock DSL spelling or exact subset (PK-only vs arbitrary target vs constraint name).

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — lean is 3 if PK-default `DO NOTHING` + `DO UPDATE SET <non-pk>=EXCLUDED.<non-pk>` only; 5 if arbitrary target (`ON CONFLICT (col)` / `ON CONSTRAINT`), explicit `EXCLUDED` refs, and `insert.fromSelect` + batch parity are all required
  - Justification: Touches only the DSL macro + generation path (1 new part file + 3 parsing files + 1 generation file + DSL surface), no migration/schema or storage-layer change, no new module. Pattern is well-established from prior DSL extensions (`count`, `@>`/`<@`, `tablePK`, `insert.fromSelect` — see `InsertPart.scala`, `ParsedQuery.scala`, `FragmentBuilder.scala`). Single focused PR fits in `modules/sql/core`. The 3→5 range hinges on whether arbitrary conflict target + `EXCLUDED` expression support + `WHERE` predicates are included.

- **Autonomy:** 3 / 6 — needs product/design choice before coding
  - Justification: Mechanics are mechanical once DSL spelling and scope are fixed (an agent can implement `ON CONFLICT (...) DO NOTHING/DO UPDATE` expansion autonomously — `TableCompanion.scala` already shows the SQL shape), but 4 blocking design decisions (see Open Questions) about spelling (`onConflictDoNothing` vs `onConflict(_.col).doNothing` vs `into.onConflict`), conflict-target default (PK-only vs explicit), `EXCLUDED` syntax, and `WHERE`-predicate scope risk rework if chosen wrong. A 30-minute human decision on those points would raise autonomy to 5/6.

- **Ambiguity-to-resolve:** 4 / 6 — notable open questions block start
  - Justification: Title is 2 words "`on conflict`" plus "`query dsl`" with no body; code gives no hint about preferred DSL syntax, target selection, or `EXCLUDED` spelling. Five concrete ambiguities below must be resolved or assumed; the implementation cannot be reviewed without agreeing on them. Lightweight clarification (one paragraph confirming value-list `INSERT` scope and the exact extension-method name) would drop this to 1–2.

## Open Questions

1.  **DSL spelling & placement:** Should the user write `into(p).onConflictDoNothing`, `_ <- onConflictDoNothing`, `_ <- onConflict(_.email) doNothing`, `_ <- Q.onConflict[Person](_.email).doNothing`, or `into(p) onConflict (_.email) doUpdate ...`? Choice determines the `Term` shape the macro must match (`Select` + `Apply` after `into` vs standalone step) and whether `onConflict` is an extension on `InsertValues` or a new `T.Partial.OnConflict`. Which name is reserved for potential `merge`/`upsert` sugar?
2.  **Conflict target default:** Should bare `onConflictDoNothing` default to `ON CONFLICT (<pk>)` (mirrors `TableCompanion.scala:30`) or require an explicit column list `onConflict(_.id) doNothing`? For tables with no PK (`Ints`, `Arrays`, `MultiPK2`), should the default error at macro time ("Table has no PK — specify target") or silently omit target (`ON CONFLICT DO NOTHING` without columns — only valid for `ON CONSTRAINT` form)? Composite PK: `ON CONFLICT (id1, id2)` must list all PK columns — confirm.
3.  **`DO UPDATE` SET syntax & `EXCLUDED` refs:** Should `DO UPDATE` be implied as `SET <non-pk> = EXCLUDED.<non-pk>` (zero-arg `doUpdate`, matches `TableCompanion.upsert:32-36`) or explicit `doUpdate(_.first := excluded(_.first), _.last := excluded(_.last))`? How is `EXCLUDED` spelled — `excluded(_.col)`, `EXCLUDED.col`, or `Q.excluded[Person](_.col)`? Must also decide `DO UPDATE` + `WHERE` support.
4.  **Arbitrary target support:** Is `ON CONFLICT (<non-pk cols>)` (unique index on `email`) in scope, or strictly PK? Is `ON CONFLICT ON CONSTRAINT "uq_name"` needed? Should `ON CONFLICT (col) WHERE <pred>` (partial index) be supported, and if so, what DSL for the `WHERE` predicate — `onConflict(_.email) where (_.isActive == true) doNothing`?
5.  **Scope of `on conflict`:** Is this only for `Q.insert[A]` + `Q.insert.fromSelect[A]` (the Postgres-correct scope), or also for `update`/`delete` conflict-like extensions? The issue says "query dsl" — does it include `batchOptimizedInsert` parity so `batchOptimizedInsert.unsafeParse` of a conflicting insert still multi-`VALUES` expands correctly?
6.  **Interaction with existing CRUD:** Should hand-written `ON CONFLICT` subsume `TableCompanion.upsert`/`insertOrDoNothing` (keep them as thin wrappers that could be reimplemented as `@compile` queries) or keep them as separate string-concat impls? If both paths ship, ensure generated SQL is consistent (same quoting, same `EXCLUDED` handling).
7.  **Type restriction & validation:** Should the macro validate that the conflict target columns have a unique constraint/index at compile time (via `TableRepr` index metadata) or defer to Postgres runtime error (`no unique or exclusion constraint matching the ON CONFLICT specification`)? Likely defer — confirm, and decide whether `Option[A]` columns in target are allowed.
