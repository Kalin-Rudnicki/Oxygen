# OXY-13 — Support automatic join clauses

## Original
- **Key:** OXY-13
- **Checklist line:** `- [ ] [OXY-13](https://kr-oxygen.atlassian.net/browse/OXY-13) — **Task** · Low — Support automatic join clauses`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Support automatic join clauses
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-13
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Add "auto join" / "natural join" support to the `oxygen-sql` compile-time query DSL so that a `JOIN` (or `LEFT JOIN`) can be written **without an explicit `ON` condition** — the `ON` clause is auto-generated from `foreignKey` metadata already present on `TableRepr`.

Today every join requires a manual predicate:

```scala
for {
  p <- select[Person]
  n <- join[Note] if n.personId == p.id   // explicit ON
  _ <- where if p.groupId == i
} yield (p, n)
```

The desired form would infer that predicate from the FK declaration (e.g. `@foreignKey[Note, Person]((_.personId, _.id))`):

```scala
for {
  p <- select[Person]
  n <- join[Note]               // ON automatically from FK
  // or: n <- join[Note].auto / n <- join[Note] if natural
} yield (p, n)
```

This directly matches the only code signal found in the repo — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala:43-44`:

```scala
// TODO (KR) : support auto generated natural joins
// def natural: Boolean = macroOnly
```

That TODO is inside `object Q` next to `def join` / `def leftJoin`, indicating the intended API was contemplated as something like `Q.natural` / `join[A] if natural` or `join[A].natural`. The underlying FK infrastructure already exists and is the enabler: `oxygen.sql.schema.annotations.foreignKey[Current, References]`, `ForeignKeyRepr` / `ForeignKeyRepr.Built` with `columnPairs`, `TableRepr.foreignKeys` / `builtForeignKeys`, derived in `DeriveTableRepr.foreignKeys`, and example usage in `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala:128`.

"Automatic" here is interpreted as **FK-driven**, not SQL `NATURAL JOIN` (join on all same-named columns) — the latter is rare in Postgres and has no FK awareness. FK-driven auto-join is the common ORM/DSL pattern and matches the repo's own FK modeling. The two are related but distinct; the TODO's wording "natural joins" likely uses "natural" loosely to mean "the natural FK relationship."

**Who it affects:** Authors of hand-written queries in `oxygen-sql` (`modules/sql/core` DSL, `docs/docs/sql/queries.md`). Reduces boilerplate and copy-paste errors for the common case where the join condition is exactly the FK. Low priority suggests it is ergonomic, not blocking.

**Why it matters:** FK-based auto-joins make the DSL more declarative, keep join logic DRY with the schema definition, and prevent drift where the schema's FK and the query's `ON` clause diverge. For tables with composite FKs (e.g. `MultiPK2 -> MultiPK1` in the test fixtures) the auto-generated `ON` would correctly emit a multi-column `AND` without the author enumerating each column.

**Inferred acceptance criteria:**
1. A query can `join[A]` / `leftJoin[A]` without a manual `if <cond>` and still compile, with the `ON` clause derived from a `foreignKey` between the joined table and a table already in scope (the preceding `select` or an earlier join).
2. Composite FKs produce `col1 = col1 AND col2 = col2`; `leftJoin` variant works and yields `Option[A]` as today.
3. Explicit `if <cond>` syntax remains supported and takes precedence; mixing auto and manual joins in one query works.
4. Ambiguous case (zero or >1 applicable FKs between the two tables) fails at compile time with a clear error (via `report.errorAndAbort`).
5. Generated SQL is verified (e.g. `JOIN "note" "n" ON "n"."person_id" = "p"."id"`), and `docs/docs/sql/queries.md` DSL vocabulary is updated.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Direct code signal: `Q.scala:43-44` TODO says "support auto generated natural joins" — phrase matches the issue title "Support automatic join clauses" almost verbatim, and sits adjacent to `join`/`leftJoin` definitions. No other `auto join` / `natural join` mention exists in the repo (grep confirmed).
  - FK infrastructure is already modeled end-to-end (`ForeignKeyRepr`, `TableRepr.foreignKeys`, `DeriveTableRepr`, `@foreignKey`), so the prerequisite for FK-driven auto-joins is present — this is not speculative modeling.
  - No competing interpretation found; sibling issues (OXY-98 lateral join, OXY-15 effect migration, etc.) are distinct and do not overlap.
  - Rating stays at 4 not 5/6 because the exact intended API shape (`if natural` vs bare `join[A]` vs `join[A].auto` vs SQL `NATURAL JOIN`) is not specified and must be inferred, and the Jira body was not fetched — so design ambiguity remains.

## Required Changes

- [ ] **DSL API — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala` + `T.scala`** — add auto-join entry point. Options (pick one after design review):
  - Bare `def join[A](using TableRepr[A]): T.Join[A]` without requiring `withFilter` (most ergonomic), or
  - `def natural: Boolean = macroOnly` as hinted by the TODO (e.g. `join[Note] if natural`), or
  - `def auto[A](using TableRepr[A]): T.Join[A]` explicit variant. Must also cover `leftJoin` symmetry. Update `T.Partial.JoinLike` hierarchy and `T.Join` / `T.LeftJoin` / `T.Partial.Join` / `T.Partial.LeftJoin` to support the no-predicate path.
  - *Verified:* current `T.Partial.Join[A].withFilter` requires a predicate; *Inferred:* new overload/flag needed.

- [ ] **Parser — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/JoinPart.scala`** — extend `JoinPart.parse` to accept the auto form. Today it parses `Q.join[A]` -> `withFilter` -> `map`; auto form needs a branch that skips `withFilter`/`filterExpr` parsing and instead records an `Auto` marker. Will need to resolve which FK to use by inspecting `RefMap` (tables in scope) and `TableRepr.foreignKeys` at macro expansion time (requires `TypeclassExpr.TableRepr` + `TableRepr.Built` access inside `Quotes`).

- [ ] **Model — `JoinPart` + `PartialQuery.SelectQuery/UpdateQuery/DeleteQuery` + `ParsedQuery`** — extend `JoinPart` to represent auto-joins, e.g. `filterExpr: Option[QueryExpr]` or `sealed trait JoinCondition { case Manual(expr); case Auto(fk: ForeignKeyRepr.Built) }`, and `show` to render the resolved `ON`. Alternatively keep `filterExpr` but populate it synthetically during parsing.

- [ ] **SQL generation — `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala:358-365`** — in `def join(j: JoinPart)`, branch on auto vs manual: auto generates `ON "lhs"."col" = "rhs"."col" (AND ...)` from `ForeignKeyRepr.Built.columnPairs`. Must handle qualified column names via `Column` metadata and respect `tableRepr.tableRef` / `mapQueryRef.sqlString` aliasing as the manual path does. Composite FKs emit `AND`-joined equality list.

- [ ] **FK resolution logic (new, small)** — helper that given `joinedTable: TableRepr[A]` and `inScope: List[TableRepr[?]]` (from `RefMap`), finds the single `ForeignKeyRepr.Built` where `fk.self == joinedTable && fk.references == candidate` or vice versa. Must decide directionality (usually joined table FKs to an earlier table), and emit a compile-time error if 0 or >1 matches. Consider composite and multi-FK cases; reuse `TableRepr.builtForeignKeys` (already lazy).

- [ ] **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala` + new `JoinAutoSpec`** — add cases: single-column FK auto join, composite FK auto join (e.g. `MultiPK2 -> MultiPK1`), `leftJoin` auto, mixed auto+manual, ambiguous FK error (compile-fails), no-FK error. Extend `Migration`-aware test fixtures if needed.

- [ ] **Docs — `docs/docs/sql/queries.md`** — update DSL vocabulary table (`join[A] if <cond>` -> also document auto form) and join example to show both manual and auto. Optionally note that auto-join is FK-driven, not SQL `NATURAL JOIN`.

- **Verified vs. inferred:** `Q.scala` TODO, `ForeignKeyRepr`/`TableRepr` FK plumbing, current `JoinPart`/`FragmentBuilder` join handling, and `queries.scala` FK example were verified by reading code. That "automatic" = FK-driven auto-`ON` (not SQL `NATURAL JOIN` keyword) and the specific API spelling are inferred from context and remain to be confirmed.

## Estimates & Autonomy

- **Story points:** 5 (Fibonacci)
  - *Justification:* Pure DSL/macro work confined to `modules/sql/core` — no DB migration, no runtime infra. Moderate complexity: macro parsing branch, FK lookup, SQL generation for composite keys, and error messaging. Comparable to other DSL extensions like `IN` (OXY-17) or `group by` (OXY-100). If ambiguous-FK diagnostics and `leftJoin` symmetry are scoped out, could be 3; if `NATURAL JOIN` keyword support is also required, 8.

- **Autonomy:** 3 / 6 — needs design pairing
  - *Justification:* An agent can implement the FK-lookup + generation once the API shape is chosen, but the API shape (`bare join` vs `if natural` vs `auto`) and FK direction/ambiguity policy require a 1-question product decision before coding. Otherwise isolated to the query macro layer with clear conventions.

- **Ambiguity-to-resolve:** 4 / 6 — moderate-high, blocks start
  - *Justification:* Title gives no API spec; the TODO comment is commented-out pseudocode, not a spec. Must resolve: (a) desired syntax, (b) whether SQL `NATURAL JOIN` keyword is also in scope or only FK-driven `ON`, (c) which FK direction to prefer and how to report 0/ >1 matches, (d) whether `leftJoin` auto is required in v1.

## Open Questions

1. **API syntax:** Should auto-join be `for { p <- select[Person]; n <- join[Note] }` (bare, no `if`), `join[Note] if natural`, `join[Note].auto`, or a new `Q.autoJoin[Note]`? The TODO hints at `def natural: Boolean` but may be stale — needs PO confirmation.
2. **FK vs SQL NATURAL JOIN:** Is the goal FK-driven `ON col = col` (recommended) or literal `NATURAL JOIN` / `NATURAL LEFT JOIN` SQL? The two have different semantics (NATURAL joins on all same-named columns, no FK needed).
3. **FK direction & scope:** When `Note` FKs to `Person`, should `join[Note]` from `select[Person]` resolve? What about the reverse (`select[Note]; join[Person]` where FK is on the already-joined table)? Should only FKs declared on the joined table count, or also FKs on in-scope tables pointing to the joined table?
4. **Ambiguity policy:** If two tables have zero FKs between them, or two distinct FKs (e.g. `authorId` and `reviewerId` both FK to `Person`), should the compiler error, or should the user be required to specify which FK / fall back to manual `if`? What error message is desired?
5. **Composite & multi-column FKs:** Confirm that auto-generating `col1 = col1 AND col2 = col2` for composite PKs is sufficient, and whether FKs that reference non-PK unique columns (if any) should be supported.
6. **Update/Delete joins:** `PartialQuery.UpdateQuery` and `DeleteQuery` also carry `joins: List[JoinPart]` — should auto-join be supported there as well, or only for `SELECT`?
