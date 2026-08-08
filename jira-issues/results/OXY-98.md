# OXY-98 — Add support for `lateral join` + `union all` + `sparse data` + `zstream agg` in order to query nested data structures

## Original
- **Key:** OXY-98
- **Checklist line:** `- [ ] [OXY-98](https://kr-oxygen.atlassian.net/browse/OXY-98) — **Task** · Higher — Add support for \`lateral join\` + \`union all\` + \`sparse data\` + \`zstream agg\` in order to query nested data structures`
- **Type:** Task
- **Priority:** Higher
- **Title (verbatim):** Add support for `lateral join` + `union all` + `sparse data` + `zstream agg` in order to query nested data structures
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-98
- **Checklist section:** To Do
- **Epic (inferred):** [OXY-1](https://kr-oxygen.atlassian.net/browse/OXY-1) — **Epic** · Normal — oxygen-sql (owning module `oxygen-sql` per `modules/sql/*` and `build.sbt`; all query DSL work in `modules/sql/core/src/main/scala/oxygen/sql/...` falls under it; no explicit epic link in checklist title — association inferred from DSL keywords)
- **Siblings (inferred):** OXY-13 Task · Low — Support automatic join clauses; OXY-14 Task · Normal — Add support for querying into a JSONB structure (alternative nested-data strategy); OXY-6 Task · Low — Add support for array input + unnest; OXY-17 Task · High — Add support for IN; OXY-94 Task · Lower — Add `on conflict` support; OXY-100 Task · Lowest — Add query support for `group by`

## Expanded Description

**What this issue likely is:** A compound `oxygen-sql` query-DSL extension that adds four coordinated features so a single compiled query (`@compile` / `QueryO.compile` / `QueryIO.compile`) can fetch an entire **nested/hierarchical domain graph** in one round-trip and stream it into domain objects without N+1 queries or manual flat-row deduplication. The four pieces are not independent features — they form one pipeline for sparse-row nested fetching:

1. **`lateral join` (Postgres `LATERAL`):** Allow a `JOIN` whose right-hand side is a correlated sub-select that can reference columns of the preceding `FROM` item, i.e. `FROM person p, LATERAL (SELECT * FROM note n WHERE n.person_id = p.id ORDER BY n.created_at LIMIT N) AS notes` or `LEFT JOIN LATERAL (...) ON true`. This is the SQL primitive that makes per-parent sub-selection efficient without a cartesian `GROUP BY` over all parent columns and that composes with `LIMIT`/`ORDER BY` per parent (e.g. "top 3 notes per person"). Today's DSL in `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala:40-41` only has `join[A] if cond` / `leftJoin[A] if cond` (inner/left) bound to `JoinPart.JoinType.{Inner, LeftOuter}` (`modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/JoinPart.scala:33-45`) and `FragmentBuilder.join` (`modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala:358-371`) which emits `JOIN <table> ... ON ...` — no `LATERAL`, no subquery.

2. **`union all`:** Allow combining two or more `SELECT` queries with `UNION ALL` into a single result stream, with aligned (null-padded) columns so different logical row types can share one result set. Typical pattern for nested data: `SELECT p.* , NULL::note_columns FROM person p WHERE ... UNION ALL SELECT NULL::person_columns, n.* FROM note n WHERE ... ORDER BY sort_key` — then the DB driver returns one `ZStream` of rows where each row is sparse (only one side populated). Today `ParsedQuery.SelectQuery` (`modules/sql/core/src/main/scala/oxygen/sql/generic/model/ParsedQuery.scala:165-244`) models a single `SELECT ... FROM ...` with `joins/where/orderBy/limit/offset/ret` and no `UNION` — verified by grep: zero hits for `UNION` in `modules/sql/**/*.scala` and `docs/docs/sql/queries.md`.

3. **`sparse data`:** The row model that `UNION ALL` (and `LEFT JOIN` with null-padded projections) produces: a single SQL row type that is the outer union of multiple table projections, where for any given physical row only a subset of columns is non-NULL (e.g. `(Person, Option[Note], Option[Address])` where each `UNION ALL` leg populates a different subset). The title's `sparse data` implies first-class DSL/decoder support for decoding such nullable-column rows as `Option[A]` product fields and for building them with null-padding on the SQL side. `Option[A]` columns already exist (`Option`-wrapped `RowRepr` / `RowRepr.optional`) and `QueryExpr.QueryVariableReferenceLike` handles optional refs, but there is no DSL helper for "select sparse tuple `(Option[Person], Option[Note])` as union legs".

4. **`zstream agg` / `SparseStreamAggregator`:** The app-side stream aggregation that turns the flat sparse `ZStream[Row]` into hierarchical domain objects (`PersonWithNotes(notes: Seq[Note])`, `DepartmentWithEmployeesWithProjects`, etc.) *without* buffering the whole result in memory. `modules/general/zio/src/main/scala/oxygen/zio/SparseStreamAggregator.scala:9-82` already implements this: leaf `SparseStreamAggregator.of[A]` for `Option[A] -> A`, plus combinators `optional`, `many[S]`, `*:` (AndThen/zip) for ` (Option[A], Option[B]) -> (A, Option[B])` style aggregation, with `toPipeline` / `aggregateStream` (`ZPipeline`/`ZStream`) and `stream.agg(agg)` syntax (`modules/general/zio/src/main/scala/oxygen/zio/syntax/stream.scala:9`) and `QueryO.>>>` / `QueryIO.>>>` / `Returning.>>>` already wired (`modules/sql/core/src/main/scala/oxygen/sql/query/[query.scala:192,263,325,401`](modules/sql/core/src/main/scala/oxygen/sql/query/query.scala:192), `QueryResult.scala:48`). `StreamAggregatorSpec` verifies the combinators but no `it-test` demonstrates `UNION ALL` + sparse decode + `agg` end-to-end, and `docs/docs/sql/queries.md` does not mention any of the four features.

**Concrete motivating examples (inferred desired DSL — exact spelling TBD, see Open Questions):**

```scala
// Example 1 — per-parent lateral join (top N children per parent)
@compile
val personsWithTopNotes: QueryO[(Person, Option[Note])] =
  for {
    p <- select[Person]
    n <- lateralLeftJoin[Note] if n.personId == p.id // emits: FROM person p LEFT JOIN LATERAL (SELECT * FROM note n WHERE n.person_id = p.id ORDER BY n.created DESC LIMIT 3) AS n ON true
  } yield (p, n)
// then: personsWithTopNotes >>> (SparseStreamAggregator.of[Person] *: SparseStreamAggregator.of[Note].many[List].optional) // PersonWithNotes

// Example 2 — union-all sparse fetch for parent + two child types in one round-trip
@compile
val deptHierarchy: QueryO[(Option[Department], Option[Employee], Option[Project])] =
  ( for { d <- select[Department] } yield (d.some, None, None) )
    .unionAll( for { e <- select[Employee]; d <- join[Department] if e.deptId == d.id } yield (None, e.some, None) )
    .unionAll( for { p <- select[Project]; e <- join[Employee] if p.ownerId == e.id } yield (None, None, p.some) )
    .orderBy(_.deptId.asc) // or stable sort key
// then: deptHierarchy.stream.agg( SparseStreamAggregator.of[Department] *: SparseStreamAggregator.of[Employee].many[List] *: SparseStreamAggregator.of[Project].many[List] )
```

**Who it affects:** Any service that reads object graphs (1-N, N-M, tree) via `oxygen-sql` and today must choose between N+1 queries (loop over parent IDs), massive duplicate-parent flat joins (`SELECT p.*, n.* FROM person p JOIN note n ...` with parent-column duplication), or the JSONB alternative of OXY-14 (`jsonb_agg`/`jsonb_build_object` pushed into Postgres). OXY-98 is the **app-side sparse-stream alternative** to OXY-14's DB-side JSON aggregation: keep rows relational, stream them lazily via `ZStream`, aggregate with `SparseStreamAggregator`.

**Why it matters (Higher priority):** Nested fetching is the primary completeness gap for the SQL DSL. The current DSL can only express flat joins; without `LATERAL` it cannot do per-parent `LIMIT`/`ORDER BY`; without `UNION ALL` it cannot fetch heterogeneous subtrees in one query; without a sparse row model + `ZStream` aggregation, callers must hand-roll deduplication or buffer. The Higher priority (vs Low/Lower for sibling polish like OXY-6/13/94) signals this is blocking real feature work and is the planned "correct" path for nested queries alongside OXY-14 (Normal) as the complementary JSON path.

**Inferred acceptance criteria:**

1. **Lateral join DSL** — at least `lateralJoin[A]` and `lateralLeftJoin[A]` (or reuse `join`/`leftJoin` with a `lateral` modifier) usable in a `for`-comprehension where the right-hand side's filter can reference earlier `select`/`join` bindings; generated SQL contains `JOIN LATERAL (...)` / `LEFT JOIN LATERAL (...) ON true` (or `ON cond`) with correct correlation.
2. **Union all DSL** — a combinator to union two or more `QueryO`/`QueryIO` with `UNION ALL` and column alignment (null-padding with correct casts so all legs share one `RowRepr`); requires adding a `ParsedQuery.UnionQuery` or similar and correct `ResultDecoder` composition. `UNION` (distinct) may be deferred if spec says `union all` only.
3. **Sparse data handling** — the `SELECT` projection can be a sparse tuple of `Option[A]` columns (e.g. `(Option[Person], Option[Note])`) that correctly round-trips through `DecoderBuilder`/`ResultDecoder` with nullable columns; at minimum covers the union-all legs above and lateral left joins yielding `Option[Child]`.
4. **ZStream agg wiring + example** — an `it-test` that inserts a parent + N children (+ grand-children for union case), executes the lateral/union query as a `ZStream`, aggregates via `>>>` / `.agg(SparseStreamAggregator.of[...].many...)` into a nested domain type, and asserts ordering, empty-children handling, and streaming (not buffered) behavior. At least one test per sub-feature.
5. **Streaming correctness** — lateral/union queries preserve the existing `stream` / `streamWithFetchSize` / `Returning.>>>` / `QueryO.>>>` pipeline semantics, do not break `orderBy`/`limit`/`offset`/`where` composition, and generated SQL is visible via `@compile(debug=true)` / `ParseContext` diagnostics.
6. **Docs** — `docs/docs/sql/queries.md` (DSL vocabulary table and join/union/sparse examples) and/or a new `docs/docs/sql/nested-queries.md` documenting the pattern, when to choose lateral vs. flat join vs. union-all vs. OXY-14 JSONB, and the `SparseStreamAggregator` recipe.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner

**Justification:**

- **Title is unusually explicit.** Four comma-separated keywords plus a purpose clause ("in order to query nested data structures") map 1:1 to known Postgres constructs (`LATERAL` for per-row subqueries, `UNION ALL` for combining row types, sparse/null-padded rows for heterogeneous unions, and the repo's existing `SparseStreamAggregator` for stream aggregation). No alternative reading fits as well — e.g. "sparse data" is not a standard SQL term elsewhere in the repo, and "zstream agg" literally names `SparseStreamAggregator` (`modules/general/zio/src/main/scala/oxygen/zio/SparseStreamAggregator.scala:79`) and its `>>>` integration on `QueryO`/`Returning`.
- **Code signal is strong and verified.** `SparseStreamAggregator` + `QueryO.>>>` + `Returning.>>>` + `stream.agg` + `StreamAggregatorSpec` were verified present and tested in isolation (leaf/optional/many/*: combinators). Conversely, verified *absent* are any `LATERAL` or `UNION` handling: zero hits for `union`/`lateral` in `modules/sql/**/*.scala` and `docs/docs/sql/queries.md`, `JoinPart` only has `Inner`/`LeftOuter` and `FragmentBuilder.join` only emits `JOIN`/`LEFT JOIN`, `ParsedQuery.SelectQuery` has no union node, and `Q.scala`/`T.scala` have no lateral/union DSL — confirming the gap is exactly what the title names.
- **Sibling signal corroborates.** OXY-14 ("querying into a JSONB structure", Normal) is the DB-side alternative for the same nested-data problem; the checklist note in OXY-14's triage explicitly calls OXY-98 the "app-side `SparseStreamAggregator`" approach. No other To Do issue claims lateral/union/sparse/zstream scope (OXY-13 is auto-joins, OXY-100 is `group by`, OXY-6 is `array+unnest`), so no competing ownership.
- **Why not 5/6:** No skipped test, no `TODO (KR)` marking the exact syntax, and no Jira body/design doc was retrievable, so the *chosen DSL spelling* (standalone `lateralJoin[A]` vs. modifier on `join`, `unionAll` as method on `QueryO` vs. free function `Q.unionAll`, sparse tuple as `(Option[A], Option[B])` vs. dedicated `SparseRow` wrapper, and whether deeper nesting/combinator generation is macro-derived or hand-written) is inferred, not copied from a spec. Priority Higher and Epic OXY-1 module hint are confident but still indirect.

## Required Changes (only if Confidence >= 3)

> All paths repo-grounded; mark verified vs. inferred.

**Verified present (no change needed):**

- [x] `modules/general/zio/src/main/scala/oxygen/zio/SparseStreamAggregator.scala` — full `SparseStreamAggregator` trait + `Leaf`/`Optional`/`Many`/`AndThen` + `toPipeline`/`aggregateStream` + `*:` combinator — verified
- [x] `modules/general/zio/src/main/scala/oxygen/zio/syntax/stream.scala:9` — `ZStream.agg(agg)` extension — verified
- [x] `modules/sql/core/src/main/scala/oxygen/sql/query/[query.scala:192,325](modules/sql/core/src/main/scala/oxygen/sql/query/query.scala:192)` + `QueryResult.scala:48` — `QueryO.>>>` / `QueryIO.>>>` / `Returning.>>>` sparse-agg wiring ( `>>> SparseStreamAggregator` / `>>> ZPipeline` ) — verified
- [x] `modules/general/zio/src/test/.../StreamAggregatorSpec` (located via `find modules -name StreamAggregatorSpec.scala`) — `single`/`optional`/`many`/`aOptB`/`mega` specs verifying combinators in isolation — verified
- [x] `modules/sql/core/src/main/scala/oxygen/sql/model/json.scala` + `RowRepr.typedJsonb` path for JSONB alternative (OXY-14's complementary track) — verified

**To implement (repo-grounded):**

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala` — add DSL entry points (inferred spelling, pick one and keep consistent):
  ```scala
  // Option A — explicit lateral joins (most Postgres-faithful)
  def lateralJoin[A](using t: TableRepr[A]): T.Partial.LateralJoin[A] = macroOnly
  def lateralLeftJoin[A](using t: TableRepr[A]): T.Partial.LateralLeftJoin[A] = macroOnly
  // Option B — free function for lateral subqueries: Q.lateral(q: QueryO[A]): T.Lateral[A]
  // Keep existing join/leftJoin untouched for non-lateral path (backwards compat).
  // Alt considered: `Q.lateral(cond).join[A] if ...` modifier — reject unless spec prefers it.
  ```
  *Inferred — verify against `Q.scala:1-74` current vocabulary (`select`, `join`, `leftJoin`, `where`, `orderBy`, `limit`, `offset`, `insert`, `update`, `delete`, `count`, `mkSqlString`).*

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/T.scala` — add corresponding `T.LateralJoin` / `T.LateralLeftJoin` types and their `Partial.LateralJoin[A]` / `Partial.LateralLeftJoin[A]` targets with `withFilter` (or direct `map`/`flatMap`) mirroring `T.Partial.Join[A]` (`T.scala:139`). Also add union types if DSL is comprehension-based:
  ```scala
  // union on QueryO/QueryIO (alternative: free function Q.unionAll)
  final class UnionOps[O] private { def unionAll(that: QueryO[O]): QueryO[O] = macroOnly }
  extension [O](q: QueryO[O]) def unionAll(that: QueryO[O]): QueryO[O] = macroOnly
  ```
  *Inferred — `T.scala:161` notes type-safety could be stricter; this is where union lateral type sig lives.*

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/JoinPart.scala` — extend `JoinPart.JoinType` with `LateralInner` / `LateralLeft` (or new `LateralJoinPart` if `LATERAL (SELECT ...)` correlation differs from simple `JOIN table ON cond`). Update `JoinPart.parse` (`JoinPart.scala:36-64`) to detect `Q.lateralJoin` / `Q.lateralLeftJoin` via `AppliedAnonFunctCall.parseTyped` quoting, resolve the lateral subquery's `RefMap` correlation (right side may reference left `VariableReference.FromQuery` bindings — unlike current `JoinPart` which adds both refs independently), and produce a `LateralJoinPart` that holds the subquery `ParsedQuery` or `QueryExpr` for generation. *Verified gap: `JoinPart` currently parses only `Q.join`/`Q.leftJoin` and assumes `tableRepr.tableRef` not a subquery.*
  - [ ] Decide: lateral leg is `LATERAL (SELECT ... WHERE n.person_id = p.id)` (sub-select with correlation) vs. `LATERAL (SELECT ... FROM note ...)` as a derived lateral table — first is sufficient for the motivating case and reuses `SelectQuery` parsing.

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/model/ParsedQuery.scala` — add union support. Minimal v1:
  ```scala
  // new top-level node (or extend SelectQuery with unions: List[SelectQuery])
  final case class UnionQuery(legs: NonEmptyList[SelectQuery], orderBy: Option[OrderByPart], limit: Option[LimitPart], offset: Option[OffsetPart]) extends ParsedQuery
  // or: SelectQuery.unions: List[SelectQuery]
  ```
  Update `ParsedQuery.parse` to detect `unionAll` chaining (macro produces a `Term` that encodes multiple selects) and map to `UnionQuery`. Ensure `allQueryRefs` / `refs` union correctly across legs and that `toTerm` builds a single `QueryContext` with one `ResultDecoder` (all legs must project the same `O` / same nullable column shape). *Verified: `ParsedQuery.SelectQuery` is single-select today (`ParsedQuery.scala:165-175`); `ParsedQuery.InsertQuery.FromSelect` already holds a nested `SelectQuery` — union reuses that pattern.*

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala` — emit new SQL:
  - [ ] `join` for lateral: ` "\n    JOIN LATERAL (" + subqueryFrag + ") AS " + alias + " ON " + onFrag` and `LEFT JOIN LATERAL` variant (`FragmentBuilder.scala:358-371` is the template; add `LateralJoinPart` branch).
  - [ ] `unionAll` for union: `GeneratedFragment.flatten(legs.map(_.makeFragment).intersperse(GeneratedFragment.sql("\nUNION ALL\n")))` plus trailing `orderBy`/`limit`/`offset` on the whole union (Postgres requires outer `ORDER BY` after the last leg, or per-leg ordering inside each `SELECT` — decide and document).
  - [ ] Sparse null-padding: when a union leg projects `(Option[Person], Option[Note])` but one leg only has `Person`, the missing side must be `NULL::type` with correct casts so Postgres can infer the column type. This is generated via `NULL` fragments with `:: <colType>` or via `GeneratedFragment.sql("NULL")` plus `InputEncoder` handling of `None` decode — verify decoder can handle all-NULL legs ( `RowRepr.optional` already nullable; `DecoderBuilder` must allow `None` for missing side).

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/DecoderBuilder.scala` / `GeneratedResultDecoder.scala` — ensure sparse tuple `(Option[A], Option[B])` decoding is sound for union/lateral streams: each `VariableReference.FromQuery` that is `optional` (from `lateralLeftJoin` / union null leg) is decoded as `Option[Decoded]` via `ResultDecoder.optional`; collection side (`many`) is left to `SparseStreamAggregator` (not decoder). Add `ResultDecoder.Sparse` awareness if needed for `ZStream agg` type `O` shape `(Option[Parent], Option[Child])`.

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/GeneratedFragment.scala` / `GeneratedSql.scala` — if union fragments need indented sub-fragments per leg, reuse `GeneratedFragment.indented` / `GeneratedSql.indented` (`FragmentBuilder.scala:301-307` precedent for `SelectPart.FromSubQuery`).

- [ ] Parsing — `modules/sql/core/src/main/scala/oxygen/sql/generic/parsing/{RawQueryExpr.scala, QueryExpr.scala, AppliedAnonFunctCall.scala, Function.scala, PartialQuery.scala, PartialQueryParsers.scala, MapChainParser.scala}` — add cases:
  - [ ] Lateral join anon-funct call detection parallel to `JoinPart.parse`'s `Q.join`/`Q.leftJoin` quoting but for the new `Q.lateralJoin` symbols; handle correlation scope (lateral subquery's `RefMap` must see the outer `select`'s `mapQueryRef` — current `JoinPart.parse` adds both refs fresh, so lateral needs ordered ref extension).
  - [ ] Union `Term` detection — the macro term for `q1.unionAll(q2)` must be intercepted before `ParsedQuery.SelectQuery.fullParser` consumes it; likely add a `UnionQuery.parser: Parser[Term, UnionQuery]` at the `ParsedQuery` top level with higher priority than `SelectQuery`.

- [ ] `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/annotations.scala` / `CompileMacros.scala` / `QueryContext.scala` — confirm `QueryContext.QueryType` still `Select` for union/lateral selects (or add `Union` if metrics/labels need it), and that `@compile` on `val x: QueryO[...] = for { ... } yield ... unionAll ...` still routes through `CompileMacros.query` / `ParsedQuery.compile` (`ParsedQuery.scala:402`).

- [ ] **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/`** (existing `TableCompanionQuerySpec`, `CustomQuerySpec`, `IsolationAspectSpec` pattern):
  - [ ] `LateralJoinSpec` / `NestedQuerySpec` — insert `Person` + N `Note`s per person, execute `@compile val q: QueryO[(Person, Option[Note])] = for { p <- select[Person]; n <- lateralJoin[Note] if n.personId == p.id } yield (p, n)` with `ORDER BY`/`LIMIT` per child, assert per-parent top-N, plus `lateralLeftJoin` empty-children case.
  - [ ] `UnionAllSparseSpec` — build a `UNION ALL` query yielding `(Option[Department], Option[Employee], Option[Project])`, execute as `ZStream`, pipe through `SparseStreamAggregator.of[Department] *: SparseStreamAggregator.of[Employee].many[Seq] *: SparseStreamAggregator.of[Project].many[Seq]`, assert hierarchy matches inserted graph and that `orderBy` on sort key is stable across legs.
  - [ ] Edge tests: lateral + `where` + `orderBy` + `limit`/`offset` composition, union with `input[I]` params, `input.optional` interaction with sparse decode, empty result (`0` rows) yields empty `ZStream` (not failure).
  - [ ] Unit tests for parsing/generation error messages: mis-aligned union legs (different column counts / incompatible `RowRepr`s) produce a clear `ParseResult.error` at compile time.

- [ ] **Docs — `docs/docs/sql/queries.md`** — extend DSL vocabulary table with `lateralJoin[A] if cond` / `lateralLeftJoin[A] if cond` / `unionAll` + add a dedicated "Querying nested data structures (sparse + ZStream agg)" section with:
  - When to choose flat join vs. lateral vs. union-all sparse vs. OXY-14 JSONB
  - The sparse row model (`Option` tuples, null-padding)
  - The `SparseStreamAggregator` recipe (`of`, `optional`, `many`, `*:`, `>>>` / `.agg`, `stream` vs `chunk`)
  - Ordering caveat for `Union All` sparse streams (aggregator is order-sensitive — see Open Questions)

- [ ] **Examples — `example/apps/web-server`** (if repo wants a runnable nested-query example):
  - [ ] Add one `Note`-per-`Person` lateral demo or a small 3-table hierarchy demo using the new combinators, showing the `QueryO >>> agg` form alongside the existing `PostApiImpl` / `UserApiImpl` patterns.

**Out of scope (but note):**

- Full `GROUP BY` support (OXY-100) — not required for v1 lateral/union/sparse; lateral sub-select + `LIMIT` avoids the `GROUP BY` explosion, and union aggregation is app-side (`many`), not SQL `GROUP BY`.
- `array input + unnest` (OXY-6) and `IN` (OXY-17) — orthogonal collection predicates; should not be bundled into OXY-98's change set.
- Transparent `auto-volume` nesting derivation (auto-`yield` of nested case classes without explicit sparse tuple + manual `SparseStreamAggregator` wiring) — defer to follow-up; v1 keeps explicit `yield (p.some, n.some)` + explicit `agg` construction.

**Verified vs. inferred:** Presence of `SparseStreamAggregator` + `>>>` wiring + `StreamAggregatorSpec` was verified by file reads and `grep -rn` zero-hit verification for `LATERAL`/`UNION` in `modules/sql` and `docs/docs/sql/queries.md`. That the intended feature is the specific 4-step Postgres+ZStream pipeline for nested graphs (and not, e.g., a research spike or a metrics/zipkin task) is the title's verbatim reading — strong but still an interpretation since no Jira body or `TODO` comment was retrieved to lock the exact DSL spelling.

## Estimates & Autonomy

- **Story points:** 13 (Fibonacci) — extra-large / borderline Epic. Larger than OXY-14 (5, single JSONB projection), OXY-6 (5, array+unnest), or OXY-13 (auto joins, single DSL addition). Bundles four separable but coupled sub-features that each touch the full SQL-generation pipeline: `LATERAL` (new `JoinPart` + correlation-scope parsing + `FragmentBuilder` lateral SQL), `UNION ALL` (new `ParsedQuery.UnionQuery` node + multi-leg `RefMap` + `ORDER BY`/`LIMIT` scoping + `NULL`-padding / type-cast handling), `sparse data` (nullable `Option` tuple `RowRepr`/`ResultDecoder` alignment across heterogeneous legs), and `zstream agg` wiring/demo (already implemented core `SparseStreamAggregator` but not proven end-to-end with sparse rows). Comparable to shipping two medium features together; if kept as one issue, 13 is appropriate. Prefer splitting into 3 subtasks (lateral=5, union+sparse=5, agg demos/docs=2) which would individually be 5/5/2 — listed as 13 to reflect single-issue sizing.
  - Justification: Each leg requires a parser addition (`MapChainParser`/`PartialQueryParsers` + quoting in `AppliedAnonFunctCall`), a model addition (`ParsedQuery`, `JoinPart`/`UnionPart`), a generation addition (`FragmentBuilder` SQL emit with correct `LATERAL (... ) ON` and `UNION ALL` leg flattening), plus `DecoderBuilder` nullable-column handling and an `it-test` proving `ZStream` streaming semantics. Cross-cutting concerns are non-trivial: lateral correlation scope (right side refs outer FROM), union column alignment / null-cast correctness, and ordering guarantees for sparse aggregation.

- **Autonomy:** 3 / 6 — moderately autonomous with human checkpoint needed. An agent with this briefing + the repo can produce a plausible lateral/union/sparse implementation by mirroring `JoinPart`/`SelectQuery`/`FragmentBuilder` patterns, but the *intended DSL spelling* and the *sparsity + aggregation contract* (see Open Questions) are genuinely under-specified — building without a 15-minute confirmation risks baking in a spelling (`lateralJoin` vs. `Q.lateral(subquery)` vs. modifier) that doesn't match the product owner's mental model, and risks mis-ordering semantics for `SparseStreamAggregator` (which is order-sensitive via `AndThen`'s "LHS will be eagerly emitted" note at `SparseStreamAggregator.scala:68`). Moderate autonomy: can explore and prototype, but should pause before committing the public DSL API.

  - Justification: The macro/SQL-generation path is well-trodden (add `Q` entry -> `T` type -> quote-match in `*Part.parse` -> `FragmentBuilder` emit), but naming and combinator shape are product decisions, not inferrable from code alone. The repo has zero precedent for `UNION`/`LATERAL` spelling, so the agent would be inventing public vocabulary — which is precisely what autonomy rating 3 flags.

- **Ambiguity-to-resolve:** 5 / 6 — significant-to-major ambiguity blocks start. Title is 18 words but still under-specified on at least five axes that change the required code: (1) lateral API shape, (2) union API shape and whether `UNION` distinct vs `UNION ALL` only, (3) sparse row encoding (`Option` tuple vs. named wrapper vs. typed sparse row), (4) depth/generality of `zstream agg` (hand-written `*: ` combinator composition vs. macro-derived aggregator for arbitrary case class trees), and (5) ordering/fetch-size contract for correct sparse aggregation (see Open Questions). Each is a 30-second to 5-minute human decision that changes files touched and tests written.

  - Justification: Without answers the agent could plausibly build any of 2^5 = 32 different feature variants, all satisfying the literal title but only ~one matching the intended design. The four-way bundling itself is ambiguous: is this one task to land atomically, or four micro-tasks that could be sequenced (lateral first, union+sparse second, agg polish third)? That sequencing decision alone is a 1-2 point sizing swing.

## Open Questions

1. **Lateral API shape:** Should lateral joins be standalone `Q.lateralJoin[A] if cond` / `Q.lateralLeftJoin[A] if cond` (symmetric with today's `Q.join`/`Q.leftJoin`, cleanest) or a modifier on the existing join (`Q.join[A].lateral if cond` / `Q.lateral(subQuery)`) or a free function `Q.lateral(query: QueryO[A])` that takes an explicit subquery `QueryO`? The choice changes parser structure (`JoinPart` extension vs. new `LateralPart`) and whether lateral legs can be arbitrary `SELECT` subqueries (with their own `WHERE`/`ORDER BY`/`LIMIT`) or just table refs. Preference inferred: standalone `lateralJoin` + sub-select via implicit filter correlation `if n.personId == p.id` (minimal), but confirm.

2. **Union API shape:** Prefer `queryA.unionAll(queryB)` as an extension method on `QueryO[O]`/`QueryIO[I,O]` (chaining, keeps comprehension structure readable) or `Q.unionAll(q1, q2, q3)` as a variadic free function (simpler parser — single `UnionQuery` node)? Also: is the requirement strictly `UNION ALL` (no dedup, order-preserving, streaming-friendly) or also `UNION` distinct? Title says `union all` explicitly — assume `UNION ALL` only for v1.

3. **Sparse row encoding:** Should a sparse row be an `Option`-tuple `(Option[Person], Option[Note])` (Pragmatic, reuses existing `RowRepr.optional`/`ResultDecoder.optional`, aligns with union null-padding), a typed `Sparse2[A,B]` / `Sparse3[A,B,C]` wrapper that carries null-padded leg identity at the type level (safer but new `RowRepr` typeclass plumbing), or an anonymous sparse row view derived from `Tuple`? For union legs, how are missing-column `NULL`s cast — `NULL:: <pg type>` per column (explicit typing) or relying on Postgres inference from the first leg's column types (fragile)? Confirm expected null-padding strategy.

4. **ZStream agg depth / derivation:** Is the desired consumer code hand-written combinator composition `SparseStreamAggregator.of[Person] *: SparseStreamAggregator.of[Note].many[Seq] *: ...` (exactly what `StreamAggregatorSpec` megaSpec demos, good for v1, no macro) or should OXY-98 provide a macro-derived `SparseStreamAggregator.derived[PersonWithNotes]` that mirrors `TableRepr.derived` and auto-chooses `optional`/`many` from the domain case-class shape (`case class PersonWithNotes(person: Person, notes: List[Note])`)? Derived is more ergonomic but is essentially OXY-89/90's auto-transform problem applied to aggregation — significantly more scope.

5. **Ordering guarantee for correct aggregation:** `SparseStreamAggregator`'s doc explicitly says rows are order-sensitive: the input `(Option[A], Option[B])` stream must be ordered so that all `B`s for a given `A` are contiguous and `A` repeats only when a new parent starts (see class doc example: `1,2,(None,true),(3,None,"A"),...`). The simplest SQL that satisfies this is `SELECT ... UNION ALL ... ORDER BY parent_sort_key, child_sort_key NULLS FIRST` (or `ORDER BY p.id, n.id`) — but the required sort key is not named in the issue. Should the DSL provide `orderBySparse`/`aggOrderBy` that guarantees aggregator-correct ordering, or is it the caller's responsibility to add `orderBy(p.id.asc, n.id.asc)`? Must also confirm expected in-order vs. out-of-order sparse rows (e.g. `LATERAL LIMIT` legs vs. `UNION ALL` legs interleaved).

6. **FETCH / streaming contract:** For large hierarchies, should the sparse stream be bounded by `LIMIT` per lateral subquery (e.g. "top 3 notes per person" via lateral `LIMIT 3` — primary benefit of lateral), or paginated at the parent level with `streamWithFetchSize`? Should the generated SQL use `CURSOR` / server-side pagination (`fetchSize` hint in `QueryResult.Returning.Config`) to preserve `ZStream` laziness under backpressure, or does Postgres JDBC streaming via `streamWithFetchSize` already satisfy the contract?

7. **Scope of nesting depth:** Is v1 expected to handle single-level nesting (parent + `List[Child]`) only, or arbitrary depth (e.g. `Department -> List[Employee -> List[Project]]` with N levels via `N` union legs or recursive lateral joins)? The latter multiplies the `SparseStreamAggregator` combinator depth (`*: ` chaining-is-right-associative per `SparseStreamAggregator.scala:68` LHS-eager-emit note) and may require macro derivation for >3 levels.

8. **Scope vs. OXY-14 sequencing:** Is OXY-98 the *alternative* to OXY-14's JSONB aggregation (caller picks one), or are they expected to layer (e.g. a `LATERAL` subquery that itself returns `jsonb_agg`) and should benchmarks compare them? If alternative, should `docs/docs/sql/queries.md` frame a decision tree ("use JSONB for shallow wide hierarchies, use sparse+agg for deep streaming hierarchies")? If layered, should the `jsonb_agg` expression be usable *inside* the lateral subquery's yield?

9. **Backwards compatibility / feature gating:** Should the feature be behind an opt-in import (like `oxygen.transform.auto.given`) or enabled globally? No gating is expected — new DSL vocabulary is additive — but confirm that existing `QueryO`/`QueryIO` call sites remain source- and binary-compatible without recompilation.

10. **Assumption to confirm:** That `oxygen-sql` + `oxygen-zio` (`SparseStreamAggregator`) are the owning modules under Epic OXY-1 `oxygen-sql`, and that no new `oxygen-metrics`/`oxygen-events` integration is implied by "zstream agg" beyond the already-wired `QueryO.>>>` / `Returning.>>>` / `stream.agg` path. Confirmed by verified wiring, but explicit product sign-off would rule out a separate event-stream interpretation.

