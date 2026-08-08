# OXY-14 — Add support for querying into a JSONB structure

## Original
- **Key:** OXY-14
- **Checklist line:** `- [ ] [OXY-14](https://kr-oxygen.atlassian.net/browse/OXY-14) — **Task** · Normal — Add support for querying into a JSONB structure`
- **Type:** Task
- **Priority:** Normal
- **Title (verbatim):** Add support for querying into a JSONB structure
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-14
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Extend the `oxygen-sql` query DSL (`modules/sql/core/src/main/scala/oxygen/sql/query/dsl/`, `generic/`) so a `SELECT` query can return its result as a single (or per-parent-row) **JSONB** value that encodes a nested/hierarchical structure, decoded client-side as `TypedJsonb[A]` (or `Jsonb`). Instead of today's flat-row model (`yield (p, n)` returns `List[(Person, Note)]` with one row per join and app-side aggregation via `SparseStreamAggregator`), the SQL would build the hierarchy inside Postgres using JSON functions (`jsonb_build_object`, `to_jsonb`/`row_to_json`, `jsonb_agg`, `jsonb_object_agg`) and return a single `JSONB` column per logical entity.

Concrete motivating example — fetch `Person` with nested `List[Note]` in one round-trip:

```scala
// desired DSL (one possible spelling — exact syntax TBD)
@compile
val personWithNotes: QueryIO[UUID, TypedJsonb[PersonWithNotes]] =
  for {
    id <- input[UUID]
    p  <- select[Person]
    // DSL builds: SELECT jsonb_build_object('person', to_jsonb(p.*), 'notes', (SELECT jsonb_agg(to_jsonb(n.*)) FROM notes n WHERE n.person_id = p.id))
    _  <- where if p.id == id
  } yield jsonbBuildObject(p, jsonbAgg[Note](???))
// decoded via given JsonCodec[PersonWithNotes] -> TypedJsonb
```

A lighter variant is one-row-per-parent with `jsonb_agg` on a `leftJoin`:

```scala
for {
  p <- select[Person]
  n <- leftJoin[Note] if n.personId == p.id
} yield jsonbAgg(n) // SELECT p.*, jsonb_agg(to_jsonb(n.*)) GROUP BY p.pk
```

A third variant is fetching an entire result set as a single JSONB array (`SELECT jsonb_agg(row_to_json(t)) FROM (...)`).

**Why the phrasing "into a JSONB structure"** — the query *projects* into `JSONB` (the RETURNING/SELECT clause produces JSONB), distinct from *storing* typed objects into a `JSONB` column (which already works via `TypedJsonb[A]` in `modules/sql/core/src/main/scala/oxygen/sql/model/json.scala:9` and `RowRepr.typedJsonb` in `modules/sql/core/src/main/scala/oxygen/sql/schema/RowRepr.scala:339`). `docs/docs/sql/models.md:103` documents `TypedJsonb[A]` for column storage with no mention of query-side JSON projection, confirming the gap.

**Who it affects:** Any service that needs nested/hierarchical reads (parent + children, 1-N or N-M) and wants to avoid N+1 queries or manual flat-row deduplication. Directly complementary to **OXY-98** (`lateral join + union all + sparse data + zstream agg … to query nested data structures`) which solves the same "nested data fetch" problem via app-side `SparseStreamAggregator` (`modules/general/zio/src/main/scala/oxygen/zio/SparseStreamAggregator.scala`). OXY-14 is the **DB-side JSON aggregation** alternative: push nesting into Postgres. Consumers would choose one or both.

**Why it matters:** Flat-row joins duplicate parent columns per child row (bandwidth, duplication) and require careful aggregation. A JSONB projection returns one row per parent with children already nested, closely matching domain types that already have `JsonCodec`s. For subgraph fetches it is often simpler and avoids the `lateral join` / sparse-aggregation machinery.

**Inferred acceptance criteria:**
1. A new DSL form that produces `JSONB`-typed output, at minimum one of:
   - Scalar JSON constructors: `jsonb_build_object`, `to_jsonb`/`row_to_json`, or `jsonb_object`.
   - Aggregate JSON: `jsonb_agg` (and optionally `json_agg`, `jsonb_object_agg`).
   Both must be usable in the `yield` (RETURNING) position of a `select` query.
2. Generated SQL uses Postgres JSONB functions with correct `GROUP BY` (for aggregation) and child sub-query correlation where needed.
3. Result decoding: `QueryO[TypedJsonb[A]]` / `QueryIO[I, TypedJsonb[A]]` where `A` has a `given JsonCodec[A]` (reuse `RowRepr.typedJsonb` path) correctly decodes the JSONB text via `fromJsonString` (already used in `RowRepr.scala:341`). Also support raw `Jsonb`.
4. Composes with existing DSL: `where`, `join`/`leftJoin`, `orderBy`, `limit`, and `input`.
5. At least one `it-test` example that inserts a parent + N children, runs the JSONB query, and asserts the decoded `TypedJsonb` matches the expected hierarchy (including empty-children case — `jsonb_agg` returns `null` vs `[]`; `COALESCE` handling must be decided).
6. Docs in `docs/docs/sql/queries.md` and/or `docs/docs/sql/models.md` with examples.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title (7 words, no Jira body fetched) is ambiguous among at least three readings: (a) DB-side JSON aggregation (`jsonb_agg`/`jsonb_build_object`), (b) inserting/selecting `TypedJsonb[A]` column values in queries (but that already works for columns — `RowRepr.typedJsonb` exists and is tested in `RowSchemaSpec.scala:72`), and (c) a filter-side JSONB operator (`@>`, `?`, `->>`). (a) is the clear frontrunner because (b) is already implemented for columns and (c) would be phrased "querying JSONB" not "into a JSONB structure".
  - Code signal is moderate: `TypedJsonb[A]`/`Jsonb` and `Column.Type.Jsonb` exist (`Column.scala:67`, `model/json.scala:9`) but there is zero DSL vocabulary for `jsonb_build_object`/`jsonb_agg`/`row_to_json` (`Q.scala` only has `select`/`join`/`where`/`count`/`mkSqlString`), and no `json` case in `QueryExpr`/`RawQueryExpr`/`FragmentBuilder` — confirming a real gap on the *query projection* side.
  - Sibling signal supports (a): OXY-98 ("lateral join + union all + sparse data + zstream agg in order to query nested data structures") explicitly states the *nested data* goal and names `SparseStreamAggregator`; OXY-14 alongside it reads as the JSON-native alternative to that app-side aggregation path. No other sibling matches as well.
  - Downgraded from 4 because the exact DSL spelling (yield-level `jsonbAgg` vs. dedicated `Q.jsonb.*` helpers vs. auto-nesting of case classes) and the intended shape (per-parent `jsonb_build_object` vs. global `jsonb_agg` vs. both) are not specified anywhere and would require a human/design decision.

## Required Changes (only if Confidence >= 3)

- **DSL surface — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala`, `T.scala`:**
  - [ ] Add JSON/JSONB helpers, e.g. `Q.jsonb.buildObject(...)`, `Q.jsonb.agg[A]`, `Q.jsonb.toJsonb[A]`, or as `yield` extensions (`col.toJsonb`, `rows.jsonbAgg`). At minimum expose `jsonb_build_object` + `jsonb_agg` + `to_jsonb`/`row_to_json`. Model as new `T` types (e.g. `T.JsonbBuildObject`, `T.JsonbAgg`) or as generic `QueryExpr`-producing macros.
  - [ ] Decide whether the API is explicit (user calls `jsonb_build_object("person", p, "notes", notes)`) or auto-derived (e.g. `yield JsonbNested(p, notes)` derives the build). Prefer explicit for v1 — simpler to spec and to compile.
  - [ ] *Verified:* `Q.scala:1-74` currently has no JSON vocabulary; `count` is the only aggregate helper.
  - [ ] *Inferred:* `T.scala` will need new extension/target methods or a new `Jsonb` DSL object similar to `Q.count`.

- **Parsing — `modules/sql/core/src/main/scala/oxygen/sql/generic/parsing/RawQueryExpr.scala`, `QueryExpr.scala`, `Function.scala`:**
  - [ ] Add `RawQueryExpr.JsonbBuildObject` / `JsonbAgg` / `ToJsonb` cases detected via `Function` matching, and corresponding `QueryExpr.Jsonb*` nodes.
  - [ ] Handle column-splat for products (`p.*` -> `to_jsonb(p)` must expand to all `RowRepr` columns). May reuse `RowRepr.columns` or introduce a `to_jsonb` row expansion.
  - [ ] *Verified:* `QueryExpr.scala` already partitions into `ConstValue`/`VariableReferenceLike`/`Binary`/`BuiltIn`/`Composite`; JSON nodes will likely be a new `BuiltIn` sub-case or new top-level case. `RawQueryExpr.scala` is the pre-type-checked mirror.
  - [ ] *Inferred:* no existing JSON case; discovery via `Binary`/`BuiltIn` emulation needed.

- **SQL generation — `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala`, `DecoderBuilder.scala`:**
  - [ ] Add `queryExprToFragment` branches that emit Postgres JSON functions with correct syntax:
    - `jsonb_build_object('k1', expr1, 'k2', expr2, ...)`
    - `to_jsonb(rowAlias)` / `row_to_json(rowAlias)` (or per-table column list)
    - `jsonb_agg(expr)` with surrounding `GROUP BY` handling (aggregate queries need `GROUP BY` on all non-aggregated selected columns / PK).
    - `COALESCE(jsonb_agg(...), '[]'::jsonb)` for empty-children empty-array semantics (decision point — see Open Questions).
  - [ ] For correlated `jsonb_agg` sub-selects (e.g. children per parent), decide between `GROUP BY` on outer join vs. `LATERAL` subquery (`SELECT ... FROM person p, LATERAL (SELECT jsonb_agg(n.*) FROM note n WHERE n.person_id = p.id) AS notes`). The latter pairs with OXY-98's lateral join but can be done standalone.
  - [ ] Wire `RETURNING jsonb_build_object(...)` / `SELECT jsonb_build_object(...)` via existing `ReturningPart` / `FragmentBuilder.ret` / `DecoderBuilder.ret` path. Aggregate case must produce a single `JSONB` column (`size=1`) decoded by `ResultDecoder.SingleDecoder` or `typedJsonb`.
  - [ ] *Verified:* `FragmentBuilder` method `ret` (`FragmentBuilder.scala:332`) handles the RETURNING clause; `allQueryRefs` tracking is central. `ParsedQuery.SelectQuery` owns the `SELECT` shape. No GROUP BY support is currently surfaced in DSL (`Q.scala` has no `groupBy`), so aggregate JSON may require adding `GROUP BY` or restricting to sub-select form first.
  - [ ] *Inferred:* group-by handling is the hardest cross-cut; may be deferred to v1 sub-select form to avoid full `GROUP BY` support.

- **Decoding — `modules/sql/core/src/main/scala/oxygen/sql/schema/RowRepr.scala`, `ResultDecoder.scala`:**
  - [ ] Reuse `RowRepr.typedJsonb` / `typedJson` (`RowRepr.scala:333-342`) — decode as `PGobject` -> `Jsonb(value.string)` -> `fromJsonString[A]`. Ensure `DecoderBuilder` can derive a `ResultDecoder[TypedJsonb[A]]` for a single-column JSONB RETURNING. For raw `Jsonb`, use `RowRepr.jsonb` (`RowRepr.scala:321`).
  - [ ] No new `Column.Type` needed — `Column.Type.Jsonb` already exists (`Column.scala:67`).
  - [ ] *Verified:* `RowRepr.typedJsonb` already bakes in `JsonCodec` decode; `SingleDecoder` exists.

- **Typeclass / generic interaction:**
  - [ ] If auto-nesting from product types is desired (`yield JsonbNested[PersonWithNotes](p, notes)`), add a `DeriveProductJsonbProjection` or reuse `ProductGeneric` to map product fields to JSON keys. Out of scope for explicit v1.

- **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala`, `TableCompanionQuerySpec.scala`, `RowSchemaSpec.scala`:**
  - [ ] Compile-time snapshot tests: `debug = true` queries that assert emitted SQL contains `jsonb_build_object` / `jsonb_agg` / `to_jsonb`.
  - [ ] Integration tests (via `PostgresTestContainer` / `SqlAspects`): create `Person` + `Note` rows, run per-parent JSONB query, assert `TypedJsonb[PersonWithNotes].value` matches expected codec-decoded struct; cover empty-children (`[]` vs null), null handling, and `orderBy`/`limit` composition.
  - [ ] *Verified:* `queries.scala:166-250` defines `Person`/`Note` test fixtures usable for this.

- **Docs — `docs/docs/sql/queries.md`, `docs/docs/sql/models.md`:**
  - [ ] Document JSONB querying pattern, contrast with `SparseStreamAggregator` (OXY-98), and note when to choose DB-side JSON vs. app-side aggregation (tradeoffs: single round-trip vs. streaming, Postgres JSON overhead, codec requirements).

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 5 (Fibonacci) — medium feature. Larger than a doc spike (OXY-5 = 2) but smaller than a full cross-cutting epic. Core work is new DSL vocabulary + parse + FragmentBuilder emission + Decoder wiring + tests/docs. Comparable to OXY-6 (array + unnest) which also requires new input + fragment branches. If full `GROUP BY` + lateral-subselect support is required for safe per-parent aggregation, push to 8.
- **Autonomy:** 3 / 6 — moderately autonomous. An agent with the repo can produce a plausible `jsonb_build_object` + `jsonb_agg` implementation, but the *intended shape* (sub-select vs. group-by, explicit vs. auto-nested, empty-array null semantics, and whether `OXY-14` should be JSON-only or also cover `jsonb @>` filters) needs a 10-minute human/design confirmation before coding. Ambiguous scope risks building the wrong JSON primitive.
  - Justification: Code path is well-trodden (add `RawQueryExpr` → `QueryExpr` → `FragmentBuilder` branch, analogous to existing `count`/`Binary`/`BuiltIn`), but product intent for the "JSONB structure" nesting model is not typed anywhere.
- **Ambiguity-to-resolve:** 4 / 6 — significant ambiguity blocks start. Not clear whether (i) per-parent nested JSON, (ii) global result-set JSON array, or (iii) both is expected; whether grouping/lateral-join support is in scope; how keys are named (derived from field names vs. explicit); and whether filtering on JSONB (`->>`, `@>`) is included or separate.
  - Justification: Title is 7 words; no Jira description was retrievable; sibling OXY-98's description is more explicit about its scope while OXY-14's is not. A short confirmation ("per-parent `jsonb_build_object` + `jsonb_agg` with explicit keys, `COALESCE` to `[]`, sub-select form first, no filter operators in this issue") would drop this to 1–2.

## Open Questions

1. **Per-parent vs. global JSON:** Should `SELECT jsonb_build_object('person', ..., 'notes', jsonb_agg(...))` produce one JSONB row per parent (requiring `GROUP BY` or lateral sub-select), or a single `jsonb_agg(jsonb_build_object(...))` for the whole result set, or both forms?
2. **GROUP BY vs. LATERAL sub-select:** For per-parent children aggregation, is `LEFT JOIN ... GROUP BY p.pk` acceptable, or is the `LATERAL (SELECT jsonb_agg ... WHERE n.person_id = p.id)` form preferred (avoids expanding `GROUP BY` to all parent columns and composes with `orderBy`/`limit`)?
3. **Empty children semantics:** `jsonb_agg` on no rows returns `NULL` in Postgres. Should the emitted SQL be `COALESCE(jsonb_agg(...), '[]'::jsonb)` so decoded `Seq` is `[]` not `null`, or should decoding handle `NULL` -> `[]` explicitly?
4. **Key naming:** Are JSON keys derived from case-class field names / table column names, or explicitly supplied as string literals in the DSL (`jsonb_build_object("person", p, "notes", notes)`)? Explicit is simpler, derived is more ergonomic — needs a choice.
5. **Scope vs. OXY-98:** Is OXY-14 the *alternative* to OXY-98's sparse-stream approach, or are they expected to layer (e.g. `LATERAL jsonb_agg` sub-select combined with `SparseStreamAggregator`)? Should they be implemented to interoperate?
6. **Filtering on JSONB in scope?** Does "querying into a JSONB structure" include `WHERE payload @> '{"key":"val"}'` / `payload->>'field' = ?` predicate operators on `TypedJsonb` columns, or is it strictly projection (`SELECT ... INTO JSONB`)?
7. **Type safety:** Should the DSL enforce `given JsonCodec[A]` at the call site for `TypedJsonb[A]` projections (so arbitrary non-codec types fail at compile time with a clear message), or should `Jsonb` (raw `String`) be the only return type with manual decode?
8. **Assumption to confirm:** That "querying into a JSONB structure" is read-side JSON projection (this triage's interpretation) and not a new persistence pattern for writing JSONB column values inside queries beyond today's `TypedJsonb` column storage.

