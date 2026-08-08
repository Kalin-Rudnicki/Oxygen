# OXY-17 — Add support for IN

## Original
- **Key:** OXY-17
- **Checklist line:** `- [ ] [OXY-17](https://kr-oxygen.atlassian.net/browse/OXY-17) — **Task** · High — Add support for IN`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Add support for IN
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-17
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Add `IN` predicate support to the `oxygen-sql` query DSL (`modules/sql` — Epic OXY-1 `oxygen-sql`) so hand-written compiled queries (`@compile` / `QueryIO.compile` + `where if ...`) can filter with SQL `IN`.

Today the DSL (`modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala`, `T.scala`, `docs/docs/sql/queries.md`) supports binary comparisons (`==`, `!=`, `<`, `<=`, `>`, `>=`, `<=>`, `@>`, `<@`) via `BinOp.Comp` → `QueryExpr.BinaryComp` → `FragmentBuilder.queryExprToFragment.binary`, plus `&&`/`||` and `count`/`tablePK`/`tableNPK`. There is **no `IN` / `NOT IN` support** — verified by grep: `BinOp.scala` has no `IN`, `RawQueryExpr.scala:173-192` / `QueryExpr.scala:256-298` have no `In` case, `FragmentBuilder.scala:175-221` has no `IN` branch, and `docs/docs/sql/queries.md` DSL vocabulary table lists no `in`. Any attempt to write `where if ids.contains(person.id)` / `where if person.id.in(ids)` / `where if person.status.in(Seq(...))` either fails to compile in the macro or produces a macro error ("unknown").

In Postgres/SQL `IN` has 3 closely related forms — the title's 2-word brevity leaves which one(s) is intended ambiguous, but all reduce to the same macro+SQL-generation gap:

1.  **Value-list `IN` (primary interpretation):** `WHERE col IN (?, ?, ?)` / `WHERE col NOT IN (?, ?, ?)` where the list is a runtime collection (e.g. `Seq[UUID]`, `Set[String]`, `List[Email]`). This is the classic use-case sibling to OXY-6 (`array input + unnest`): OXY-6 is the single-array-bind `= ANY(?::type[])` / `UNNEST(?)` path, while OXY-17 is the expanded-placeholder `IN (?, ?, ?)` path. OXY-6's result file explicitly calls this out: "OXY-17 `IN` with literal list vs array `IN` — coordinate syntax so they don't conflict."
2.  **Subquery `IN`:** `WHERE col IN (SELECT id FROM other WHERE ...)` — `IN` with a sub-`SELECT` rather than a value list. Less likely as the sole scope, but often co-requested with list `IN`.
3.  **Static / `const` list `IN`:** `WHERE col IN (1, 2, 3)` where the list is a compile-time `const(Seq(...))`. Trivially covered if (1) is implemented.

**Who it affects:** Every service using `oxygen-sql` hand-written queries that needs to filter by a dynamic set — `findPersonsByIds(ids: Seq[UUID])`, `findOrdersByStatuses(statuses: Set[OrderStatus])`, `where if user.groupId.in(allowedGroups)`. Current workaround is N separate queries, string-interpolated SQL, inserting IDs into a temp table then joining, or abusing `batchOptimizedInsert` — all of which defeat the DSL's type-safe, single-round-trip promise.

**Why it matters (High priority):** `IN` is one of the most common SQL predicates; its absence forces workarounds that hit the JDBC parameter limit (~32767), blow up the query-plan cache (one plan per list length if inlined), and bypass compile-time safety. The epic OXY-1 is `In Progress`, and OXY-6 (array+unnest, Low) is the alternative collection-filter mechanism — shipping `IN` + `unnest` together gives users both the portable `IN (?, ?, ?)` form and the Postgres-idiomatic single-param `ANY`/`UNNEST` form. Priority `High` suggests this is blocking real queries, not speculative.

**Inferred acceptance criteria:**

1.  DSL syntax for `IN`/`NOT IN` in `where` / `join ON` / `having`-like predicates, at minimum one ergonomic spelling — e.g.:
    ```scala
    // collection from an input param
    @compile
    val findByIds: QueryIO[Seq[UUID], Person] =
      for {
        ids <- input[Seq[UUID]]
        p   <- select[Person]
        _   <- where if p.id.in(ids)          // or: ids.contains(p.id) / p.id.inList(ids)
      } yield p

    // inline literal / const
    where if p.status.in(Set("active", "pending"))
    where if p.status.notIn(excluded)
    ```
    The exact spelling is unspecified by the title — any of the above satisfies "IN", but it must be documented.

2.  Generated SQL is `"<col> IN (?, ?, ?)"` (or `NOT IN`) with one `?` per element for single-column types, and `"<col> IN (?, ?, ?)"` expands correctly for composite?/single-column restriction — likely restrict to single-column `A` where `A` has a `RowRepr`/`Column.Type` (UUID, String, Int, custom newtypes like `Email`) and emit a compile-time error for composite rows. Empty-collection semantics must be defined (Postgres `IN ()` is syntax error): either `WHERE FALSE` (for `IN`) / `WHERE TRUE` (for `NOT IN`) or `IN (NULL)` that matches nothing — and must not generate invalid SQL.

3.  Works as `QueryIO[Seq[A], O]` / `QueryIO[Set[A], O]` (or `ArraySeq[A]`) — single collection param binding to N `?` placeholders via `InputEncoder` — and composes with other inputs (tuple-ized via `FragmentBuilder`'s multi-input `ProductGeneric` handling) and with other predicates (`&&`/`||`, `isEmpty`/`nonEmpty` guards).

4.  Compile-time macro support: new `RawQueryExpr`/`QueryExpr` case (`In`/`NotIn`) and parsing in `RawQueryExpr.Binary` alternative or dedicated `RawQueryExpr.In` parser, plus a `FragmentBuilder` branch that emits the `IN (...)` fragment with correct `qmark` expansion (`RowRepr.columns.exprSeqQMark` per element) and `GeneratedInputEncoder` concatenation.

5.  Parameter binding / encoding: each element encoded via its `InputEncoder[A]` (reusing `RowRepr` column count = 1); for multi-input queries the `IN` collection's encoders must interleave correctly with other scalar inputs in `GeneratedFragment`/`GeneratedInputEncoder`.

6.  Tests + docs: `docs/docs/sql/queries.md` DSL vocabulary updated with `in`/`notIn` examples; `it-test` (`modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala`) compiles the new syntax (`debug = true` snapshot) and integration-tests against a real Postgres (`DbMigrationSpec`/`PostgresTestContainer`) covering non-empty list, empty list, single element, large list (1000+), `NOT IN`, and composition with another `input` param.

7.  Coordination with OXY-6: `IN (?, ?, ?)` vs `= ANY(?::type[])` choice is surfaced so the two features don't produce conflicting DSL (e.g., `in` for expanded `IN`, `contains`/`any` for array `ANY`). If both ship, `IN` should NOT be sugar over OXY-6's array mechanism — they are distinct SQL forms.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title "IN" in the `oxygen-sql` epic context (OXY-1 In Progress) has a single dominant SQL meaning — the `WHERE col IN (...)` predicate. No other module uses "IN" as a feature name, and the SQL DSL is the only place where `IN` would be a missing keyword. Sibling OXY-6 ("array input + unnest") explicitly frames OXY-17 as the scalar-list `IN` counterpart, removing the main ambiguity between `IN (?, ?, ?)` vs array `ANY`.
  - Code gap is verified, not inferred: `BinOp.scala` enumerates every supported operator (`==`, `!=`, `<`, `<=`, `>`, `>=`, `<=>`, `<#>`, `<->`, `<+>`, `@>`, `<@`, `&&`, `||`) with no `IN`/`NOT IN`/`ANY`; `RawQueryExpr.scala` / `QueryExpr.scala` / `FragmentBuilder.scala` have no `In` case; `docs/docs/sql/queries.md` vocabulary table has no `in`. This is exactly the shape of other DSL-extension tasks (OXY-6, OXY-94, OXY-100) where the missing syntax corresponds to the task title.
  - Downgraded from 5/6 because the title is only 2 words ("IN") with no Jira body fetched and no `TODO`/`FIXME`/`skip` comment mentioning `IN` in the codebase. Exact DSL spelling (`p.id.in(ids)` vs `ids.contains(p.id)` vs `Q.in(p.id, ids)`), collection type (`Seq` vs `Set` vs `ArraySeq`), empty-list semantics, and subquery-`IN` scope are unspecified — so implementation details remain inferred.
  - Remaining alternative interpretations (subquery `IN (SELECT ...)` as primary scope, or `IN` meaning Scala `Iterable.contains` rather than SQL `IN`) are materially less likely given the SQL-module ownership and the OXY-6 complement, but cannot be ruled out without the Jira body.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **DSL surface — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala` + `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/T.scala` (Verified)**
  - Add `IN`/`NOT IN` extension(s) usable inside `where if ...`. Options (pick one, document choice):
    - Extension on query-field reps: `extension [A](field: T.Column[A]) def in(coll: Seq[A]): T.Predicate` / `notIn`, or
    - Extension on collection input: `extension [A](coll: Seq[A]) def contains(field: T.Field[A]): T.Predicate`, or
    - Helper in `Q`: `Q.in(field, coll)` / `Q.notIn(field, coll)`.
  - Decide input shape: `input[Seq[A]]` / `input[Set[A]]` / `input[ArraySeq[A]]` / `input[List[A]]` — likely accept `Seq[A]` (ergonomic) and normalize to `ArraySeq` internally, similar to `RowRepr.seq` (`modules/sql/schema/RowRepr.scala:seq`). Add `T.InInput[A]` or reuse `T.Input[Seq[A]]` with a flag so `FragmentBuilder` knows to expand to N `?` not 1 `?`.
  - Verified: `Q.scala:10` / `T.scala:161` define `input`, `optional`, `const` only — no collection IN helper today.

- [ ] **Parsing — `modules/sql/core/src/main/scala/oxygen/sql/generic/parsing/RawQueryExpr.scala` + `modules/sql/core/src/main/scala/oxygen/sql/generic/model/QueryExpr.scala` + `modules/sql/core/src/main/scala/oxygen/sql/generic/model/BinOp.scala` (Verified)**
  - Add `RawQueryExpr.In` (and `NotIn` or `In(fullTerm, lhs, rhs, negated: Boolean)`) carrying `lhs: RawQueryExpr` (typically a `QueryVariableReferenceLike`) and `rhs: RawQueryExpr` (typically an `InputVariableReferenceLike` for `Seq[A]` or a `ConstValue` for literal `Seq`). Alternatively model as `RawQueryExpr.Binary` with a new `BinOp.In` — but `IN` is n-ary on RHS, so a dedicated case is cleaner.
  - Add `QueryExpr.In` (and `NotIn`) — `sealed trait InLike extends QueryExpr` with `field: QueryVariableReferenceLike`, `collection: InputVariableReferenceLike | ConstValue`, `negated: Boolean`, `queryRefs` = both sides. Restrict `field.rowRepr` to single-column (error if multi-column) at `QueryExpr.parse` time.
  - Update `RawQueryExpr.parse` to match the chosen DSL spelling (e.g., `Select(Ident("in"), ...)` / `Apply(Select(...))` shape). Mirror `OptionNullability` / `CountWithArg` precedent (`RawQueryExpr.scala:205-206`, `QueryExpr.scala:304-318`) for how built-ins are lowered.
  - Inferred: no `In` case exists today; `Binary.parse` only handles `Comp`/`AndOr`. New case needs `Term` shape matching for the extension method call.

- [ ] **SQL generation — `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala` (Verified)**
  - Add branch in `queryExprToFragment.apply` for `QueryExpr.In`/`NotIn`. Emit `"<fieldFrag> IN (?, ?, ...)"` / `"<fieldFrag> NOT IN (?, ?, ...)"` where each `?` is `parentContext.columns.exprSeqQMark` for the element type's column count (must be 1).
  - Expand the collection input's `GeneratedInputEncoder` to N copies concatenated (`GeneratedInputEncoder.flatten` over elements), wiring each element's `InputEncoder[A]` (from `TypeclassExpr.RowRepr.inputEncoder`) — loop over runtime collection length to produce N `?` fragments joined by `, `. For `ConstValue` with literal `Seq`, expand at macro time to N `?` with const encoders.
  - Handle empty collection: emit `GeneratedFragment.sql("FALSE")` for `IN` (since `x IN ()` is invalid and semantically matches nothing) and `GeneratedFragment.sql("TRUE")` for `NOT IN`, or `1=0`/`1=1` — document choice. Do not emit `IN ()` or `IN (NULL)` without explicit justification.
  - Verified: current `binary` / `composite` branches show how `GeneratedFragment.of(..., op.sqlPadded, ...)` assembles fragments and how `queryExprToInputEncoder` maps `InputVariableReferenceLike` to encoders — reuse that pattern but iterate N times.

- [ ] **Encoding / type support — `modules/sql/core/src/main/scala/oxygen/sql/schema/RowRepr.scala` + `modules/sql/core/src/main/scala/oxygen/sql/schema/InputEncoder.scala` + `modules/sql/core/src/main/scala/oxygen/sql/generic/model/TypeclassExpr.scala` (Inferred)**
  - For `IN (?, ?, ?)` no array OID/`unsafeWriteArray` is needed — each element binds as a scalar `?` via its existing `RowRepr[A].inputEncoder`. Verify `contramap` / tuple-zipping still works when one of the inputs is a collection (expands to N `?` but counts as one logical input param).
  - Restrict to single-column `A` initially; for multi-column `A` (e.g., `Person`) either error at macro time ("IN not supported for composite types") or extend to row-value `IN ((?, ?), (?, ?))` — defer the latter unless spec demands it.
  - If supporting `Set`/`List`/`Seq` uniformly, normalize via `SeqOps` (`oxygen.predef.core.SeqOps`) or `ArraySeq` conversion so iteration order is deterministic for tests.

- [ ] **Query model — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/ParsedQuery.scala` / `WherePart.scala` / `RefMap.scala` (Verified — no change expected unless `IN (SELECT ...)` is also required)**
  - For value-list `IN`, no new `ParsedQuery` shape — `WherePart.filterExpr` already holds an arbitrary `QueryExpr`, so `QueryExpr.In` flows through `where(w)` unchanged.
  - If subquery `IN (SELECT ...)` is deemed in scope, add `SelectPart.FromSubQuery` reuse or a new `RawQueryExpr.InSubQuery(lhs, subQuery: ParsedQuery.SelectQuery)` and a `FragmentBuilder.select(subQuery)` emission for `IN (SELECT ...)`. Not required for the high-confidence value-list interpretation — note as optional.

- [ ] **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala` + `modules/sql/core/src/test` + `modules/sql/test-utils` (Verified — patterns exist)**
  - Compile-time: `@compile(debug = true)` queries that use `in`/`notIn` compile and snapshot expected SQL (`"WHERE \"p\".\"id\" IN (?, ?, ?)"` etc.) — mirror `ltreeAncestors` / `ltreeDescendants` examples (`queries.scala:382-396`).
  - Integration (via `DbMigrationSpec` + `PostgresTestContainer` under `modules/sql/test-utils`): insert N `Person` rows, query with `ids = Seq(p1.id, p2.id)` → asserts correct subset; cases: empty `Seq` → 0 rows (or all rows for `NOT IN`), single element, 1000+ elements (prove plan caching / param expansion works), literal `const(Set(...))`, and composition with another `input[Email]` param (`input[Email]` + `input[Seq[UUID]]` tuple) and with `&&`/`||`.
  - Edge: `NOT IN` with empty list, `IN` with duplicate values, `IN` inside `join ON` vs `where`.

- [ ] **Docs — `docs/docs/sql/queries.md` (Verified — section exists)**
  - Update DSL vocabulary table (`queries.md:116-129`) with `field.in(coll)` / `field.notIn(coll)` row, plus a short example for collection input + empty-list behavior. Cross-link to OXY-6 (`array input + unnest`) to explain when to use `IN (?, ?, ?)` vs `= ANY(?::type[])`/`UNNEST`.

- [ ] **Out of scope / follow-ups**
  - Row-value `IN` for composite types (`(a, b) IN ((1, 2), (3, 4))`), `IN` with lateral subquery, and `IN` on nullable `Option[A]` columns — defer to follow-up unless product clarifies.
  - Coordinate with OXY-6 sequencing: if both `IN` and `array+unnest` are planned, decide DSL naming so they don't collide (`in` vs `any`/`contains`/`unnest`).

- **Verified vs. inferred:** Absence of `IN` in `BinOp`/`RawQueryExpr`/`QueryExpr`/`FragmentBuilder`/`queries.md` was verified by grep + file reads. That this maps to `WHERE col IN (?, ?, ?)` value-list semantics (vs subquery `IN`) and that DSL spelling will be `field.in(coll)`-like are inferred from OXY-6 complement and common SQL usage — no Jira body or TODO comment was found to lock the spelling.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — lean is 3 if value-list `IN (?, ?, ?)` only; 5 if parameterized collection + const literal + empty-list + `NOT IN` edge cases and docs/tests are all in scope
  - Justification: Touches only the DSL macro + generation path (3 parsing files + 1 generation file + DSL surface), no migration/schema or storage-layer change, no new module. Pattern is well-established from prior DSL extensions (`@>`, `<@`, `count`, `tablePK` — see `BinOp.scala:17-29`, `QueryExpr.scala:312-318`, `FragmentBuilder.scala:175-226`). Single focused PR fits in `modules/sql/core`. The 3→5 range hinges on whether empty-collection + multi-collection + `NOT IN` + subquery-`IN` are all required.

- **Autonomy:** 3 / 6 — needs product/design choice before coding
  - Justification: Mechanics are mechanical once DSL spelling and empty-list semantics are fixed (an agent can implement `IN (?, ?, ?)` expansion autonomously), but 4 blocking design decisions (see Open Questions) about spelling (`in` vs `contains` vs `Q.in`), collection type (`Seq` vs `Set` vs `ArraySeq`), empty-list SQL, and `IN` vs `ANY`/`UNNEST` boundary with OXY-6 risk rework if chosen wrong. A 30-minute human decision on those points would raise autonomy to 5/6.

- **Ambiguity-to-resolve:** 4 / 6 — notable open questions block start
  - Justification: Title is 2 words with no body; code gives no hint about preferred DSL syntax or empty-collection handling. Five concrete ambiguities below must be resolved or assumed; the implementation cannot be reviewed without agreeing on them. Lightweight clarification (one paragraph confirming value-list `IN` vs subquery `IN` and the exact extension-method name) would drop this to 1–2.

## Open Questions

1.  **DSL spelling:** Should the user write `where if p.id.in(ids)`, `where if ids.contains(p.id)`, `where if p.id.inList(ids)`, or `where if Q.in(p.id, ids)` / `Q.notIn(...)`? Choice determines the `Term` shape the macro must match (`Select` + `Apply`) and whether `in` is an extension on the field or on the collection. Which name is reserved for OXY-6's array `ANY`/`UNNEST` (`contains` vs `any` vs `in`)?
2.  **Collection type:** Public API `Seq[A]` vs `Set[A]` vs `ArraySeq[A]` vs `List[A]` vs `Chunk[A]`? `RowRepr.seq` already supports `F[A]` via `SeqOps`; should `in` accept any `SeqOps` collection or strictly `Seq`? Deduplication semantics for `Set` (order?) and whether to accept `const(Seq(...))` literals matter for testing.
3.  **Empty-list semantics:** Postgres `IN ()` is syntax error. Should `where if p.id.in(Seq.empty)` lower to `WHERE FALSE` (matches nothing) and `NOT IN (empty)` to `WHERE TRUE` (matches all), or to `IN (NULL)` / short-circuit at runtime? This affects generated SQL validity and must be documented + tested.
4.  **`NOT IN` vs `NOT (x IN (...))`:** Is `NOT IN` a first-class `notIn`/`not in` syntax or derived via `!in` / `NOT (col IN (...))`? `NOT IN` with `NULL` elements has different SQL semantics (unknown) — should docs warn about it?
5.  **Subquery `IN` scope:** Is `WHERE x IN (SELECT id FROM other)` in scope, or strictly value-list `IN`? If subquery is included, what syntax — `where if p.id.in(select[Other].map(_.id))` or `where if p.id.inSubQuery(otherQuery)`? This materially expands the macro work (new `ParsedQuery` handling).
6.  **Interaction with OXY-6:** Should `IN` be implemented as sugar over OXY-6's array mechanism (`x = ANY(?::type[])`) or as distinct `IN (?, ?, ?)` expansion? Decision affects param count, plan-cache behavior, JDBC limits, and whether the two issues should be merged or sequenced. Recommendation: keep them distinct — `in` for expanded `IN`, `any`/`contains`/`unnest` for array.
7.  **Type restriction:** Single-column types only (UUID, String, Int, newtypes via `Column.Type`) or also row values (`(a, b) IN ((1, 2), (3, 4))`)? Likely restrict to single-column initially — confirm, and decide whether `Option[A]` columns are supported (`Option[UUID].in(...)`).
