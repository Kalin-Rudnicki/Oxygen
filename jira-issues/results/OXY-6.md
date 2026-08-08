# OXY-6 — Add support for array input + unnest

## Original
- **Key:** OXY-6
- **Checklist line:** `- [ ] [OXY-6](https://kr-oxygen.atlassian.net/browse/OXY-6) — **Task** · Low — Add support for array input + unnest`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Add support for array input + unnest
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-6
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Add first-class support in `oxygen-sql` (`modules/sql`) for passing a collection/array as a single bind parameter and expanding it in SQL via PostgreSQL's `UNNEST` (or equivalent `ANY`/`IN` via array).

Today the query DSL (`modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala`, `T.scala`) supports scalar inputs (`Q.input[A]`), optional inputs, and const inputs (`InputPart`). Each input binds to `?` placeholders per-column via `InputEncoder` / `FragmentBuilder`. There is already an `InputEncoder.ArraySeqEncoder` (`modules/sql/schema/InputEncoder.scala:78`) that writes a JDBC array (`writer.unsafeWriteArray`) and a corresponding per-type column `Type` (`Column.Type`), but the **DSL has no syntax to declare an array-typed input nor to generate `UNNEST(?::type[])` / `= ANY(?)` SQL**.

Common use-cases this would unblock:
- **Bulk filter:** `SELECT * FROM person WHERE id = ANY(?)` or `WHERE id IN (SELECT * FROM UNNEST(?))` with `input[Seq[UUID]]` / `ArraySeq[UUID]` as a single param, instead of expanding to N `?` placeholders or doing N queries.
- **Join against an input array:** `SELECT ... FROM person JOIN UNNEST(?) AS ids(id) ON person.id = ids.id` or `FROM UNNEST(?)` as a derived table.
- **Bulk insert/select from values supplied as arrays:** complementary to `batchOptimizedInsert` but for read-side filtering/joining.

**Who it affects:** Any service using `oxygen-sql` hand-written queries (`@compile` / `QueryIO.compile`) that needs to filter or join by a dynamic set of IDs/values. Currently the only workaround is string-interpolated SQL, multiple queries, or abusing `batchOptimizedInsert` into a temp table.

**Why it matters:** Without it, `IN` lists must be expanded to N bind params (hitting JDBC param limits ~32767, query-plan cache blowup) or require workarounds. `UNNEST` with a single array bind is the idiomatic Postgres solution — one param, one plan, efficient index use. This is also a prerequisite/sibling to `OXY-17` (Add support for `IN`) and `OXY-98` (lateral join + union all + sparse data) which both touch collection handling.

**Inferred acceptance criteria:**
1. A new DSL input form for array/collection inputs, e.g. `input[Seq[A]]` / `input[ArraySeq[A]]` / `input[Chunk[A]]` where `A` has a `RowRepr`/`Column.Type` (or a dedicated `Q.input.array[A]` helper) that binds as a single JDBC array parameter via `InputEncoder.ArraySeqEncoder` (or a new `ArrayEncoder`).
2. A DSL construct that expands the array in SQL, at minimum one of:
   - `where if ids.contains(person.id)` lowering to `person.id = ANY(?)`, OR
   - `where if person.id.in(ids)` / explicit `UNNEST`, OR
   - `select`/`join` from `UNNEST(ids)` — e.g. `val ids <- Q.unnest(input[Seq[UUID]])` or `join[Unnest[A]]` style.
   The exact syntax is not specified in the title; any of the above would satisfy "array input + unnest".
3. Generated SQL uses `UNNEST(?::type[])` or `ANY(?::type[])` with correct Postgres array type cast derived from `Column.Type` / `RowRepr` for `A`.
4. Works as a `QueryIO[Seq[A], O]` / `QueryIO[ArraySeq[A], O]` (single array param) and composes with other `input` params (tuple-ized via `FragmentBuilder`'s multi-input handling).
5. Includes compile-time macro support (new `QueryExpr`/`RawQueryExpr` case, `FragmentBuilder` branch) and runtime `InputEncoder` wiring so `PreparedStatement` writes the array correctly.
6. Docs updated (`docs/docs/sql/queries.md`) and at least one `it-test` example (e.g. filter `Person` by `Seq[UUID]` via unnest) passes.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title is unambiguous in Postgres context: "array input + unnest" is a well-known pattern (`UNNEST($1::uuid[])`) — no other plausible meaning in `oxygen-sql`.
  - Codebase confirms the gap: `InputEncoder.ArraySeqEncoder` exists but is only used for batch internals (`BatchChunkEncoder`), while the DSL's `Q.input` only handles scalar `A` / `Option[A]` / `Const[A]` (`modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/InputPart.scala:18`, `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala:10`) with no array variant, and `FragmentBuilder` / `QueryExpr` / `RawQueryExpr` have no `UNNEST` case.
  - Sibling issues corroborate: `OXY-17` (Add support for `IN`) is the scalar-list alternative, and `OXY-98` mentions sparse/nested data — consistent with array/unnest being the array-side piece.
  - Downgraded from 5 because no explicit spec, TODO, skipped test, or Jira body was retrievable to confirm exact DSL spelling (`ANY` vs `UNNEST` vs `IN (SELECT * FROM UNNEST...)`).

## Required Changes (only if Confidence >= 3)

- **DSL surface — `modules/sql/core/src/main/scala/oxygen/sql/query/dsl/Q.scala` and `T.scala`:**
  - Add array-input helper, e.g. `Q.input.array[A]` / `Q.input.seq[A]` or extend `Q.input.apply[Seq[A]]` to be recognized as array input. Add `T.ArrayInput[A]` (or `T.Input[ArraySeq[A]]` specialization) alongside existing `Input`/`OptionalInput`/`ConstInput`.
  - Add unnest construct, e.g. `Q.unnest[A](arrayInput)` returning a table-like `T.Select[A]` / `T.Join[A]` or a `WHERE`-usable expression (`contains`/`in`). Alternatively add `Extension` on `Seq[A]` like `ids.contains(field)` that lowers to `field = ANY(?)`.
  - Verified: current `Q.input` object only has `apply`, `optional`, `const` — no array; `T.scala` has no collection input type.

- **Parsing — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/part/InputPart.scala`, `RawQueryExpr.scala`, `QueryExpr.scala`:**
  - Extend `InputPart.parse` to match the new array input shape (`Q.input.array` / `Q.input[Seq[_]]`) and produce a distinct `VariableReference.ArrayInput` (or flag on `FromInput`).
  - Add `RawQueryExpr.Unnest` / `RawQueryExpr.ArrayContains` and corresponding `QueryExpr.Unnest` / `QueryExpr.ArrayComp` cases. Handle `Option[Seq[A]]` if desired.
  - Inferred: no existing `Unnest` case today; `QueryExpr.Binary` only handles `Comp` / `AndOr`.

- **SQL generation — `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/FragmentBuilder.scala`:**
  - Add branch in `queryExprToFragment` for the new `QueryExpr` case that emits `UNNEST(?::type[])` or `? = ANY(?::type[])` with proper type cast. Derive the Postgres array type string from `TypeclassExpr.RowRepr` / `Column.Type.sqlType` for `A`.
  - Handle single-column vs multi-column arrays (likely restrict to single-column `A` initially — e.g. `UUID`, `Int`, `String`, `Email` newtype — and error on composite).
  - If supporting `FROM UNNEST(...)` / `JOIN UNNEST(...)`, add a new `SelectPart.FromUnnest` / `JoinPart.Unnest` or reuse `SelectPart`/`JoinPart` with an `Unnest` table repr and emit `FROM UNNEST(?) AS alias` or `JOIN UNNEST(?) AS alias ON ...`.
  - Verified: `FragmentBuilder` currently emits `?` per column via `RowRepr.columns.exprSeqQMark`; array path needs `?::type[]` + `UNNEST(...)`.

- **Encoding — `modules/sql/core/src/main/scala/oxygen/sql/schema/InputEncoder.scala`, `InputWriter.scala`, `Column.scala`:**
  - Wire the array input's `InputEncoder[ArraySeq[A]]` / `Seq[A]` using existing `ArraySeqEncoder(inner, colType)`. Ensure `contramap` / tuple-zipping in `FragmentBuilder` still works when one of the inputs is an array (single `?` not N `?`).
  - Verify `InputWriter.unsafeWriteArray` / `PreparedStatement` array binding handles the array OID correctly (may need `java.sql.Array` creation via `Connection.createArrayOf`).
  - Inferred: `ArraySeqEncoder` exists but is not derived via `DeriveProductInputEncoder` for `Seq` — needs explicit derivation or helper.

- **Query model — `modules/sql/core/src/main/scala/oxygen/sql/generic/model/ParsedQuery.scala`, `PartialQuery.scala`, `RefMap.scala`:**
  - If `UNNEST` is modeled as a `FROM`/`JOIN` source, add a new `PartialQuery`/`ParsedQuery` variant or extend `SelectPart` to carry the unnest input ref. Otherwise if modeled as a `WHERE` predicate (`IN`/`ANY`), no new query shape — just a new `WherePart` expr.
  - Ensure `allQueryRefs` correctly tracks array inputs for "unused input" warnings.

- **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala`, `modules/sql/core/src/test/scala/...`:**
  - Add compile-time tests that `@compile` queries with array input + unnest compile and generate expected SQL (snapshot via `debug = true`).
  - Add integration tests (via `DbMigrationSpec` / `PostgresTestContainer` in `modules/sql/test-utils`) that insert N rows then `SELECT ... WHERE id = ANY(?)` / `JOIN UNNEST(?)` returns correct subset, including edge cases: empty array (should return 0 rows, not error), null array, large array (1000+ elements), and composition with other inputs (`input[Email]` + `input[Seq[UUID]]`).

- **Docs — `docs/docs/sql/queries.md`, `docs/docs/sql/models.md`:**
  - Document the new `input` array form and `unnest`/`contains` syntax with examples. Cross-link from `models.md` column-type section (which Postgres types are array-compatible).

- **Out of scope / follow-ups:**
  - Multi-column unnest (`UNNEST(?::int[], ?::text[])`) — defer.
  - `OXY-17` `IN` with literal list vs array `IN` — coordinate syntax so they don't conflict.

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 5
  - Justification: Touches macro parsing (`RawQueryExpr`/`QueryExpr`/`InputPart`), SQL generation (`FragmentBuilder`), and schema/encoder plumbing (`InputEncoder`/`Column.Type`). Not trivial, but scoped to one module (`modules/sql/core`) with no migration/schema change. Fits a single focused PR; 8 if `FROM UNNEST` as derived table with aliasing is required instead of just `= ANY(?)`.

- **Autonomy:** 3 / 6 — needs product/design choice before coding
  - Justification: Core mechanics are mechanical once the DSL spelling is chosen, but the spelling itself (`Q.unnest` vs `ids.contains(x)` vs `x.in(ids)` vs `= ANY(?)` vs `IN (SELECT UNNEST(...))` and `FROM` vs `WHERE` usage) is ambiguous and affects many files. An agent can implement any one variant autonomously, but picking the wrong one risks rework.

- **Ambiguity-to-resolve before start:** 4 / 6 — notable open questions block start
  - Justification: Five blocking design decisions (see Open Questions below) about SQL form, input type, and FROM vs WHERE placement. Code signal does not disambiguate; needs ~30-min human decision before implementation.

## Open Questions

1. **SQL form: `UNNEST` vs `ANY` vs `IN (SELECT * FROM UNNEST(...))`?** Title says "unnest" but `col = ANY(?::type[])` is semantically equivalent and simpler (no derived table). Which form should the DSL emit? `ANY` is more common for `WHERE` filtering; `UNNEST` is needed for `JOIN`/`FROM` use-cases.
2. **DSL spelling:** Should the user write `where if ids.contains(person.id)` (extension method), `where if person.id.in(ids)`, `Q.unnest(ids)` as a table source, or `join[Unnest[UUID]] if ...`? The choice determines whether this is a `WherePart` expr or a new `SelectPart`/`JoinPart`.
3. **Input collection type:** `Seq[A]` vs `ArraySeq[A]` vs `Chunk[A]` vs `List[A]`? `InputEncoder.ArraySeqEncoder` uses `ArraySeq`; should the public API accept `Seq` (covariant, ergonomic) and convert, or strictly `ArraySeq`? What about empty-array semantics (Postgres `ANY('{}')` returns false — should empty input short-circuit to `WHERE false`)?
4. **Type support:** Single-column primitives only (UUID, Int, String, custom `Column.Type` wrappers like `Email`) or composite rows (`UNNEST` of a row type)? Likely restrict to single-column initially — confirm.
5. **Casting and OID:** Should SQL be `UNNEST(?::uuid[])` with explicit `::type[]` cast derived from `Column.Type`, or rely on JDBC `createArrayOf` type inference? Explicit cast is safer but needs mapping from `Column.Type` to Postgres array type name.
6. **Interaction with `OXY-17` (IN):** Should `IN` be implemented as sugar over this array+unnest mechanism, or as a separate `IN (?, ?, ?)` expansion? Decision affects whether these two issues should be combined or sequenced.
7. **Null/empty handling:** Should `Option[Seq[A]]` be supported (nullable array input)? If so, what SQL for `None` — `IS NULL` guard or skip predicate via optional-input mechanism?
