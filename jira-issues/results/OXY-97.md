# OXY-97 — Support explicitly naming FK + IDX

## Original
- **Key:** OXY-97
- **Checklist line:** `- [ ] [OXY-97](https://kr-oxygen.atlassian.net/browse/OXY-97) — **Task** · Lower — Support explicitly naming FK + IDX`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Support explicitly naming FK + IDX
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-97
- **Checklist section:** To Do

## Expanded Description

**What it likely means:** Allow developers to supply an explicit database name for foreign-key constraints and indexes defined via `oxygen-sql` table annotations, instead of always using the auto-generated names.

Today the annotation surface in `modules/sql/core/src/main/scala/oxygen/sql/schema/annotations.scala:42-52` is:

```scala
final class foreignKey[Current, References](refs: (Current => Any, References => Any)*) extends StaticAnnotation
final case class references[References]() extends StaticAnnotation
class indexed extends StaticAnnotation
object indexed { class unique extends indexed }
class index[Current](cols: (Current => Any)*) extends StaticAnnotation
object index { class unique[Current](cols: (Current => Any)*) extends index[Current](cols*) }
```

None of these take a name. In `modules/sql/core/src/main/scala/oxygen/sql/generic/typeclass/DeriveTableRepr.scala` the derivation hard-codes `None` for the name in all four places:

* `foreignKeys.classForeignKey` — line 146: `ForeignKeyRepr[A, RT]( None, // TODO (KR) : allow for explicit fk naming`
* `foreignKeys.fieldForeignKey` — line 191: `ForeignKeyRepr[A, RT]( None, // TODO (KR) : allow for explicit fk naming`
* `indices.classIndex` — line 266: `IndexRepr[A]( None, // TODO (KR) : allow for explicit fk naming`
* `indices.fieldIndex` — line 295: `IndexRepr[A]( None, // TODO (KR) : allow for explicit idx naming`

The underlying models already fully support explicit names — `ForeignKeyRepr(explicitName: Option[String])` (`modules/sql/core/src/main/scala/oxygen/sql/schema/ForeignKeyRepr.scala:6`), `IndexRepr(explicitName: Option[String])` (`modules/sql/core/src/main/scala/oxygen/sql/schema/IndexRepr.scala:6`), `ForeignKeyState(explicitFKName: Option[String])` and `IndexState(explicitIdxName: Option[String])` with fallback to auto-names (`modules/sql/migration/src/main/scala/oxygen/sql/migration/model/ForeignKeyState.scala:8,17`, `IndexState.scala:8,17`), plus persistence of `fkNameIsExplicit`/`idxNameIsExplicit` (`conversion/domainToDb.scala:110,120`), diffing of explicit renames (`StateDiffer.scala:168,192`, `DiffApplier.scala:198-205`, `StateDiff.scala:209-213`), and SQL generation using the resolved name (`MigrationQueries.scala:73-78`). The only missing piece is wiring the annotation parameter through `DeriveTableRepr` to populate `explicitName`.

**Who it affects:** Any service defining Postgres tables via `oxygen-sql` (`TableRepr.derived`) that needs stable, human-readable constraint/index names — important for hand-written migrations, `psql \d` readability, error messages (`foreign_key_violation` references constraint name), and avoiding auto-name churn when tables/columns are renamed (auto names are `fk____<self>___<refs>____<col>__<col>` / `idx[_u]____<table>____<cols>` — verbose and derived from schema+table+columns, so renames change them unless explicitly named).

**Why it matters (Lower priority):** Not blocking, but without it the four `TODO (KR)` sites remain, migration diffs cannot express `RenameExplicitlyNamedForeignKey`/`RenameExplicitlyNamedIndex` from the annotation (only via manual `StateDiff`), and operators cannot give FK/IDX names that match existing DB conventions. Priority is Lower because auto-names work functionally — this is ergonomics + stability.

**Inferred acceptance criteria:**

* `@foreignKey` (class-level, composite FK) and `@references` (field-level, single-col FK) accept an optional explicit name that, when supplied, populates `ForeignKeyRepr.explicitName` / `ForeignKeyState.explicitFKName` so `fkName`/`ref` use it instead of `autoFKName`.
* `@index` / `@index.unique` (class-level, composite) and `@indexed` / `@indexed.unique` (field-level) accept an optional explicit name that populates `IndexRepr.explicitName` / `IndexState.explicitIdxName` so `idxName`/`ref` use it instead of `autoIdxName`.
* Backwards compatible: omitting the name keeps current auto-name behavior; no existing `@references[Person]` or `@foreignKey[...](...)` code breaks.
* Migration diff/planner respects the explicit name: creating a table with an explicit name emits that name; renaming changes are diffed as `RenameExplicitlyNamed*` when `explicit*Name` is present vs. drop+create for auto-named (already implemented in `StateDiffer` — just needs the derivation to feed it).
* Validation: duplicate explicit names on the same table are reported (or at least not silently colliding); empty/blank names are rejected at compile time.
* Tests: derivation unit tests + at least one integration/migration round-trip test (`MigrationGeneratorSpec` / `StateDifferSpec`) proving explicit names survive `TableRepr -> TableState -> ForeignKeyState/IndexState -> diff -> MigrationStepColumn -> MigrationQueries` and generate `CREATE INDEX <explicit_name>` / `CONSTRAINT <explicit_name>` SQL.

## Confidence
- **Rating:** 5 / 6 — strong evidence, one clear frontrunner

**Justification:**

* **Four code TODOs match title verbatim.** `DeriveTableRepr.scala:146,191` say `TODO (KR) : allow for explicit fk naming` and `:266,295` say `allow for explicit fk/idx naming` exactly where `None` is hard-coded for `explicitName`. No other interpretation of "Support explicitly naming FK + IDX" exists — this is not a guess, it is the documented gap.
* **End-to-end plumbing already exists and is verified.** `ForeignKeyRepr`/`IndexRepr` carry `Option[String] explicitName`, `ForeignKeyState`/`IndexState` carry `explicitFKName`/`explicitIdxName` with `fkName`/`idxName = explicit.getOrElse(auto...)`, persistence stores the explicit flag, `StateDiffer`/`DiffApplier` handle `RenameExplicitlyNamed*` vs auto `RenameAutoNamed*`/`Drop*`, and `MigrationQueries` emits `idx.idxName`/`fkRef.fkName`. The only missing wiring is annotation -> derivation (exactly what the TODOs mark).
* **No competing interpretation.** Sibling issues are distinct: `OXY-123` is DB-schema compat checking, `OXY-13` is auto-joins, `OXY-94` is `on conflict`, `OXY-96` is prepared-statement caching — none overlap with naming FK/IDX. Search for `@foreignKey`/`@references`/`@indexed`/`@index` in the repo finds only the intended annotation sites.
* **Why not 6:** No skipped test or explicit Jira design doc was retrieved (Jira URL not fetched), and the exact annotation syntax (parameter position / constructor shape) is not spelled out in the TODOs, so the API shape is inferred rather than copied from a spec.

## Required Changes (only if Confidence >= 3)

> All paths repo-grounded; mark verified vs. inferred.

**Verified present (no change needed):**

* [x] `modules/sql/core/src/main/scala/oxygen/sql/schema/ForeignKeyRepr.scala` — `explicitName: Option[String]` + `Built` — verified
* [x] `modules/sql/core/src/main/scala/oxygen/sql/schema/IndexRepr.scala` — `explicitName: Option[String]` + `Built` — verified
* [x] `modules/sql/migration/src/main/scala/oxygen/sql/migration/model/ForeignKeyState.scala` — `explicitFKName`, `fkName = explicit.getOrElse(autoFKName)`, `fromRepr` — verified
* [x] `modules/sql/migration/src/main/scala/oxygen/sql/migration/model/IndexState.scala` — `explicitIdxName`, `idxName = explicit.getOrElse(autoIdxName)`, `fromRepr` — verified
* [x] `modules/sql/migration/src/main/scala/oxygen/sql/migration/persistence/conversion/domainToDb.scala:110,120` + `dbToDomain.scala:125,134` — `fkNameIsExplicit`/`idxNameIsExplicit` persistence — verified
* [x] `modules/sql/migration/src/main/scala/oxygen/sql/migration/model/StateDiff.scala:209-213` + `delta/StateDiffer.scala:168,192` + `delta/DiffApplier.scala:198-205` — explicit rename handling — verified
* [x] `modules/sql/migration/src/main/scala/oxygen/sql/migration/persistence/MigrationQueries.scala:73-78` — `CREATE INDEX ${idx.idxName}` / `DROP CONSTRAINT ${fkRef.fkName}` — verified

**To implement (repo-grounded):**

* [ ] `modules/sql/core/src/main/scala/oxygen/sql/schema/annotations.scala` — add optional name parameter to FK/IDX annotations, backwards-compatible (verified shape today has no name; inferred new shape — see Open Questions for options). Minimal proposal (keeps existing call sites compiling):
  ```scala
  // Option A — name as second param list with default (most compatible with varargs):
  final class foreignKey[Current, References](refs: (Current => Any, References => Any)*)(val name: String = "") extends StaticAnnotation
  final case class references[References](name: String = "") extends StaticAnnotation
  class indexed(val name: String = "") extends StaticAnnotation
  object indexed { class unique(override val name: String = "") extends indexed(name) }
  class index[Current](cols: (Current => Any)*)(val name: String = "") extends StaticAnnotation
  object index { class unique[Current](cols: (Current => Any)*)(override val name: String = "") extends index[Current](cols*)(name) }
  ```
  Alternative (inferred): single-param-list with named arg `name: String = ""` after varargs if Scala 3 annotation permits it; or a dedicated `@fkName`/`@idxName` annotation. Pick one and keep it consistent across all four FK/IDX annotations. Extract via `Expr` quasiquotes in `DeriveTableRepr` (requires `FromExprT` or manual pattern match on `Expr[foreignKey]` etc.). *Inferred — needs validation against Scala 3 annotation varargs rules.*
* [ ] `modules/sql/core/src/main/scala/oxygen/sql/generic/typeclass/DeriveTableRepr.scala` — wire the name through in all four places:
  * [ ] `foreignKeys.classForeignKey` (line ~146): parse `expr: Expr[foreignKey[A, RT]]` to extract name (e.g. `case '{ new foreignKey[A, RT](_*)(${_}) }` or via `TypeRepr` inspection), convert `""` -> `None` else `Some(name)`, pass to `ForeignKeyRepr[A, RT](explicitName, ...)`
  * [ ] `foreignKeys.fieldForeignKey` (line ~191): same for `Expr[references[RT]]`
  * [ ] `indices.classIndex` (line ~266): same for `Expr[index[A]]` / `index.unique[A]`
  * [ ] `indices.fieldIndex` (line ~295): same for `Expr[indexed]` / `indexed.unique`
  * Add compile-time validation: blank/empty name treated as `None`; non-blank name validated as valid Postgres identifier (optional, but prevents `CREATE CONSTRAINT ""`); duplicate explicit names on one table -> `report.errorAndAbort`.
* [ ] `modules/sql/core/src/test/scala/oxygen/sql/schema/DeriveTableReprSpec.scala` (or `modules/sql/core/src/test/scala/oxygen/sql/DeriveTableReprSpec.scala` / new spec) — add derivation tests: `TableRepr.derived` on a table annotated with explicit FK/IDX names populates `foreignKeys.head.explicitName == Some("my_fk")` and `indices.head.explicitName == Some("my_idx")`, and auto-name still works when omitted. Also test `unique` flag preserved with explicit name. *Inferred — check existing test layout first; may be `modules/sql/it-test/src/test/scala/oxygen/sql/TableReprSpec.scala`.*
* [ ] `modules/sql/migration/src/test/scala/oxygen/sql/migration/StateDifferSpec.scala` / `MigrationGeneratorSpec.scala` — add diff tests: changing an explicit name diffs as `RenameExplicitlyNamedForeignKey`/`RenameExplicitlyNamedIndex` (AlreadyExists/DoesNotExist errors as in `DiffApplier`), and a new explicit FK/IDX creates `CreateForeignKey`/`CreateIndex` with that name.
* [ ] `modules/sql/it-test/src/test/scala/oxygen/sql/queries.scala` or `modules/sql/it-test/src/test/scala/oxygen/sql/migration/MigrationFsSpec.scala` — add one example table using explicit names (e.g. `@foreignKey[Child, Parent]((_.parentId, _.id))(name = "fk_child_parent")` and `@indexed(name = "idx_child_email")`) and assert generated migration SQL contains that name.
* [ ] `docs/docs/sql/models.md` or `docs/docs/sql/migrations.md` — document the new `name` parameter, show before/after examples, explain auto-name vs explicit-name trade-off, and note that renaming an explicit name is a `RenameExplicitlyNamed*` migration (backwards-compatible) while renaming a column referenced by an auto-named FK/IDX implicitly renames the auto-name (drop+create).
* [ ] `modules/sql/core/src/main/scala/oxygen/sql/schema/annotations.scala` — add Scaladoc on each modified annotation explaining the `name` parameter, that it controls `CONSTRAINT`/`INDEX` names, and that empty means auto-named.

**Out of scope (but note):**

* Changing auto-name generation logic itself (`ForeignKeyState.AutoRef.autoFKName`, `IndexState.AutoRef.autoIdxName`) — not needed; only wiring.
* Supporting explicit naming for PK constraints (`@primaryKey`) — separate issue.

## Estimates & Autonomy (only if Confidence >= 3)

* **Story points:** 3
  * *Justification:* Touch surface is tiny (1 annotation file + 1 derivation file with 4 sites), model/migration/query layers already done, change is purely wiring + validation + tests/docs. Fits Fibonacci 3 (small macro change with test coverage); 2 if annotation syntax is trivial, 5 if varargs annotation parsing proves awkward and requires a new annotation type.*

* **Autonomy:** 5 / 6 — mostly autonomous, just run it
  * *Justification:* Task is isolated to `modules/sql/core` (annotation + `DeriveTableRepr`) plus adding tests/docs; no cross-module or product ambiguity beyond parameter shape. TODO comments are the spec; existing `ForeignKeyRepr`/`IndexRepr` plumbing makes the target behavior obvious. Only minor style decision (name param position) benefits from a quick check.*

* **Ambiguity-to-resolve:** 2 / 6 — low, minor decisions before start
  * *Justification:* Intent is clear from TODOs and existing `explicitName` plumbing; only open is the exact annotation API shape (single vs. dual param list, `name: String` vs `Option[String]`, or separate `@fkName` annotation) and whether to validate identifier syntax. 10-minute code-owner check resolves it; no product/design blocker.*

## Open Questions

1. **Annotation API shape:** Should the name be `(refs: ...)(name: String = "")` (two param lists, most varargs-friendly), `name: String = ""` as a named argument after varargs, or a separate annotation like `@fkName("my_fk")` / `@idxName("my_idx")`? The TODOs don't specify; the choice must keep `@references[Person]` and `@foreignKey[MultiPK2, MultiPK1]((_.id1Ref, _.id1), ...)` compiling without modification. Recommend two param lists with default `""` for backwards compat — confirm with code owner.
2. **Field-level vs class-level FK naming:** `@references[Person]` is field-level (one FK per field). Does the team want to also allow `@references[Person](name = "fk_note_person")` on that field, or only class-level `@foreignKey` explicit names? Both should be supported for consistency, but the per-field case is less commonly needed — worth confirming.
3. **Duplicate/blank validation:** Should duplicate explicit FK/IDX names on the same table be a compile error (`report.errorAndAbort`) or a runtime `StateDiffError`? Compile-time is friendlier but requires `DeriveTableRepr` to track seen names. Should empty string be silently treated as auto-named or rejected?
4. **Postgres identifier limits:** Names are capped at 63 bytes in Postgres. Should derivation truncate/error, or leave it to migration-time Postgres error? Existing auto-names can already exceed the limit (`fk____...` with long table/column names) — not fixing here, but explicit names should be validated if the team wants.
5. **Docs placement:** `docs/docs/sql/models.md` vs `migrations.md` — where should explicit naming be documented? Recommend `models.md` for annotation usage + `migrations.md` for the resulting migration diff behavior.
