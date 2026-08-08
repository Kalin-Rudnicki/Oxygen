# OXY-15 — Add support for running an effect as a db migration step

## Original
- **Key:** OXY-15
- **Checklist line:** `- [ ] [OXY-15](https://kr-oxygen.atlassian.net/browse/OXY-15) — **Task** · Low — Add support for running an effect as a db migration step`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Add support for running an effect as a db migration step
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-15
- **Checklist section:** To Do

## Expanded Description

**What this issue likely means:** Extend the `oxygen-sql-migration` system so that a migration can include an *arbitrary application effect* (a `ZIO[Database, E, Unit]` or similar) as one of its versioned, exactly-once steps — alongside the auto-derived DDL diffs (`StateDiff` / `MigrationStepColumn`).

Today every migration step is a pure schema-DDL diff derived by `MigrationPlanner.diffStates` / `MigrationGenerator` from the difference between two `MigrationState` snapshots. Steps are `MigrationStepColumn.StateDiff` variants (create/drop/rename table, column, index, FK, schema, extension) serialized into `PersistedMigrationFile.diff` as JSON and turned into SQL at apply time via `MigrationQueries.diffToQuery` → `MigrationRepo.evalStep` → `Database` execution. There is no way to express a data migration (backfill, seed, re-encode, fixup), a custom SQL statement beyond the `StateDiff` vocabulary, or any ZIO logic as a versioned migration step tracked in the `oxygen_migration.migrations` / `migration_steps` execution log.

An "effect as a db migration step" would let a developer declare something like:

```scala
// hypothetical API — exact shape TBD
MigrationSchema.of(UserRow, PostRow)
  .withEffect("backfill display_name") { db => /* ZIO[Database, E, Unit] */ }
```

or provide a `Seq[CustomMigrationStep]` to `MigrationService` / `MigrationGenerator`, so that the effect runs:
- transactionally (respecting `MigrationConfig.Atomicity` — `AllOrNothing` / `PerMigration` / `None`),
- exactly once (recorded in `ExecutedMigrationStepRow` / `ExecutedMigration` so re-running `migrate` skips it),
- in a deterministic order relative to DDL steps,
- with failure surfacing as a `MigrationError` variant that aborts the migration and, depending on atomicity, rolls back the enclosing transaction.

**Who it affects:** Any service using `oxygen-sql-migration` that needs data migrations or custom logic tied to a schema version — e.g., backfilling a newly added nullable column, seeding reference data after `CreateTable`, or running a one-off fixup that must be versioned and auditable rather than ad-hoc.

**Why it matters:** The filesystem-first migration system currently only handles *structural* schema changes. Real-world schema evolution routinely requires *data* changes alongside structure (add column + backfill, split table, re-encode JSON). Without effect steps, teams must run such logic outside the migration framework (manual scripts, separate jobs) with no exactly-once guarantee, no ordering relative to DDL, and no shared transaction.

**Inferred acceptance criteria (from codebase):**
- A new step representation for effect/code steps coexists with `MigrationStepColumn.StateDiff` — either a new `MigrationStepColumn` variant or a parallel `MigrationStep` ADT that the `PersistedMigrationFile` / `ExecutedMigrationStepRow` and `MigrationRepo` understand.
- Effects are declared alongside `MigrationSchema` (or passed to the generator/service) and survive the generate → persist → apply round-trip. Because ZIO values are not JSON-serializable, the persistence story must be designed — e.g., an opaque name/key stored in JSON that resolves to code at runtime, or an in-code registry separate from the JSON files.
- `MigrationRepo.executeStep` / `MigrationService.applyMigrations` can execute both DDL steps and effect steps, respecting `MigrationConfig.Atomicity`.
- A new `MigrationError` variant covers effect-step failure.
- Existing DDL-only migrations remain backwards-compatible (no format break for `PersistedMigrationFile.currentFormatVersion = 1` unless intentionally bumped).
- Tests cover at least: effect step runs exactly once, is skipped on re-migration, failure aborts and respects atomicity, and ordering relative to DDL steps is deterministic.
- Docs in `docs/docs/sql/migrations.md` describe the new capability.

**Key design tension (verified):** `PersistedMigrationFile` is JSON-persisted (`JsonCodec` derived, `formatVersion = 1`, stored via `MigrationFs` / `MigrationCodecs`). `MigrationStepColumn` variants are flattened to leaf-case-name keys for JSON. A `ZIO` effect cannot be serialized this way. Any implementation must decide where effect *identity* lives (JSON file vs. code registry) and how drift/reconciliation (`MigrationService.reconcile` / `matches`) handles effect steps whose code may change between deploys.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title is 10 words, but the phrase "effect as a db migration step" is unusually precise in a ZIO codebase where "effect" means `ZIO` — narrowing scope to `oxygen-sql-migration` (`modules/sql/migration/`) and to a code-step interleaved with DDL steps, rather than a generic DB-migration improvement.
  - Code signal is strong for the *gap*: `MigrationStepColumn.StateDiff` covers only DDL, `MigrationRepo.evalStep` has a single `case diff: StateDiff` branch, `PersistedMigrationFile.Step` wraps only `MigrationStepColumn`, and no `TODO`/`FIXME` or existing effect-step type exists. This confirms the feature is absent and where it would plug in.
  - No Jira body could be fetched and no doc/comment describes the desired API or persistence strategy for effects. The central ambiguity — how to persist a non-serializable `ZIO` value across the filesystem-first JSON boundary — has no precedent in the current code, so the exact contract is inferred, not verified.
  - Remaining uncertainty is product scope (data-migration only vs. arbitrary ZIO, JSON-identity vs. code-registry, ordering, rollback of effects) which keeps rating at 3 rather than 4–5, but the module and general shape are clear enough to pass the threshold.

## Required Changes
- **Module ownership:** `oxygen-sql-migration` (`modules/sql/migration/`) is primary owner; `oxygen-sql-test` for test harness changes; `docs/docs/sql/migrations.md` for docs. No other `oxygen-*` modules need direct changes unless a shared `MigrationStep` ADT is promoted.
- **Step model (`persistence/model`):**
  - [ ] Extend the step ADT to represent effect/code steps. Options (needs decision): (a) add a new `MigrationStepColumn` variant (e.g. `RunEffect(name: String, description: Option[String])`) whose JSON stores only an opaque identity, with resolution to a `ZIO` at apply time via a registry; (b) introduce a parallel top-level `MigrationStep` sealed trait with `DDL(StateDiff)` and `Effect(...)` cases that `PersistedMigrationFile.Step` wraps instead of `MigrationStepColumn` directly. — *inferred; verify existing JSON codec flattening (`leaf case names`) still works for the new variant.*
  - [ ] If option (a), add a `TypedJsonb[MigrationStepColumn]` serialization test for the new variant in [`PersistedMigrationFileSpec.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/it-test/src/test/scala/oxygen/sql/migration/PersistedMigrationFileSpec.scala) and verify `MigrationCodecs` handles it.
  - [ ] Decide `formatVersion` bump: adding a new variant is backwards-compatible for readers that ignore unknown variants only if the codec is configured permissively — otherwise bump `PersistedMigrationFile.currentFormatVersion` from `1` to `2`. — *verified: [`PersistedMigrationFile.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/persistence/model/PersistedMigrationFile.scala:22) documents formatVersion as envelope bump for incompatible on-disk shape.*
- **Schema / generator (`model` / `delta`):**
  - [ ] Allow declaring effects alongside `MigrationSchema`. Plausible API: `MigrationSchema.of(...).withEffects(...)` or a separate `CustomMigrationSteps` parameter to `MigrationGenerator.generate` / `MigrationState`. The generator must interleave effect steps with DDL diffs in a deterministic `applicationOrder` (e.g. effects run after the DDL they depend on). — *verified: [`MigrationSchema`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/model/MigrationSchema.scala:15) currently holds only `tables: ArraySeq[TableRepr[?]]` and has no effect/custom-step field; [`MigrationGenerator.generate`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/MigrationGenerator.scala:23) diffs only `MigrationState` snapshots.*
  - [ ] Update `MigrationPlanner.orderDeterministically` / `StateDiff.applicationOrder` or introduce a parallel ordering for effect steps so that mixed DDL + effect sequences are deterministic and duplicate detection still works.
  - [ ] Decide whether effect steps contribute to `MigrationCompatibility` classification and `MigrationGenerator.bump` (likely not — data migrations are runtime concerns, not schema-compatibility concerns — but must be explicit).
- **Persistence / execution (`persistence/MigrationRepo.scala`):**
  - [ ] Extend `MigrationRepo.evalStep` (currently a single `case diff: StateDiff => diffToQuery` branch in [`MigrationRepo.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/persistence/MigrationRepo.scala:53)) to handle effect steps — e.g. `case eff: RunEffect => (lookupEffect(eff.name).unit.mapError(...), None)` where the `ZIO` is resolved from a registry/layer.
  - [ ] Design the effect registry: either a `Map[String, ZIO[Database, E, Unit]]` provided as a layer/service to `MigrationService` / `MigrationRepo`, or a `MigrationEffectResolver` trait. Must be available at apply time (`MigrationService.applyMigrations`) even though the JSON file was written at generation time.
  - [ ] Ensure `ExecutedMigrationStepRow` recording works for effect steps (`sql: Option[String]` would be `None` for effects; `step: TypedJsonb[MigrationStepColumn]` stores the identity). Verify `matches` / `reconcile` in [`MigrationService.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/MigrationService.scala:58) handles effect-step equality correctly (opaque name match is sufficient; code equality is not required).
- **Service (`MigrationService.scala`):**
  - [ ] Thread the effect registry through `MigrationService` (new constructor param or `ZLayer` input) and expose it via `migrateVerified` / `applyMigrations` overloads or a new `migrateWithEffects` method.
  - [ ] Ensure `MigrationConfig.Atomicity` semantics apply to effect steps (effects inside `PerMigration` or `AllOrNothing` transactions; consider whether effects that do their own transaction control need special handling).
  - [ ] Handle effect-step failure: new `MigrationError` variant (e.g. `ErrorExecutingEffectStep(version, step, cause)`) that aborts the migration, with transaction rollback per atomicity.
- **Error handling (`model/MigrationError.scala`):**
  - [ ] Extend `MigrationError` with effect-specific cases (e.g. `ErrorExecutingEffectStep`, `UnknownEffectName`). — *verified: current sealed trait has cases for `ErrorReadingMigrationFiles`, `InvalidMigrationVersion`, `MissingMigration`, `MigrationsDiffer`, `OutOfOrderMigration`, `MigrationsStale`, `ErrorInitiatingMigrations`, `ErrorPersistingMigration`, `ErrorFetchingMigrations`, `ErrorExecutingMigrationStep` — no effect variant.*
- **Tests:**
  - [ ] Add unit tests for JSON round-trip of the new step variant.
  - [ ] Add integration tests in `modules/sql/it-test` akin to [`MigrationSpec.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/it-test/src/test/scala/oxygen/sql/MigrationSpec.scala:14) covering: declare schema + effect step → generate → apply → verify effect ran exactly once → re-apply is no-op; effect failure aborts and respects `AllOrNothing` vs `PerMigration`.
  - [ ] Add `DbMigrationSpec` / `MigrationCheck` interaction test if effects affect filesystem verification.
- **Docs:**
  - [ ] Update `docs/docs/sql/migrations.md` with an "Effect / data migration steps" section: how to declare an effect step, how it is persisted/identified, ordering, atomicity, failure semantics, and the non-serializable-code caveat (effect code must be present at apply time). — *verified: current doc is DDL-only and describes the filesystem-first flow.*
- **Verified vs. inferred:** That DDL steps are the only current step type, that `MigrationRepo.evalStep` is single-branch, and that `PersistedMigrationFile` is JSON-persisted were verified by reading the files above. That the new feature is "effect = ZIO data migration interleaved with DDL" and the specific registry/persistence design are inferred from the title phrase plus ZIO idioms (no Jira body to confirm).

## Estimates & Autonomy
- **Story points:** 5 — Small-to-medium feature with a focused blast radius (`oxygen-sql-migration` only) but a non-trivial design decision at the JSON/code boundary. DDL-only path is well-established; adding a second step kind touches the step ADT, codec, generator ordering, repo execution, service wiring, error model, and tests/docs. Smaller than a multi-module epic, larger than a pure doc/config task; Low priority in checklist reflects product priority not size.
- **Autonomy:** 3 / 6 — An agent could scaffold the ADT extension, codec, repo branch, and service wiring autonomously using the well-structured existing code (clear patterns in `MigrationStepColumn`, `MigrationQueries`, `MigrationRepo`, `MigrationService`). Product decisions about API shape (where effects are declared, registry vs. inline, ordering, whether effects affect versioning) require human confirmation before committing to an implementation; the JSON-identity persistence design in particular needs a 2–3 question check.
- **Ambiguity-to-resolve:** 4 / 6 — Key ambiguities must be resolved before implementation starts (see Open Questions). The codebase strongly constrains *where* to build it, but not *how the effect is identified across the filesystem boundary* or *what the developer-facing API should be*.
- **Justification:** Title is terse and no Jira body was available to narrow the contract. The module is clear, the gap is clear, but the central persistence trick (code in JSON) has no precedent in the current design and admits multiple viable solutions with different tradeoffs.

## Open Questions
1. **API shape — where are effects declared?** On `MigrationSchema` (e.g. `.withEffect(name)(zio)`), as a separate `Seq[CustomStep]` passed to `MigrationGenerator` / `MigrationService`, or as a `MigrationEffectResolver` layer? The title gives no hint.
2. **Persistence — how is a ZIO value identified in JSON?** Effects are not serializable. Should the JSON store an opaque `name`/`key` that resolves to code at apply time via a registry (requiring the code to be present at deploy), or is there a different expectation (e.g., effect is re-evaluated from code on every startup, not from the file)? This is the core design decision.
3. **Ordering — where do effect steps run relative to DDL?** After all DDL? Interleaved at declaration order? Ordered by a new `applicationOrder` tier? Must be deterministic and documented; current `StateDiff.applicationOrder` (0–18) has no slot for effects.
4. **Versioning — do effect steps affect `MigrationCompatibility` / semver bump?** Data migrations are typically runtime-only and should not force a major bump, but if an effect modifies schema-adjacent state this may matter. Title does not say.
5. **Effect type — how generic?** Is `ZIO[Database, E, Unit]` sufficient (has access to `Database` for queries), or should it be `ZIO[Database & OtherServices, E, Unit]` / `ZIO[Any, E, Unit]` so effects can use broader app layers (HTTP, config)? Narrow scope keeps it simple; broader scope may be needed for seed logic that calls external services.
6. **Reconciliation — what if effect code changes after the migration file was written?** `MigrationService.matches` currently compares stored `diff` steps exactly. For effects, should reconciliation compare only the opaque name (allowing code to evolve) or also a hash of the effect? Must decide to avoid spurious `MigrationsDiffer` on redeploys.
7. **Rollback — are effect steps reversible?** If OXY-7 (migration rollback) lands, should rolling back an effect step attempt to run an inverse effect or just delete the execution log row? Probably out of scope for OXY-15, but worth noting the interaction.
8. **Failure semantics — should a failed effect be retried?** On next `migrate`, should a failed effect step (where `startMigration` was recorded but `completeMigration` was not) be retried, skipped, or require manual intervention? Current forward-only apply has no partial-migration retry logic beyond transaction rollback.
