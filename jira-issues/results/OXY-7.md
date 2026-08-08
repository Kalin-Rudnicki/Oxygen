# OXY-7 — Support migration rollback

## Original
- **Key:** OXY-7
- **Checklist line:** `- [ ] [OXY-7](https://kr-oxygen.atlassian.net/browse/OXY-7) — **Task** · Low — Support migration rollback`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Support migration rollback
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-7
- **Checklist section:** To Do

## Expanded Description

**What this issue likely means:** Add the ability to *roll back* (revert / downgrade) database migrations that have already been applied by `oxygen-sql-migration`. Today the migration system is strictly forward-only: `MigrationService` loads `PersistedMigrationFile` JSONs from the filesystem, compares them to the `oxygen_migration.migrations` / `migration_steps` execution log in the DB, and applies any not-yet-executed migrations in version order. Drift errors (`MissingMigration`, `MigrationsDiffer`, `OutOfOrderMigration`) are intentionally fatal and there is an explicit `// TODO (KR) : support rollback` placeholder in [`MigrationService.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/MigrationService.scala:47). No `rollback`, `down`, `downgrade`, or `revert` API exists.

A rollback feature would let an operator (or test harness) undo the most recent migration(s) — either "roll back the last N migrations" or "roll back to a target version" — by computing and executing the inverse of the stored diffs, and by cleaning up the execution log so the DB state matches the filesystem state at the target version. Because every `PersistedMigrationFile` already stores a full `state: MigrationStateColumn` snapshot alongside its `diff` (see docstring in [`PersistedMigrationFile.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/persistence/model/PersistedMigrationFile.scala:14)), reverse diffing is possible without storing separate down-SQL: the planner can diff any two snapshots (e.g. `currentState -> previousState`) via the existing `MigrationPlanner.diffStates` / `StateDiffer` machinery.

**Who it affects:** Any service using `oxygen-sql-migration` for schema management, especially operators who need to recover from a bad migration in staging/production, and developers who want a faster local iteration loop than "reset the database" (the current `docs/docs/sql/migrations.md` guidance is to delete unreleased files and reset the DB).

**Why it matters:** Forward-only migrations leave no safe path for downgrading after a failed or incorrect deploy. The `MigrationsDiffer`/`MissingMigration` strictness that protects drift also means you cannot simply delete a bad migration file and re-deploy — the DB still believes it was executed. A rollback restores the DB to a previous consistent snapshot and unblocks both emergency recovery and local development workflows.

**Inferred acceptance criteria (from codebase):**
- `MigrationService` exposes a rollback operation (e.g. `rollbackTo(version)` / `rollbackLast(n)` or `rollback(targetVersion: Option[Version])`) that is filesystem-aware or DB-aware and symmetric to `applyMigrations`.
- Each `StateDiff` variant has a defined inverse, or rollback is implemented as a planner diff between two stored snapshots (current DB state vs. target snapshot), reusing `MigrationQueries.diffToQuery` for SQL generation and `MigrationRepo` for step execution.
- Rollback respects `MigrationConfig.Atomicity` (at minimum `PerMigration` and `AllOrNothing` semantics for the set of undo steps).
- `MigrationRepo` gains the ability to remove or mark-as-rolled-back rows in `oxygen_migration.migrations` and `migration_steps` for the undone version(s); `getMigrations` / `reconcile` must remain consistent after rollback.
- New `MigrationError` variants cover rollback-specific failures (e.g. `RollbackTargetNotFound`, `IrreversibleDiff`, or `RollbackFailed`).
- Tests cover at least: genesis rollback (undo creation), column add/drop rollback, and transactional atomicity of a multi-version rollback.
- Docs in `docs/docs/sql/migrations.md` are updated to describe the new capability and its interaction with drift detection and `OXYGEN_MIGRATION_ALLOW_*` guards.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Direct code signal: `// TODO (KR) : support rollback` in `MigrationService.scala:47` inside the `MigrationService` class immediately adjacent to the apply/migrate API makes the scope unambiguous (DB migration rollback, not transaction savepoint rollback).
  - Repo-wide search shows zero existing rollback/down API, no `Rollback` error variants, and no inverse diff logic — confirming this is a greenfield feature within the `oxygen-sql-migration` module, not a bug fix in an existing path.
  - `PersistedMigrationFile` docstring explicitly notes the snapshot "unlocks reverse / cross-version diffing later... rather than needing each drop-style diff to carry enough information to invert itself" — a deliberate design affordance for rollback.
  - Remaining uncertainty is product scope (target-version vs. last-N vs. both, CLI vs. programmatic-only, handling of irreversible/data-lossy inversions) which keeps rating at 4 rather than 5 or 6. No Jira description could be fetched to narrow that scope.

## Required Changes
- **Module ownership:** `oxygen-sql-migration` (`modules/sql/migration/`) is the primary owner; `oxygen-sql-test` for rollback test utilities; `docs/docs/sql/migrations.md` for documentation. No other `oxygen-*` modules need direct changes.
- **Data model / planner:**
  - [ ] Verify that running `MigrationPlanner.diffStates(current, target)` with arguments swapped produces a correct reverse diff for all `StateDiff.CanBeDerived` variants; if any `StateDiff` lacks a natural inverse (e.g. `DropTable` vs. `CreateTable` needing full table definition), confirm the snapshot-based approach supplies the needed detail (it does — `CreateTable` carries the full `TableState`). — *verified: `StateDiffer.phase1` and `phase2` cover all derived diffs and `MigrationQueries.diffToQuery` covers them; `PersistedMigrationFile.state` carries the full snapshot.*
  - [ ] Define what "irreversible" means for product purposes — e.g. `DropColumn` inverted is `CreateColumn` but data is lost; document that rollback is lossy for destructive changes rather than lossless re-creation. Decide whether rollback should be blocked or warn when the inverse contains data-lossy steps. — *inferred; needs product decision.*
  - [ ] Optionally add an explicit `invert(diff: StateDiff): StateDiff` helper or rely on snapshot diff; either approach should be tested for parity.
- **Persistence (`MigrationRepo`):**
  - [ ] Add repo operations to delete or archive rolled-back migrations: e.g. `rollbackMigration(version: Version): IO[MigrationError, Unit]` that deletes from `ExecutedMigrationStepRow` then `ExecutedMigrationRow` within the same transaction, and a `getMigrations` filter that reflects the rolled-back state. — *verified: `MigrationRepo.Live` in [`MigrationRepo.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/persistence/MigrationRepo.scala:27) currently only has `initialize`, `startMigration`, `completeMigration`, `executeStep`, `getMigrations`; no delete path.*
  - [ ] Add inverse execution path analogous to `evalStep` but for rollback steps, reusing `MigrationQueries.diffToQuery` on the reverse diffs.
- **Service (`MigrationService`):**
  - [ ] Add public API: e.g. `def rollbackTo(targetVersion: Option[Version]): IO[MigrationError, MigrationResult]` and `def rollbackLast(count: Int)` that (a) loads the executed migrations from the DB, (b) resolves the target snapshot from the filesystem (or from the DB's stored `PersistedMigrationFile` states if filesystem is canonical), (c) computes `MigrationPlanner.diffStates(currentState, targetState)`, and (d) executes the reverse steps in reverse `applicationOrder` (drop FKs/indices before dropping tables/columns). — *verified: `applyMigrations` and `reconcile` in [`MigrationService.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/MigrationService.scala:58) handle forward apply; no rollback path exists.*
  - [ ] Wire `MigrationConfig.Atomicity` semantics for rollback (all rollback steps in one transaction for `AllOrNothing`, per-migration for `PerMigration`).
  - [ ] Add ZIO service accessors and layer helpers mirroring `migrateUnverified` / `customMigrateUnverifiedLayer` for rollback (e.g. `MigrationService.rollbackTo(...)`).
- **Error handling:**
  - [ ] Extend `MigrationError` in [`MigrationError.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/migration/src/main/scala/oxygen/sql/migration/model/MigrationError.scala:8) with rollback-specific cases (e.g. `RollbackTargetNotFound`, `RollbackStepFailed`, `NothingToRollback`). — *verified: current sealed trait has 9 cases, none for rollback.*
- **Filesystem interaction:**
  - [ ] Decide whether rollback also deletes/downgrades filesystem files (likely not — filesystem is source-of-truth for forward history; rollback should only affect DB state so re-migration is possible) and document the choice. — *inferred from `MigrationFs` / `MigrationCheck` design where files are committed history.*
- **Tests:**
  - [ ] Add integration tests in `modules/sql/it-test` akin to [`MigrationSpec.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/sql/it-test/src/test/scala/oxygen/sql/MigrationSpec.scala:14) covering: apply v1 → apply v2 → rollback to v1 → verify data/schema, rollback of a backwards-compatible column add, and rollback atomicity on failure.
  - [ ] Add unit tests for inverse diff correctness (forward diff + reverse diff = empty).
- **Docs:**
  - [ ] Update `docs/docs/sql/migrations.md` with a "Rollback" section: API, CLI if any, atomicity, and warning about irreversible data loss. — *verified: current doc is forward-only and explicitly says to reset the DB to collapse history.*

## Estimates & Autonomy
- **Story points:** 8 — Moderate-to-large feature touching service, repo, planner/queriers, and error model plus new integration tests and docs. Forward path already established well-tested patterns, but rollback introduces branching product decisions (reversibility, atomicity, filesystem vs. DB source-of-truth) and careful transaction ordering. Smaller than an Epic, larger than a trivial task; Low priority in checklist reflects product priority not size.
- **Autonomy:** 3 / 6 — An agent could scaffold the mechanics (snapshot reverse diff, repo delete, service method) autonomously using the well-structured existing code, but product decisions about API shape (target version vs. last-N), whether to permit lossy rollbacks, and whether rollback touches the filesystem require human confirmation before committing to an implementation.
- **Ambiguity-to-resolve:** 4 / 6 — Key ambiguities must be resolved before implementation starts (see Open Questions). The codebase strongly constrains *how* to build it, but not *what the contract should be*.

## Open Questions
1. **API shape:** Should rollback be "to version X" (`rollbackTo(Version("1.0.0"))`), "last N" (`rollbackLast(1)`), or both? Should there be a CLI or only a programmatic `MigrationService` method? The checklist title gives no hint.
2. **Filesystem vs. DB source-of-truth:** After rollback, should the filesystem be modified (delete rolled-back files) or should rollback only revert DB state so a subsequent `migrate` can re-apply? Current forward model treats filesystem as committed history — DB-only rollback is the likely intent but unconfirmed.
3. **Handling data-lossy inverses:** `DropColumn` → `CreateColumn` normally requires a default for non-nullable columns, and dropped tables lose data. Should rollback be allowed for incompatible/destructive inverses, require a flag (analogous to `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE`), or be blocked with an error? No guidance in the codebase.
4. **Partial / intermediate state:** If a multi-migration rollback fails mid-way, what is the recovery story? Should `Atomicity.AllOrNothing` be the only supported mode for rollback, or should `PerMigration` leave the DB at an intermediate version?
5. **Interaction with drift checks:** After a rollback, `reconcile` would see filesystem files for the rolled-back versions that are now absent from the DB — they would be considered "to execute" and re-applied on next `migrate`. Is that desired (rollback is transient) or should those filesystem files be ignored/removed?
6. **Idempotency and concurrent callers:** Should `rollbackTo` be idempotent if already at target? Should it reject rollback if there are unapplied forward migrations pending?
7. **Migration execution log cleanup:** Should rolled-back steps remain as auditable rows (e.g. add a `rolledBackAt` column) or be hard-deleted? Hard delete is simpler and matches "re-apply" semantics, but audit history might be desired.
