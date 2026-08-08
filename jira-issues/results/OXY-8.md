# OXY-8 — Add compatibility checking to db migrations

## Original
- **Key:** OXY-8
- **Checklist line:** `- [ ] [OXY-8](https://kr-oxygen.atlassian.net/browse/OXY-8) — **Task** · High — Add compatibility checking to db migrations`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Add compatibility checking to db migrations
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-8
- **Checklist section:** To Do

## Expanded Description

**What it likely means:** Add automated classification of schema/migration diffs as `BackwardsCompatible` vs `Incompatible`, drive semver bumps from that classification, and gate incompatible (breaking/data-loss) migrations behind an explicit opt-in so they cannot be written accidentally.

In the current codebase this corresponds to the `oxygen-sql` migration subsystem (`modules/sql/migration/`):

* **Classify every `StateDiff`** — additive changes (create table/schema/extension, add nullable column, add non-unique index, drop index/FK) are backwards-compatible; destructive or constraint-adding changes (drop table/column/schema, add non-nullable column, tighten to `NOT NULL`, add unique index or FK to an existing table, rename table/column/schema) are incompatible.
* **Bump versions accordingly** — genesis → `1.0.0`, incompatible → major bump, backwards-compatible → minor bump.
* **Gate writes** — `MigrationCheck` with `OXYGEN_MIGRATION_ALLOW_UPDATE` / `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE` env vars: CI (`allowUpdate=false`) reports `PendingUpdate`/`BlockedIncompatible` instead of writing; locally an incompatible migration requires both flags. This is the "avro paradigm" guard spec (`DbMigrationSpec` / `MigrationCheckSpec`).
* **Persist classification** — `PersistedMigrationFile.compatibility` field so history records how each version was classified.

**Who it affects:** Any service that owns Postgres tables via `oxygen-sql` and uses filesystem-first migrations (`MigrationSchema` / `MigrationService`). Without this, a developer could commit a breaking change (e.g., adding a `NOT NULL` column without a default) that fails on databases with existing rows.

**Why it matters (High priority):** Prevents silent data-loss / deploy-time failures; makes the cost of a schema change visible at code-review time via the version bump and the required env flag.

**Inferred acceptance criteria:**

* `MigrationGenerator.classify` covers all `StateDiff` variants, with the special case that constraints on a table created in the same migration stay compatible (table is empty).
* `MigrationGenerator.bump` maps compatibility to semver correctly.
* `MigrationCheck.check` returns `BlockedIncompatible` when `compatibility == Incompatible && !allowIncompatible`, and `Wrote` only when allowed.
* `PersistedMigrationFile` stores `compatibility` and round-trips via JSON.
* Guard spec (`DbMigrationSpec`) surfaces the outcome in tests (pass / `PendingUpdate` / `BlockedIncompatible`).
* Docs (`docs/docs/sql/migrations.md` — Versioning & compatibility) describe the table of changes.

## Confidence
- **Rating:** 5 / 6 — strong evidence, one clear frontrunner.

**Justification:**

* **Code matches title verbatim.** `modules/sql/migration/src/main/scala/oxygen/sql/migration/MigrationGenerator.scala:52-92` implements `classify`/`classifyOne` with the exact backwards-compatible vs incompatible table from the docs; `persistence/model/MigrationCompatibility.scala:11-13` defines the enum; `MigrationCheck.scala:69-72` gates writes on it. This is not a guess — the implementation is complete.
* **Tests lock the contract.** `modules/sql/it-test/src/test/scala/oxygen/sql/migration/MigrationGeneratorSpec.scala:40-71` asserts `BackwardsCompatible` for nullable-column adds (minor bump) and `Incompatible` for non-nullable adds (major bump); `MigrationCheckSpec.scala:40-77` asserts the `BlockedIncompatible` lifecycle.
* **Docs describe the feature as shipped.** `docs/docs/sql/migrations.md` (Versioning & compatibility) lists the same classification table and the `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE` requirement, plus the "No default values yet" caveat.
* **No competing interpretation.** Sibling `OXY-123` ("Add compatibility spec for DB Schema") and `OXY-29`/`OXY-38` (HTTP/endpoint schema compat) are distinct — they are about `oxygen.schema.compat` for API schemas, not migration diff classification, so they do not steal this issue's meaning. No `TODO`/`FIXME` in the migration module asks for missing compat work.

## Required Changes (only if Confidence >= 3)

> **Note on current state:** The feature described above is already implemented and tested. If OXY-8 is still marked To Do, the remaining work is verification/closure rather than green-field implementation. The list below covers what was built (verified) and what — if anything — remains.

**Verified present (no change needed):**

* [x] `modules/sql/migration/persistence/model/MigrationCompatibility.scala` — `BackwardsCompatible` / `Incompatible` enum + `JsonCodec` — verified.
* [x] `modules/sql/migration/MigrationGenerator.scala` — `classify`, `classifyOne` (all `StateDiff` variants + `createdTables` context), `bump`, `GenerateResult` — verified.
* [x] `modules/sql/migration/MigrationCheck.scala` — `Config` with `OXYGEN_MIGRATION_ALLOW_UPDATE` / `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE`, `Outcome.PendingUpdate` / `BlockedIncompatible`, gating logic — verified.
* [x] `modules/sql/migration/persistence/model/PersistedMigrationFile.scala` — `compatibility` field + `currentFormatVersion` — verified.
* [x] `modules/sql/test-utils/src/main/scala/oxygen/sql/test/DbMigrationSpec.scala` — guard-spec harness that surfaces compatibility outcomes — verified.
* [x] `docs/docs/sql/migrations.md` — Versioning & compatibility section — verified.
* [x] Tests: `modules/sql/it-test/src/test/scala/oxygen/sql/migration/MigrationGeneratorSpec.scala`, `MigrationCheckSpec.scala`, `MigrationFsSpec.scala`, `PersistedMigrationFileSpec.scala` — verified.

**If closing the ticket, do:**

* [ ] Confirm with assignee/PO that the implemented classification table matches the intended product contract (especially the `createdTables` exception and the "no default values yet" limitation noted in docs).
* [ ] Decide disposition of `OXY-123` vs `OXY-8` — `OXY-123` ("compatibility spec for DB Schema") may be the follow-up for richer `oxygen.schema.compat`-style diffing on DB schemas, while `OXY-8` is the migration-level gate. Document the split so `OXY-8` can be closed independently.

**If the ticket is interpreted as asking for MORE than the current binary gate (possible extensions — inferred, not verified):**

* [ ] `modules/sql/migration/MigrationGenerator.scala` — add default-value awareness so `CreateColumn` with a DB default is no longer forced `Incompatible` (docs note this as planned). Requires `ColumnState` to carry default info and `classifyOne` to branch on it.
* [ ] `modules/sql/migration/MigrationService.scala` — consider runtime compatibility enforcement (refuse to auto-apply `Incompatible` migrations without an explicit flag), if product wants apply-time gating in addition to generation-time gating. Currently `applyMigrations` applies whatever is on disk.
* [ ] `modules/sql/migration/` — richer compatibility levels (e.g., `Compatible` / `BackwardsCompatible` / `ForwardsCompatible` / `Incompatible` mirroring `oxygen.schema.compat.ComparisonResult`) if finer-grained semver or rollout policy is desired.
* [ ] Tests + docs for any extension above.

## Estimates & Autonomy (only if Confidence >= 3)

* **Story points:** 1 — if disposition is "already done, verify and close" (no code). 5 — if the default-value-aware extension is in scope (requires model change + classify update + tests/docs). 8 — if richer multi-level compat or runtime gating is required.
  * *Justification:* The binary gate is a small, pure-logic feature with narrow blast radius; extensions touch the same pure layer but add data-model and cross-cutting test/doc work.*

* **Autonomy:** 4 / 6 — an agent can verify the existing implementation, run the migration it-tests, and close or file a follow-up with little supervision; any extension (defaults, multi-level compat) needs a 1-2 question product check on the desired semantics before coding.
  * *Justification:* Code is isolated (`migration/` module, no DB needed for generation tests), conventions are clear; ambiguity is limited to whether the ticket is already done vs. wants an extension.*

* **Ambiguity-to-resolve:** 3 / 6 — moderate; the core classification is unambiguous from code/docs, but whether OXY-8 is considered DONE (and how it relates to `OXY-123`) must be confirmed before closing, and the "default values" caveat is an open product decision that changes scope.
  * *Justification:* Title is terse; without Jira body, the exact DONE bar (binary gate only vs. plus defaults/multi-level/runtime gating) is inferred from code rather than stated.*

## Open Questions

* Is `OXY-8` now **Done** and only needs checklist/Jira closure, with `OXY-123` carrying the remaining DB-schema compat-spec work? Or is `OXY-8` expected to also cover the default-value-aware `NOT NULL` handling noted as "No default values yet" in the docs?
* Should incompatible migrations also be gated at **apply time** (`MigrationService`) or is generation-time gating (`MigrationCheck`) sufficient? Current code only gates at generation.
* Is the binary `BackwardsCompatible`/`Incompatible` sufficient, or is a richer lattice (e.g., forward-compatible, fully compatible) desired mirroring `oxygen.schema.compat`?
* Does `OXY-123` subsume or duplicate `OXY-8`? Clarify ownership so the two do not drift.
