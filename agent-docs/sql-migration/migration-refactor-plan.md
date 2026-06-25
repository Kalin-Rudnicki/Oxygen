# SQL Migration Refactor — Plan

> Status: **planning complete, ready to implement**
> Module: `modules/sql/migration/src/main/scala/oxygen/sql/migration`

## Goal

Stop requiring the **entire history of model classes** to live in code in order to
represent past migrations. Instead:

- Only the **latest** table definitions live in code.
- Migration **history is persisted to the filesystem** as already-calculated diffs.
- A new migration is produced by **diffing the current code against the latest
  persisted FS state**.

This is modeled on a proven "avro topic schema migration" paradigm: persisted
schema files managed by a test, gated by env vars, so CI fails when the persisted
representation is stale and a local run (with the env var set) performs the update.

## Core restructuring: split one responsibility into two flows

Today `MigrationService` + `MigrationPlanner.calculateMigrations` entangle
"replay code-defined history → calculate → apply". Split into:

- **Generate** (dev-time, the test): current code → diff against FS → write a new
  migration file. *This is where the diff engine runs now.*
- **Apply** (runtime): read FS migration files → execute the ones this DB hasn't
  run → record in DB.

History stops living in code and starts living on the FS as already-calculated diffs.

## Decisions locked

| Topic | Decision |
|---|---|
| FS format | **Snapshot per version + diff.** Each version file is self-contained (`PersistedMigrationFile`: formatVersion, version, previousVersion, compatibility, diff, state). Reverse / cross-version diffs are later computed by diffing two snapshots. See "Version file schema". |
| Identity | **`oxygen.core.Version`** (semver, total `Ordering`). Out-of-order support deferred. |
| Renames | **Deferred.** Treated as drop+add behind the incompatible gate. Real rename hints are a fast-follow. |
| Delivery | **Test-driven** (env-var gated), like avro. Core logic stays a library so a CLI (`oxygen-db migrate`) can wrap it later. |
| Versioning | **Auto-increment**, bump level derived from the compatibility classification (see below). |
| `current.json` | **Dropped from MVP.** Derivable from the highest version file's snapshot. May return later purely as a generated convenience / PR diff target. |

## FS layout

```
migrations/
  1.1.0.json     # PersistedMigrationFile (see "Version file schema")
  1.2.0.json
  2.0.0.json
```

- Each file is self-contained (full `MigrationState` snapshot + the diff that produced it).
- The **diff source** for generation = the highest version file's `state`.
- Version files are authoritative; nothing is hand-maintained alongside them.
- `formatVersion` envelope = cheap insurance so JSON-type-diffing / soft-delete
  metadata can land later without breaking committed files.
- **Serialization reuses the existing `persistence/model` column types**
  (`TableStateColumn`, `MigrationStepColumn.StateDiff`, etc.) and their JSON
  codecs — they already serialize these exact shapes for JSONB. No new codec layer.

## Version file schema

### Envelope

A new top-level case type (e.g. `PersistedMigrationFile`) wrapping mostly-existing
column models:

```
PersistedMigrationFile {
  formatVersion:    Int               // envelope schema version; starts at 1
  version:          String            // oxygen.core.Version, e.g. "1.2.0" (StringCodec)
  previousVersion:  String | null     // the version this diff applies on top of; null for genesis
  compatibility:    "BackwardsCompatible" | "Incompatible"   // from the classifier; explains the bump
  diff:             DiffStep[]         // ordered steps from previousVersion.state -> state
  state:            MigrationStateColumn   // full resulting snapshot after this migration
}

DiffStep {
  stepNo:  Int       // canonical order is array order; stepNo is explicit/robust
  derived: Boolean   // auto-generated step (e.g. phase-3 rename) vs. primary diff
  step:    MigrationStepColumn   // the StateDiff, leaf-keyed (see codec note)
}
```

New types to add:
- `MigrationStateColumn` — there is **no existing serializer for a whole
  `MigrationState`** (the DB stores per-step, not full snapshots). Small new type:
  `{ extensions: Set[String], schemas: Set[String], tables: ArraySeq[TableStateColumn] }`.
- `PersistedMigrationFile` — the envelope above.
- `compatibility` enum (also consumed by the gate + bump logic).

Everything below `state.tables[*]` and `diff[*].step` reuses the existing column
models verbatim (`TableStateColumn`, `ColumnColumn`, `MigrationForeignKeyColumn`,
`IndexColumn`, `EntityRefColumn.*`, `MigrationStepColumn`).

**Codec note (verified):** `MigrationStepColumn` has no `@jsonDiscriminator`, and
JsonCodec derivation defaults to `UnrollStrategy.Unroll` — so the nested sealed
hierarchy flattens to **leaf case names** as the single wrapper key:
`{ "CreateColumn": { ... } }`, not `{ "StateDiff": { "AlterColumn": { "CreateColumn": ... } } }`.
`ColumnColumn.Type` serializes as its SQL string (`"UUID"`, `"TEXT"`, `"TIMESTAMP WITH TIME ZONE"`).
`sql` is intentionally **not** stored on the FS — it's derivable from the diff via
`diffToQuery` and is only recorded in the DB execution log at apply time.

### Concrete example — `1.2.0.json`

Scenario: `1.1.0` created a `public.user` table; `1.2.0` adds a nullable
`display_name` column and a unique index on `email`.

```json
{
  "formatVersion": 1,
  "version": "1.2.0",
  "previousVersion": "1.1.0",
  "compatibility": "BackwardsCompatible",
  "diff": [
    {
      "stepNo": 0,
      "derived": false,
      "step": {
        "CreateColumn": {
          "tableRef": { "schema": "public", "table": "user" },
          "column": { "name": "display_name", "columnType": "TEXT", "nullable": true }
        }
      }
    },
    {
      "stepNo": 1,
      "derived": false,
      "step": {
        "CreateIndex": {
          "idx": {
            "idxName": "idx_u____public__user____email",
            "idxNameIsExplicit": false,
            "self": { "schema": "public", "table": "user" },
            "unique": true,
            "columns": ["email"]
          }
        }
      }
    }
  ],
  "state": {
    "extensions": [],
    "schemas": ["public"],
    "tables": [
      {
        "tableName": { "schema": "public", "table": "user" },
        "primaryKeyColumns": ["id"],
        "columns": [
          { "name": "id",           "columnType": "UUID", "nullable": false },
          { "name": "email",        "columnType": "TEXT", "nullable": false },
          { "name": "display_name", "columnType": "TEXT", "nullable": true }
        ],
        "foreignKeys": null,
        "indices": [
          {
            "idxName": "idx_u____public__user____email",
            "idxNameIsExplicit": false,
            "self": { "schema": "public", "table": "user" },
            "unique": true,
            "columns": ["email"]
          }
        ]
      }
    ]
  }
}
```

## Compatibility classifier (does double duty: gate + bump)

The same analysis decides both whether generation is allowed and how the version bumps.
A migration is `Incompatible` if **any** of its diffs is incompatible, else `BackwardsCompatible`.

### Finalized per-diff classification (CP4)

The classifier is **context-aware**: it first collects the set of tables *created in this same
migration* (from `CreateTable` diffs). Constraints on a freshly-created table can never fail
against existing data (the table is empty), so they stay compatible — this keeps a greenfield
genesis migration (tables + their unique indices + FKs) from being flagged incompatible.

| Diff | Compatibility |
|---|---|
| `CreateExtension`, `CreateSchema`, `CreateTable` | Compatible |
| `CreateColumn` — **nullable** | Compatible |
| `CreateColumn` — **non-nullable** | **Incompatible** |
| `SetNullable(true)` (relax) | Compatible |
| `SetNullable(false)` (tighten) | **Incompatible** |
| `CreateIndex` — non-unique | Compatible |
| `CreateIndex` — unique, on a **newly-created** table | Compatible |
| `CreateIndex` — unique, on an **existing** table | **Incompatible** |
| `CreateForeignKey` — on a **newly-created** table | Compatible |
| `CreateForeignKey` — on an **existing** table | **Incompatible** |
| `DropIndex`, `DropForeignKey` (no row-data loss) | Compatible |
| `DropTable`, `DropColumn`, `DropSchema` | **Incompatible** |
| `Rename{Schema,Table,Column}` (deferred; shouldn't appear in auto MVP) | **Incompatible** |
| `Rename{ExplicitlyNamed,AutoNamed}{ForeignKey,Index}` (constraint-name only) | Compatible |

Bump from the overall classification: **Incompatible → major**, **Compatible → minor**.

Notes:
- **patch** is unused at MVP. Reserved for later non-structural changes (index
  rebuilds, granular JSON-type tweaks).
- Bump is computed off the **highest-severity op in the whole diff** — one
  `DropColumn` makes the entire migration a major bump.
- Next version = highest existing version file, bumped by the overall classification;
  genesis (no prior files) = `1.0.0`.

> **TODO (post-MVP):** support column **default values**. Today a non-nullable
> `CreateColumn` is always `Incompatible` because a NOT NULL column with no default breaks
> existing rows / old inserts. With default-value support, a non-nullable add *with* a
> default could be reclassified as backwards-compatible.

## Env-var gates

- `OXYGEN_MIGRATION_ALLOW_UPDATE` — write the new migration file vs. fail-with-pending (CI behavior).
- `OXYGEN_MIGRATION_ALLOW_INCOMPATIBLE` — allow a destructive/breaking change.

(Exact env-var names TBD during implementation.)

## Local-dev collapse

A free property of diffing-from-FS, not a built feature:

- **Delete everything** → diff source is empty state → one genesis migration that
  creates the whole schema.
- **Delete just the unreleased tail** → diff source becomes the highest *remaining*
  snapshot → the deleted changes collapse into one consolidated migration.

**Guardrail — collapse is local/greenfield-only.** Once a real DB has recorded
executed migrations under versions that are then deleted, the FS desyncs from the
DB's execution log. The apply path must **detect and refuse** (today's
`MissingPlannedMigration` / `MigrationsDiffer` drift checks), never silently
misbehave. Local dev is fine because the DB is reset alongside the files; prod
simply never collapses.

## Phased work

**Phase 0 — Foundations (load-bearing, first)**
- Replace version identity `Int → oxygen.core.Version` across `PlannedMigration`,
  `CalculatedMigration`, `ExecutedMigration`, `ExecutedMigrationRow`, and the
  sequence/gap-check logic in `MigrationPlanner`/`MigrationService`. Ordering comes
  free from `Version.ordering`.
- Define the versioned FS file envelope; confirm column-model JSON codecs round-trip to files.

**Phase 1 — FS persistence component (`MigrationFs`)**
- List/parse version files into an ordered collection; load the latest snapshot;
  write a new version file.

**Phase 2 — Generation (`MigrationGenerator`)**
- Derive target state via `MigrationState.fromTables`; load latest FS snapshot;
  diff via `MigrationPlanner.diffStates`.
- Run the **compatibility classifier** → gate (allow/refuse) + bump (major/minor).
- Write the new version file when permitted, else fail reporting the pending migration.

**Phase 3 — Test harness**
- A reusable spec a project includes: given current tables + migration dir, assert
  up-to-date (CI fails on stale) or update locally when the env var is set.

**Phase 4 — Apply path**
- Rework `MigrationService.migrate` to consume FS migrations instead of code
  `Migrations`; key DB execution tracking on `Version`; keep drift detection loud.

## What gets simpler / removed

- `PlannedMigration` largely collapses to "the current set of tables."
- `MigrationPlanner.calculateMigrations` (replay-all-code-history loop) is no longer
  the apply-time path — its diffing logic relocates to Generate.
- The diff/apply engine (`StateDiffer`, `DiffApplier`, `diffStates`) is kept intact.

## Post-MVP backlog (foundations above are chosen so these bolt on cleanly)

- **Backwards migrations** + safety classification (computed by diffing snapshots).
- **Forward/backward distance** — "how far back from v2.x is safe?".
- **Out-of-order / cherry-pick** detection & safe reordering (needs richer identity
  than ordered semver alone).
  - *Idea to explore:* **patch versions** may be the mechanism that enables running
    migrations out of order — i.e. patch-level migrations are the ones eligible for
    safe reordering / interleaving, while major/minor establish hard ordering.
- **Granular JSON-type diffing** via oxygen-schema for typeless JSON columns.
- **Soft delete → hard delete** command (logical delete, later consolidation migration).
- **CLI / JAR** packaging (`oxygen-db migrate`) wrapping the library core.
- **On-the-fly / code-defined dev migration** (deferred; CP6 is FS-only). A path that
  migrates a DB directly from the current-code table definitions *without* committed
  filesystem files — handy for tests and quick local setups. Note this is **not** the old
  `PlannedMigration` path (that's removed); it would reuse the generator in-memory. Until
  then, every consumer (incl. tests and the example app) stages `PersistedMigrationFile`s.
- **Amend-last-unreleased-migration** mode (reduce dev-time file churn).
- **`RC` / pre-release suffixes for local dev** (`oxygen.core.Version` already models
  `ALPHA`/`BETA`/`M`/`RC`/`SNAPSHOT`). In the ideal end state with more granular
  control over generated migrations, RC versions could represent in-progress / not-yet-
  finalized migrations during local dev — generated and iterated under an RC suffix,
  then "promoted" to a final version on release. Worth exploring as the clean answer to
  dev-time churn + the collapse workflow.

## Open / TBD during implementation

- Exact env-var names.
- Migration directory location & configuration (resource path vs. project dir).
- Whether `current.json` returns as a generated convenience.
