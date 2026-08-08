# OXY-88 — Add support for partial transformer

## Original
- **Key:** OXY-88
- **Checklist line:** `- [ ] [OXY-88](https://kr-oxygen.atlassian.net/browse/OXY-88) — **Task** · Lower — Add support for partial transformer`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add support for partial transformer
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-88
- **Checklist section:** To Do
- **Epic:** [OXY-87](https://kr-oxygen.atlassian.net/browse/OXY-87) — **Epic** · Normal — oxygen-transform (In Progress)
- **Epic siblings (oxygen-transform):**
  - OXY-88 — Task · Lower — Add support for partial transformer (this issue)
  - OXY-89 — Task · Lower — Add support for configurable transformer
  - OXY-90 — Architecture · Low — Figure out if any form of full-auto transform makes sense (+ OXY-91, OXY-92 subtasks)

## Expanded Description

**What this issue likely means:** Extend `oxygen-transform` (`modules/general/transform`) with a *partial* transformation capability — a derived transformer that is allowed to be incomplete/fallible at the field level, rather than requiring a total 1:1 field mapping.

Today the module provides two derived typeclasses:

- `Transform[From, To]` (infallible) — `TransformMacros.deriveTransform` via `DeriveProductTransform` / `DeriveSumTransform`. Every field/case in `To` must have a name-matched field/case in `From` and a `Transform[F,T]` instance for the field types. Extra `From` fields are ignored; missing `To` fields are a *compile error* (`report.errorAndAbort` in `DeriveProductTransform.scala:43`). Fallible field conversions (e.g. `String => Int`, `Option[A] => B`) are impossible.
- `TransformOrFail[From, To]` (fallible) — `TransformMacros.deriveTransformOrFail` via `DeriveProductTransformOrFail` / `DeriveSumTransformOrFail`. Same name-matching requirement, but field transforms are `TransformOrFail[F,T]` (supporting `Option[A] => B` via `TransformRequireOption`, `String => A` via `StringDecoder`, `From => Either[String,To]` via `FromEitherFunction`, plus `Seq`/`Option`/`Pure` liftings). Failures are accumulated as `TransformError` with `ScopePath` (field/index/subtype) context.

`TransformOrFail` already demonstrates the *pattern* for partial data: `TransformOrFailSpec` derives `PartialPerson(first: String, last: Option[String], age: Option[Int]) => FullPerson(first: String, last: String, age: Int)` and fails with `MissingRequired` when an `Option` is `None`, with patch-like product and sum tests. So "partial transformer" is not just `TransformOrFail` under a different name — it must be a *new* capability beyond what `TransformOrFail` currently does.

The most plausible intended meaning (given the Epic's sibling breakdown) is one of these closely related interpretations — all share the idea that the transformer is *partial* w.r.t. the target shape:

**Primary interpretation (patch / subset product):** Allow deriving a transformer where `From` is a *partial/patch* representation of `To` — e.g. every field in `To` maps to `Option[F]` in `From`, or `From` simply has fewer fields than `To` — and the transform either (a) fails with `MissingRequired` if a required target field is absent (like `TransformOrFail` already does for `Option => Required`), or (b) merges a patch into an existing `To` instance (`patch.transformInto(existing: To): To` or `Transform[Patch, To => To]`). This is the Chimney-style "partial transformer" / PATCH semantics hinted at by `Specified[A]` in OXY-70. It would enable `Partial[Person]` (all fields optional) to update a `Person` without failing when `None` means "leave original".

**Alternative interpretation (compile-time partial / builder):** A staged/partial transformer that can be *completed* with per-field overrides — i.e. `Transform.derived[From,To].withFieldComputed(_.field, f).withDefault(...)` — where the "partial" is the intermediate builder that hasn't yet satisfied all missing `To` fields. In Chimney this is literally called a partial transformer builder. Under this reading OXY-88 is the prerequisite for OXY-89 (configurable transformer): first allow `DeriveProductTransform` to succeed even when some `To` fields lack a source match, collecting them as "unmapped" slots to be filled by configuration, rather than aborting at compile time.

**Least likely but possible:** Simply introduce a `PartialTransform[From, To]` alias/type that is `From => Either[TransformError, To]` (i.e. rename `TransformOrFail` to match Chimney terminology where "PartialTransformer" = fallible). This would be trivial but does not justify a separate Lower-priority Task distinct from OXY-89.

The distinction between OXY-88 and OXY-89 supports the primary+alternative synthesis: OXY-88 establishes *that* a transform can be derived from a subset/optional source (and how missing fields are handled — fail vs. keep-original vs. default), while OXY-89 adds the *configuration DSL* for renaming/computing/consting those fields. OXY-90 then asks whether any of this can be fully automatic (no manual `given` wiring).

**Who it affects:** Any service layer using `oxygen-transform` to map between API/domain/DB models (e.g. `example/apps/*/conversion/{apiToDomain,domainToApi,dbToDomain,domainToDb}` — currently hand-written `Transform.derived` chains via `transformers.scala`). Partial transformers would be used for PATCH endpoints, optional-field ingestion, and incremental model evolution where the source is missing fields that the target requires.

**Why it matters:** Without partial support, every new target field forces a source change or a manual hand-written `Transform`/`TransformOrFail` instance. PATCH/partial-update use-cases have no first-class story — today you must either make everything `Option` in both models or write bespoke merge logic. Symmetric with OXY-89/OXY-90: the Epic aims to make `oxygen-transform` competitive with Chimney/MapStruct for real-world model mapping.

**Inferred acceptance criteria:**
- A new or enhanced derivation entry point for partial products/sums (e.g. `Transform.derivedPartial`, `PartialTransform[From,To]`, or `TransformOrFail` with relaxed field-count checking) compiles when `To` fields are missing in `From` or when `From` fields are `Option[F]` and `To` fields are `T`.
- Missing required fields surface as `TransformError.Cause.MissingRequired` at the correct `ScopePath.Field` (and `SubType` for sums), consistent with existing `TransformError`/`ScopePath` reporting. If the "patch-merge" interpretation, an overload `def patch(into: To): Transform[Patch, To]` / `def transformPatch(patch: From, original: To): To` keeps original values when `None`.
- Existing `Transform`/`TransformOrFail` derivations remain source-compatible (no behavior change for total mappings).
- Sum-type handling mirrors product handling: extra/missing cases and `Option` sub-fields behave consistently with `DeriveSumTransformOrFail`.
- Tests analogous to `TransformOrFailSpec` cover the partial product/sum paths (present, missing, `None` vs. `Some`, nested, collection) and error paths with correct `ScopePath`.
- Docs sketch the new API (likely in `agent-docs` or `docs/docs` for transform) and note interaction with OXY-89 (configurable overrides) and OXY-90 (full-auto decision).

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Strong module signal: `jira-issues/checklist.md:47` places OXY-88 under Epic OXY-87 `oxygen-transform` (In Progress, Normal) alongside OXY-89 (configurable) and OXY-90 (full-auto). The three titles form a coherent Chimney-inspired progression: basic (already done), partial (this), configurable (next), full-auto (architecture decision). Module `modules/general/transform` exists with exactly the code that would need to change.
  - Strong code signal for the *gap*: `DeriveProductTransform.scala:40-50` and `DeriveProductTransformOrFail.scala:40-54` both `report.errorAndAbort` when a `To` field lacks a `From` match — confirming that subset/partial derivation is currently a compile error. `ProductGeneric.Field.constructorDefault` (supports default values) exists but is never consulted during transform derivation — hinting at a planned path for filling missing fields.
  - Strong code signal for the *shape* of the feature: `TransformOrFailSpec.scala:20-54` already has `PartialPerson`/`FullPerson` and `PartialSum`/`FullSum` with `Option`→`Required` via `TransformRequireOption`, proving the fallible partial pattern is intended but not yet exposed as a first-class "partial transformer" (currently users must hand-write `Option` in the partial model and use `TransformOrFail.derived`).
  - Weak signal for the *exact contract*: no Jira body was fetchable, no `TODO`/`FIXME` mentions "partial", and the title "Add support for partial transformer" is 5 words with no spec. Whether "partial" means (a) patch-merge `(Patch, Original) => Updated`, (b) builder/placeholder for configurable overrides, or (c) just an alias for `TransformOrFail` is inferred from sibling titles and Chimney conventions, not verified. This keeps rating at 3, not 4–6.

## Required Changes (only if Confidence >= 3 — confidence is 3, so included)

- **Module ownership:** `oxygen-transform` (`modules/general/transform`) is primary; `modules/general/core` (`oxygen-meta/k0 ProductGeneric`) only if default-value or field-filtering helpers are needed; no `oxygen-sql`/`oxygen-http` changes except example conversions.
- **API design — decide the surface (needs product decision, all options grounded in current code):**
  - [ ] **Option A — Relax `TransformOrFail` derivation (minimal):** Modify `DeriveProductTransformOrFail.from` to not abort when `fromFieldMap.get(toField.name)` is `None`, but instead require an implicit `Default[T]` / `Option[T]` / `constructorDefault` for that field, or treat `From` field `Option[F]` → `T` via `TransformRequireOption` as the partial case. This makes "partial" just a mode of `TransformOrFail.derived` without a new type. — *verified: current abort at [`DeriveProductTransformOrFail.scala:43`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/DeriveProductTransformOrFail.scala:43) is the gate.*
  - [ ] **Option B — New `PartialTransform` / `Transform.partial` alias (Chimney-parity):** Introduce `trait PartialTransform[From, To] { def transformPartial(from: From): Either[TransformError, To] }` or alias `type PartialTransform[A,B] = TransformOrFail[A,B]` and a derivation `inline def partial[From,To]: PartialTransform[From,To] = ${ ... }` that delegates to `DeriveProductTransformOrFail` but with the relaxed field logic. Keeps naming aligned with Chimney where `PartialTransformer` = fallible. — *inferred; leverages existing `TransformError`/`ScopePath`.*
  - [ ] **Option C — Patch-merge transformer (PATCH semantics):** Introduce `trait PatchTransform[Patch, Target] { def patch(patch: Patch, original: Target): Target // or Either }` or `Transform[Patch, Target => Target]` where `Patch` is `ProductGeneric[A]` with `Option` fields, `None` means "retain original". Derive via a new `DerivePatchTransform` that pairs `fromGeneric.fields` with `toGeneric.fields` by name and generates `if patch.field.isDefined then transform else original.field`. — *inferred from OXY-70 `Specified` / PATCH context; no current patch helper exists.*
  - [ ] Decision needed on which of A/B/C (or combination: A+B then C as follow-up) satisfies OXY-88. Recommend A+B as OXY-88 scope (derive from subset/optional source), defer C to OXY-89 or a follow-up if PATCH merge is required.

- **Generic derivation — product:**
  - [ ] **File:** [`DeriveProductTransform.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/DeriveProductTransform.scala) and [`DeriveProductTransformOrFail.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/DeriveProductTransformOrFail.scala) — extract shared field-matching logic (currently duplicated) and add a `allowMissing: Boolean` or `partial: Boolean` mode.
  - [ ] For each `toField` missing in `fromFieldMap`: if `toField.constructorDefault.nonEmpty` (has Scala default) or implicit `Default[T]` in scope, use that; else if `toField` type is `Option[U]`, default to `None`; else require `TransformOrFail[Option[F], T]` path that fails with `MissingRequired` (already exists as `TransformRequireOption`) — the "partial" is that the derivation *succeeds at compile time* and defers the missing-field check to runtime `Left`. — *verified: [`ProductGeneric.Field.constructorDefault`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/core/src/main/scala/oxygen/meta/k0/ProductGeneric.scala:115) exists but is unused in transform.*
  - [ ] Preserve `ScopePath.Field`/`Index`/`SubType` error wrapping: new missing-field branches must wrap via `.atField(toField.name)` like current `DeriveProductTransformOrFail.scala:71` does, and builder `buildFlatMapExpr` must sequence the new `Either`-producing steps.
  - [ ] Consider whether `From` fields that are `Option` wrapping the same underlying type but `To` fields are required should *always* use `TransformRequireOption` (current `TransformOrFail` does) vs. a new `PatchKeepOriginal` combinator if Option C is chosen.

- **Generic derivation — sum:**
  - [ ] **Files:** [`DeriveSumTransform.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/DeriveSumTransform.scala) and [`DeriveSumTransformOrFail.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/DeriveSumTransformOrFail.scala) — mirror product changes for case matching. Current `fromGeneric.cases.map(CaseTransform.from(_))` iterates `From` cases requiring a `To` case by name; partial sums may need to handle `From` having more cases than `To` (or vice versa) with appropriate failure or filtering. — *verified: abort at `DeriveSumTransform.scala:44` / `DeriveSumTransformOrFail.scala:44` when case missing.*
  - [ ] Ensure `atSubType` wrapping (`DeriveSumTransformOrFail.scala:61`) is preserved for error paths.

- **Core typeclasses & macros:**
  - [ ] **File:** [`TransformMacros.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/generic/TransformMacros.scala) — add `derivePartial` / `deriveTransformPartial` entry points or a flag parameter to `deriveTransformOrFail`. Verify `TypeType.Case` / `TypeType.Sealed` dispatch covers new modes (partial products/sums still dispatch on same `typeType`).
  - [ ] **File:** [`Transform.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/Transform.scala) / [`TransformOrFail.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/main/scala/oxygen/transform/TransformOrFail.scala) — add companion `inline def partial[From,To]` or new trait if Option B/C. Keep `given` priority chain (`TransformLowPriority.*` / `TransformOrFailLowPriority.*`) intact so new givens (e.g. `Default`, `Patch`) don't shadow existing `Option`/`Seq`/`Pure` liftings.
  - [ ] Verify interaction with existing liftings: `TransformOption`, `TransformSeq`, `TransformPure`, `TransformRequireOption`, `DecodeString`, `FromEitherFunction`, `AtField`/`AtSubType` must compose with new partial field transforms.

- **Tests:**
  - [ ] **File:** [`TransformSpec.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/test/scala/oxygen/transform/TransformSpec.scala) — add total-to-partial positive cases (e.g. `FullPerson => PartialPerson` via `RequireOption` inverse, subset source => superset target with defaults).
  - [ ] **File:** [`TransformOrFailSpec.scala`](/home/kalin/dev/repo/worktrees/oxygen-jira/modules/general/transform/src/test/scala/oxygen/transform/TransformOrFailSpec.scala) — extend `PartialPerson`/`FullPerson` / `PartialSum`/`FullSum` coverage: missing field -> `MissingRequired` at correct `ScopePath`, default-valued field uses default, `Option` target field gets `None` when source missing. Add nested-collection partial cases.
  - [ ] If patch-merge (Option C): add `PatchSpec` for `patch(original)` semantics (`None` retains original, `Some` transforms and replaces).

- **Examples & docs:**
  - [ ] Update `example/apps/*/conversion/*` or add `example/apps/example-app/src/main/scala/oxygen/example/transform/PartialExample.scala` demonstrating partial usage.
  - [ ] Document in Epic (`agent-docs` or `docs/docs`) the new API and its relationship to OXY-89 (configurable) and OXY-90 (full-auto) — especially the compile-time vs runtime missing-field handling and the role of `constructorDefault` / `Default`.

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 5 — Fibonacci scale for a Task (not Epic). Derivation macros (`DeriveProductTransform*`, `DeriveSumTransform*`, `TransformMacros`) plus new tests and sub-`Option`/`Default` handling is more than a 2–3 point extension but smaller than an 8-point cross-module feature. If patch-merge semantics (Option C) is included, re-estimate to 8. Epic OXY-87 would aggregate OXY-88 (5) + OXY-89 (5–8) + OXY-90 (3–5).
  - *Justification:* Macro-heavy work with `ProductGeneric`/`SumGeneric`/`ValDef`/`MatchBuilder` is delicate; requires mirroring product+sum paths and verifying `ScopePath` error reporting. Pure `oxygen-transform` scope (no DB/HTTP changes) bounds effort.

- **Autonomy 1–6:** 3 / 6 — moderate autonomy.
  - *Justification:* An agent with the repo + this briefing can implement the macro changes and tests autonomously, but the *product contract* for "partial" (fail vs. keep-original vs. default, and whether to introduce a new `PartialTransform` type vs. enhance `TransformOrFail`) requires a human decision before code. The ambiguous title forces at least one confirmation loop.

- **Ambiguity-to-resolve 1–6:** 5 / 6 — high ambiguity must be resolved before an agent starts.
  - *Justification:* Title is 5 words with no Jira body. The three sibling issues (OXY-88 partial, OXY-89 configurable, OXY-90 full-auto) could partition the same design space in multiple ways. Core open questions (see below) — patch-merge vs. compile-time partial builder, handling of defaults/Option, new type vs. flag — are architectural and block the first derivation edit. Low priority confirms this is not blocking, but also means no spec has been forced to resolve.

## Open Questions

1. **What does "partial" mean for this issue?** (a) Derive succeeds when `From` has fewer fields than `To` and missing fields fail at runtime (`MissingRequired`)? (b) Derive a `Patch`-style transformer `(Patch, Original) => Updated` where `None` retains original (PATCH semantics)? (c) Internal builder state for OXY-89's configurable transformer (`withFieldComputed` fills missing slots)? The three interpretations share code but differ in API; a human should pick one scope for OXY-88.
2. **New type or mode on existing?** Should OXY-88 introduce a new trait `PartialTransform[From,To]` / `Transform.partial` (Chimney `PartialTransformer` parity) or just relax `TransformOrFail.derived` to allow missing fields? A new type is a breaking API surface decision and affects OXY-89's follow-on design.
3. **How to fill missing target fields at derivation?** Options: (i) fail at runtime with `MissingRequired` (current `TransformRequireOption` pattern), (ii) use Scala default value (`constructorDefault`) if present, (iii) use `Option` default `None` when target is `Option`, (iv) require the new configurable DSL (`withFieldConst`/`withFieldComputed`) from OXY-89. Which is in scope for OXY-88 vs deferred to OXY-89?
4. **Source `Option` wrapping semantics:** Does `From.field: Option[A]` → `To.field: B` always mean "require present" (fail if `None`), or does it mean "if `None`, keep original/default" (patch semantics)? Current `TransformRequireOption` implements the former; patch would need a different combinator.
5. **Sum-type partiality:** Should `From` having more cases than `To` or `To` having more cases than `From` be allowed? What is the failure mode (`DecodingFailure` vs `MissingRequired` vs compile error) and `ScopePath.SubType` handling?
6. **Defaults via `ProductGeneric.Field.constructorDefault`:** Should the partial transformer consult constructor defaults, and if so, does that require `-Yretain-trees` or a `Default[T]` typeclass fallback? This affects whether a `Person(name: String = "anon")` can be derived from a source lacking `name`.
7. **Relationship to `Specified[A]` (OXY-70):** PATCH semantics in `oxygen-json` distinguish `WasNotSpecified` vs `WasSpecified(None)` vs `WasSpecified(Some(v))`. Should the partial transformer align with `Specified` rather than `Option` for patch models, or is `Option` sufficient for OXY-88?
8. **Error accumulation vs fail-fast:** `DeriveProductTransformOrFail.buildFlatMapExpr` currently fail-fasts on first `Left` via `flatMap` chain. Should a partial transformer accumulate *all* missing-field errors (like a validated `NonEmptyList[TransformError]`) or keep fail-fast? No current code does accumulation.
9. **Scope of OXY-88 vs OXY-89 split:** If OXY-88 implements the relaxed derivation that allows missing fields, and OXY-89 implements `withFieldComputed`/`withFieldRenamed`, what prevents OXY-88 from being subsumed by OXY-89? A clear split (OXY-88 = subset/Option source, OXY-89 = explicit overrides) needs confirmation.
10. **Full-auto implication (OXY-90):** If OXY-88/OXY-89 require manual `given` instances for field transforms, does OXY-90 intend to auto-derive those `given`s transitively? That changes whether `Transform.partial` should search implicits recursively or require explicit wiring.

