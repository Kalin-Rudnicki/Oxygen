# OXY-91 — Figure out if any form of full-auto transform makes sense : Create Document

## Original
- **Key:** OXY-91
- **Checklist line:** `- [ ] [OXY-91](https://kr-oxygen.atlassian.net/browse/OXY-91) — **Subtask** · Normal — Figure out if any form of full-auto transform makes sense : Create Document`
- **Type:** Subtask
- **Priority:** Normal
- **Title (verbatim):** Figure out if any form of full-auto transform makes sense : Create Document
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-91
- **Checklist section:** To Do
- **Parent:** [OXY-90](https://kr-oxygen.atlassian.net/browse/OXY-90) — **Architecture** · Low — Figure out if any form of full-auto transform makes sense
- **Epic (inferred):** [OXY-87](https://kr-oxygen.atlassian.net/browse/OXY-87) — **Epic** · Normal — oxygen-transform (owning module `oxygen-transform` per `build.sbt:491` / `modules/general/transform`)
- **Siblings:** OXY-88 Task · Lower — Add support for partial transformer; OXY-89 Task · Lower — Add support for configurable transformer; OXY-92 Subtask · Normal — Figure out if any form ... : Create Issues

## Expanded Description

**What this issue likely is:** A research/documentation subtask of the OXY-90 architecture spike. OXY-90 asks whether `oxygen-transform` should offer any "full-auto" (zero-boilerplate, fully automatic / fully generic) derivation beyond today's explicit `Transform.derived`. OXY-91 is the **Create Document** half — produce a short design doc that evaluates options, trade-offs, and a recommendation. OXY-92 (Create Issues) will then fan out implementation tickets if the doc says "yes, proceed."

**Current state (verified from code):**

* `oxygen-transform` (`modules/general/transform/src/main/scala/oxygen/transform/`) today is **explicit-opt-in**: each `From -> To` pair needs a `given Transform[From, To] = Transform.derived` (or `TransformOrFail.derived`). Examples in `example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala:14` and `domainToApi.scala:47` define ~6 such givens by hand (`Transform.derived` per case-class pair).
* Derivation is macro-based (`TransformMacros.scala:10`, `DeriveProductTransform.scala`, `DeriveSumTransform.scala`): it requires `From` and `To` to be the same structural kind (both `Case` or both `Sealed`), field/case names to match exactly, and an implicit `Transform[F,T]` for every matched field (with built-in `Option`, `Seq`, `Pure` liftings in `TransformLowPriority`). `TransformOrFail` adds per-field `atField`/`atSubType` error scoping and `Option -> A` require, `String -> A` via `StringDecoder`, etc.
* By contrast, many other codecs in the repo already have an **`autoTransform` helper** that synthesizes a bidirectional function pair on the fly via `ProductGeneric.deriveTransform[A,B]` without materializing a `Transform` typeclass: `JsonCodec.autoTransform` (`modules/general/json/src/main/scala/oxygen/json/JsonCodec.scala:25`), `RequestCodec.PathLike.autoTransform` (`modules/http/zio/src/main/scala/oxygen/http/core/RequestCodec.scala:283`), `PageCodec.autoTransform` (`modules/ui/web/src/main/scala/oxygen/ui/web/PageCodec.scala:39`), `SchemaLike`, `RowRepr`. These still require an explicit `.autoTransform[B]` call site but no separate `given`.
* The sibling tasks OXY-88 (partial transformer) and OXY-89 (configurable transformer) suggest the design space: partial = map a superset to a subset (or with defaults/optionals), configurable = field renames / explicit per-field overrides, full-auto = no per-pair `given` at all (automatic summoning).

**What "full-auto" could mean — candidate interpretations (most to least likely):**

1. **Automatic given synthesis (full-auto summoning):** A generic `given [From, To] => Transform[From, To]` that auto-derives whenever a structural match exists, so `from.transformInto[To]` works with zero `given` declarations. This is the classic Magnolia/kittens-style auto-derivation.
2. **Transparent `autoTransform` extension:** An extension like `from.autoTransformInto[To]` / `Transform.auto[From, To]` that derives inline at the call site (like the codec `autoTransform`), bypassing the typeclass instance cache — still explicit at use site but no `given`.
3. **Lenient structural mapping:** Full-auto that also relaxes exact-name matching — e.g., ignores extra fields, fills missing `Option` fields with `None`, applies defaults, or does case conversion — overlapping with OXY-88/OXY-89 but bundled as "just works."

Interpretation (1) is taken as the primary reading for this triage because it is the natural opposite of today's explicit `Transform.derived` and matches the "any form of full-auto makes sense" framing.

**Who it affects:** Any service mapping between layers (api <-> domain, domain <-> db row) via `oxygen-transform`. Today the boilerplate is small but repetitive; full-auto would remove it at the cost of implicit search / compile-time behavior.

**Why it matters (Architecture, Low priority originally):** Decision has cross-cutting impact on compile times, error messages, rename safety, and onboarding ergonomics. Getting it wrong (too much magic) is hard to unwind.

**Inferred acceptance criteria for OXY-91's document:**

* Defines what "full-auto" means vs. partial (OXY-88) vs. configurable (OXY-89) and vs. existing codec `autoTransform`.
* Evaluates at least 3 options (keep explicit only; add opt-in generic given / auto summoning; add call-site `autoTransformInto`) with pros/cons on: ergonomics, compile time / implicit search explosion, coherence/orphan instances, error-message quality (field-mismatch diagnostics in `DeriveProductTransform.scala:43`), binary compatibility, interaction with `Option`/`Seq`/`Pure` priority givens, and rename-safety.
* References verified code paths (files listed above) and the real conversion sites in `example/`.
* Makes a recommendation (proceed / don't proceed / proceed with guardrails) and lists follow-up issues for OXY-92 to file.
* Stored where architecture docs live (e.g., `docs/docs/metaprogramming/` or `agent-docs/` or `modules/general/transform/README.md`) or as a linked doc in Jira, with enough detail that OXY-92 can be executed without re-doing the research.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner.

**Justification:**

* **Strong code signal for what "transform" means.** `modules/general/transform/` is small and self-contained (3 typeclass files + 4 generic derivations + 2 specs). The explicit `Transform.derived` pattern and its field-name-exact matching are unambiguous from `DeriveProductTransform.scala:43-65` and `TransformMacros.scala:14-25`.
* **Siblings disambiguate the design space.** OXY-88 (partial), OXY-89 (configurable), OXY-90/91/92 (full-auto) form a coherent progression from lenient to magical. The codec `autoTransform` pattern (`JsonCodec`, `RequestCodec`, `PageCodec`) provides a concrete prior for what "auto" looks like elsewhere in the repo, strengthening the interpretation.
* **Example usage grounds the boilerplate cost.** `apiToDomain.scala` / `domainToApi.scala` show the per-pair `given` burden that full-auto would eliminate, so the motivation is observable.
* **Remaining ambiguity caps the rating.** No Jira body or linked doc was fetched; whether full-auto means (1) auto-summoning vs (2) call-site helper vs (3) lenient mapping is inferred from naming and sibling context, not stated. The exact doc location / template and the decision criteria (performance data vs. opinion) are also unspecified.

## Required Changes (only if Confidence >= 3)

> Scope for **OXY-91 itself**: this subtask is research + document creation. No production code change is expected for OXY-91; implementation (if any) is deferred to issues created by OXY-92. The list below separates what OXY-91 should produce from what a hypothetical full-auto feature would touch (for context).

**OXY-91 — create the document (this issue):**

* [ ] Draft a design doc, e.g. `docs/docs/metaprogramming/transform-full-auto.md` or `agent-docs/transform-full-auto-decision.md` (or `modules/general/transform/docs/full-auto.md` — pick one and cross-link from the Epic). Suggested outline:
  1. Context — current `Transform.derived` flow, `TransformOrFail` error scoping, and the per-pair `given` sites in `example/apps/web-server/src/main/scala/oxygen/example/conversion/`.
  2. Definitions — full-auto vs. partial (OXY-88) vs. configurable (OXY-89) vs. codec `autoTransform` — with a table.
  3. Options evaluated — (A) Keep explicit only, (B) Generic auto-summoning `given`, (C) Call-site `autoTransformInto` / `Transform.auto` helper, (D) Hybrid (explicit by default, opt-in auto via import).
  4. Evaluation criteria — ergonomics, compile-time cost (macro expansion + implicit search), error-message quality (currently precise field-mismatch errors in `DeriveProductTransform.scala:43`), coherence/orphan & downstream given priority, rename-safety, effect on `Option`/`Seq`/`Pure` liftings, interaction with `TransformOrFail` branching.
  5. Prototype / measurement notes — optional but recommended: micro-benchmark compile time on a sample with N auto-derived pairs; before/after error message screenshots for a mismatched field.
  6. Recommendation — proceed or not, with guardrails (e.g., auto only for `Case <-> Case` with exact field names, behind `import oxygen.transform.auto.*`).
  7. Follow-ups for OXY-92 — concrete issue titles / scopes if proceeding.
* [ ] Review sibling intents with owners: confirm OXY-88/OXY-89 scope so the doc does not re-litigate partial/configurable design.
* [ ] Get sign-off from Epic owner (OXY-87) and link the doc from Jira OXY-91 / OXY-90.
* [ ] Update `jira-issues/results/OXY-91.md` (this file) — already done by this triage — and ensure OXY-92 can cite the doc.

**If the doc recommends proceeding — what full-auto implementation would touch (context for OXY-92, not OXY-91's own work):**

* [ ] `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala` — add an opt-in auto derivation mechanism (e.g., `object auto { given derivedAuto... }` or a `Transform.auto[From, To]` inline helper). Must avoid putting a global high-priority generic `given` in `Transform` companion that would widen implicit search for every compilation unit.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/TransformOrFail.scala` — same treatment for fallible path.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/generic/` — consider reusing `TransformMacros` vs. new macro that delegates to existing `DeriveProductTransform`/`DeriveSumTransform`; ensure `atField`/`atSubType` error paths still compose.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/extensions.scala` — potential `autoTransformInto` extension if option (C) is chosen.
* [ ] Tests: `modules/general/transform/src/test/scala/oxygen/transform/TransformSpec.scala` / `TransformOrFailSpec.scala` — add suites for auto path, mismatched field diagnostics, and priority interactions (`Option`/`Seq`).
* [ ] Docs: `docs/docs/metaprogramming/index.md` or module README — document when to use explicit vs. auto, and the import required to enable auto.
* [ ] Example: `example/apps/web-server/src/main/scala/oxygen/example/conversion/` — demonstrate the before/after (remove explicit givens if auto is adopted).

**Verified vs. inferred:**

* Verified: file lists, explicit `Transform.derived` usage, codec `autoTransform` pattern, macro structure — all read from repo at triage time.
* Inferred: doc location, exact recommendation, and whether measurements are required — assumed from repo conventions; to be confirmed by Epic owner.

## Estimates & Autonomy (only if Confidence >= 3)

* **Story points:** 2 (Fibonacci) — doc-only subtask.
  * *Justification:* Research touches ~8-10 files but produces a single markdown doc (3-5 pages) with no code change. Comparable to other doc spikes in the repo (`docs/docs/sql/`, `agent-docs/`). If the doc includes a compile-time prototype/measurement, push to 3.
* **Autonomy:** 5 / 6 — largely autonomous with this briefing + repo.
  * *Justification:* Agent can read `modules/general/transform/` and the conversion examples, survey `autoTransform` priors in `json`/`http`/`ui`, draft the doc, and propose a recommendation without human pairing. Needs a single review pass to confirm the recommendation aligns with the team's stance on implicit magic.
* **Ambiguity-to-resolve:** 3 / 6 — moderate; low enough to start.
  * *Justification:* Title is broad ("any form of full-auto makes sense") but well-bounded by the module and siblings. Main open questions are product-level (how much magic is acceptable, doc location, whether compile-time measurements are required) — resolvable in review, not blocking a first draft.

*Epic filter note:* OXY-91 traces to Epic **OXY-87 oxygen-transform** (verified owning module `oxygen-transform` in `build.sbt:491`). No epic-filter exclusion applies — OXY-87 is In Progress, and this subtask is its research gate before any transform feature work (OXY-88/89).

## Open Questions

* **Scope of "full-auto":** Does the team mean (A) auto-summoned `given Transform[A,B]` with zero declarations, (B) call-site `autoTransformInto` without a cached `given`, or (C) lenient mapping that also relaxes field-name matching / handles extra/missing fields? The triage assumes (A) primary, (B) secondary.
* **Overlap with OXY-88 / OXY-89:** Should the doc also recommend scope for partial and configurable transformers, or is it strictly full-auto? The three issues read as a progression; clarifying ownership avoids duplicate design.
* **Doc location & format:** Should the output live in `docs/docs/metaprogramming/`, `agent-docs/`, or `modules/general/transform/`? Is there a template (ADR / RFC) to follow?
* **Acceptance bar for the recommendation:** Is a qualitative trade-off table sufficient, or does the team expect a measured prototype (compile-time before/after, error-message samples)?
* **Guardrails if proceeding:** If full-auto is adopted, should it be opt-in (`import oxygen.transform.auto.given`) to avoid global implicit search cost and preserve rename-safety, or global? What is the policy for sum types?
* **Relation to codec `autoTransform`:** Should `oxygen-transform` align its full-auto API with the existing `JsonCodec.autoTransform` / `RequestCodec.autoTransform` naming and `ProductGeneric.deriveTransform` underpinnings, or keep them distinct?
* **OXY-92 handoff:** Does OXY-92 expect the doc to already contain fully scoped implementation issues (titles + estimates), or just a recommendation with rough scopes?
