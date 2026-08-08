# OXY-92 — Figure out if any form of full-auto transform makes sense : Create Issues

## Original
- **Key:** OXY-92
- **Checklist line:** `- [ ] [OXY-92](https://kr-oxygen.atlassian.net/browse/OXY-92) — **Subtask** · Normal — Figure out if any form of full-auto transform makes sense : Create Issues`
- **Type:** Subtask
- **Priority:** Normal
- **Title (verbatim):** Figure out if any form of full-auto transform makes sense : Create Issues
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-92
- **Checklist section:** To Do
- **Parent:** [OXY-90](https://kr-oxygen.atlassian.net/browse/OXY-90) — **Architecture** · Low — Figure out if any form of full-auto transform makes sense
- **Epic (inferred):** [OXY-87](https://kr-oxygen.atlassian.net/browse/OXY-87) — **Epic** · Normal — oxygen-transform (owning module `oxygen-transform` per `build.sbt:491` / `modules/general/transform`)
- **Siblings:** OXY-88 Task · Lower — Add support for partial transformer; OXY-89 Task · Lower — Add support for configurable transformer; OXY-91 Subtask · Normal — Figure out if any form ... : Create Document

## Expanded Description

**What this issue likely is:** The second half of the OXY-90 architecture spike. OXY-90 asks whether `oxygen-transform` should offer any "full-auto" (zero-boilerplate, fully automatic) derivation beyond today's explicit `Transform.derived`. The spike is split into two subtasks: **OXY-91 — Create Document** (research + decision doc with options, trade-offs, recommendation) and **OXY-92 — Create Issues** (this issue — file the follow-up Jira tickets that implement whatever OXY-91 recommends).

This is a **process / planning subtask**, not a code feature. No production code is written in OXY-92 itself; its deliverable is a set of well-scoped, estimated Jira issues under Epic OXY-87 (or linked to OXY-88/OXY-89).

**Current state (verified from code):**

* `oxygen-transform` (`modules/general/transform/src/main/scala/oxygen/transform/`) today is **explicit-opt-in**: each `From -> To` pair needs a `given Transform[From, To] = Transform.derived` (or `TransformOrFail.derived`). Examples in `example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala:14` and `domainToApi.scala:47` define ~6 such givens by hand. Derivation is macro-based (`TransformMacros.scala:10`, `DeriveProductTransform.scala`, `DeriveSumTransform.scala`): it requires `From` and `To` to be the same structural kind (both `Case` or both `Sealed`), field/case names to match exactly, and an implicit `Transform[F,T]` for every matched field (with built-in `Option`, `Seq`, `Pure` liftings in `TransformLowPriority`).
* By contrast, many other codecs in the repo already have an **`autoTransform` helper** that synthesizes a bidirectional mapping on the fly via `ProductGeneric.deriveTransform[A,B]` without materializing a `Transform` typeclass: `JsonCodec.autoTransform` (`modules/general/json/src/main/scala/oxygen/json/JsonCodec.scala:25`), `RequestCodec` (`modules/http/zio/src/main/scala/oxygen/http/core/RequestCodec.scala:283`), `PageCodec` (`modules/ui/web/src/main/scala/oxygen/ui/web/PageCodec.scala:39`). These still require an explicit call site but no separate `given`.
* The sibling tasks OXY-88 (partial transformer) and OXY-89 (configurable transformer) frame the design space: partial = superset↔subset with defaults/optionals, configurable = field renames / per-field overrides, full-auto = no per-pair `given` at all (automatic summoning).

**What OXY-92 must produce — two branches depending on OXY-91's recommendation:**

1. **If OXY-91 recommends "proceed" (adopt some form of full-auto):** File 1–4 implementation issues that cover the chosen variant. Illustrative scopes (pick per recommendation):
   * Issue A — "Implement call-site auto-derivation helper (`Transform.auto` / `autoTransformInto`)" — add an `inline def` in `Transform` companion or `extensions.scala` that delegates to `TransformMacros` without requiring a `given` in scope, behind an opt-in import (`import oxygen.transform.auto.given`) to avoid global implicit search cost.
   * Issue B — "Add opt-in global auto import + compile-time benchmark" — if the doc recommends transparent `given autoDerived`, implement the low-priority generic `given` in an `auto` object and measure compile-time delta on `example/.../conversion/` (before/after).
   * Issue C — "Update example conversions to use full-auto + document migration" — remove explicit `given Transform = Transform.derived` sites in `example/apps/.../conversion/` and add docs (`docs/docs/` or module README) explaining when to use explicit vs. auto and the import required.
   * Issue D — "Full-auto for `TransformOrFail` + error-path tests" — mirror the above for the fallible path (`TransformOrFail`, `atField`/`atSubType` scoping), if the doc recommends it.
   Issues should be children of Epic OXY-87 or linked to OXY-88/OXY-89 as appropriate, with story-point estimates and acceptance criteria.

2. **If OXY-91 recommends "reject" (do not add full-auto):** File a single closing issue or mark OXY-92 as "no implementation needed, see decision doc" with justification. Alternatively, file one housekeeping issue: "Record decision to keep explicit `Transform.derived` — close spike and link doc from Epic OXY-87." No code issues are filed; the rationale from OXY-91 is recorded in Jira so the question is not re-litigated.

**Who it affects:** Same audience as OXY-90/OXY-91 — authors of the `api ↔ domain ↔ db` conversion layers and future `oxygen-transform` library users. Downstream work for OXY-88/OXY-89 may be sequenced after OXY-92 depending on whether full-auto subsumes them.

**Why it matters:** OXY-92 is the gate that turns a research spike into actionable backlog. Without it, OXY-91's doc has no execution path; with it, the team gets estimated, reviewable issues that can be scheduled under Epic OXY-87.

**Inferred acceptance criteria:**

* OXY-91's decision doc exists and is linked from Jira (prerequisite — OXY-92 cannot be completed before OXY-91).
* Jira issues are filed that faithfully reflect the doc's recommendation (adopt → 1–4 implementation issues with scope, files-to-touch, and estimates; reject → 1 closing/no-op issue with rationale). Each issue has a title, description, and epic link (typically Epic OXY-87).
* Issues are reviewed / accepted by Epic owner (OXY-87) and are actionable without re-doing the research (i.e., they cite the doc and the verified code paths: `TransformMacros`, `DeriveProductTransform`, `TransformOption`/`TransformSeq`/`TransformPure` chains, `extensions.scala`).
* `jira-issues/results/OXY-92.md` (this file) is updated and the spike (OXY-90 + OXY-91 + OXY-92) can be closed.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner.

**Justification:**

* **Strong code + sibling signal for what "transform" and "full-auto" mean.** `modules/general/transform/` is small and self-contained (explicit `Transform.derived` pattern, field-name-exact matching in `DeriveProductTransform.scala:43-65`, macro entry `TransformMacros.scala:14-25`). Siblings OXY-88 (partial), OXY-89 (configurable), OXY-90/91/92 (full-auto split into Document + Issues) form a coherent trilogy from lenient to magical, matching each triaged result file (OXY-88/89/90/91 all verified the same module).
* **Parent architecture and subtask suffix are unambiguous.** OXY-90 is typed `Architecture · Low` with subtasks literally suffixed `: Create Document` and `: Create Issues` — the standard spike decomposition (research doc + backlog filing). OXY-91's triage already documents this split and is internally consistent with this reading.
* **Example + codec `autoTransform` precedent grounds the motivation.** `apiToDomain.scala` / `domainToApi.scala` show the per-pair `given` burden that full-auto would eliminate; `JsonCodec.autoTransform` / `RequestCodec.autoTransform` show what "auto" looks like elsewhere, strengthening the interpretation that OXY-92's follow-up issues would implement an analogous helper for `Transform`.
* **Remaining ambiguity caps the rating.** No Jira body was fetched; whether full-auto means (a) auto-summoned `given`, (b) call-site `autoTransformInto` helper, or (c) lenient mapping is inferred, not stated. How many issues OXY-92 should file (1 vs. 3–4), their exact titles/estimates, and whether "Create Issues" means actually creating Jira tickets vs. just listing them in the doc are not specified. The precise Epic linkage (children of OXY-87 vs. standalone) is also inferred from the transform module ownership.

## Required Changes (only if Confidence >= 3)

> Confidence 4 — proceeding with deeper analysis per instructions.
> Scope for **OXY-92 itself**: this subtask is **issue-creation / backlog filing**, not production code. No source-code change is expected for OXY-92; implementation (if any) is deferred to the issues that OXY-92 files. The list below separates what OXY-92 should produce from what a hypothetical full-auto implementation would touch (for context on those follow-up issues).

**OXY-92 — create the issues (this subtask):**

* [ ] **Prerequisite — consume OXY-91's decision doc** — read the doc produced by OXY-91 (e.g., `docs/docs/metaprogramming/transform-full-auto.md` or `agent-docs/transform-full-auto-decision.md` per OXY-91's proposal). Confirm the recommendation: proceed / don't proceed / proceed with guardrails, and which full-auto variant was chosen (call-site helper vs. global `given` vs. hybrid opt-in via import). Verify the doc's comparison matrix and code-path references are sufficient to scope issues without re-researching.
* [ ] **Draft follow-up issue set — branch on recommendation:**
  * **If "proceed":** File 1–4 Jira issues (create in `https://kr-oxygen.atlassian.net/browse/OXY-87` epic or as Tasks linked to Epic). Suggested templates (adapt to the doc's chosen variant; do not file all if the doc narrows scope):
    1. `OXY-9xx — Implement full-auto call-site helper for Transform` — `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala` — add `inline def auto[From, To]: Transform[From, To]` (or `Transform.auto` / `autoTransformInto` extension in `extensions.scala`) delegating to `TransformMacros` / `DeriveProductTransform` / `DeriveSumTransform`. Must avoid a global high-priority generic `given` in `Transform` companion that would widen implicit search for every compilation unit; prefer `object auto { given derivedAuto... }` opt-in. Scope: `Case <-> Case` exact field-name match first; `Sealed` sum support second. Acceptance: `case class A(x: Int); case class B(x: Int); A(1).autoTransformInto[B] == B(1)` without a `given` in scope, plus mismatched-field diagnostic still reports the field name (preserving `DeriveProductTransform.scala:43` error quality).
    2. `OXY-9xx — Implement full-auto for TransformOrFail` — `modules/general/transform/src/main/scala/oxygen/transform/TransformOrFail.scala` + `generic/DeriveProductTransformOrFail.scala` — same treatment for the fallible path, preserving `atField`/`atSubType` error scoping and `Option -> A` require / `String -> A` via `StringDecoder` branching. Only if doc recommends fallible auto-derivation.
    3. `OXY-9xx — Add opt-in auto import + compile-time benchmark` — new `modules/general/transform/src/main/scala/oxygen/transform/auto.scala` (or `object auto` inside `Transform`) exposing the opt-in import, plus a measurement note: compile-time delta on `example/apps/web-server/src/main/scala/oxygen/example/conversion/` (before/after `sbt 'project oxygen-transformJVM' compile`) and error-message before/after for a mismatched field.
    4. `OXY-9xx — Update example conversions + docs to demonstrate full-auto` — `example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala`, `domainToApi.scala`, `modules/domain-impl/.../domainToDb.scala`, `dbToDomain.scala` — remove explicit `given Transform = Transform.derived` sites where auto applies, add `import oxygen.transform.auto.given` (or call-site helper usage), and document in `docs/docs/metaprogramming/` or `modules/general/transform/README.md` when to use explicit vs. auto.
  * **If "reject":** File a single housekeeping issue or resolution comment:
    * `OXY-9xx — Record decision: no full-auto transform — keep explicit Transform.derived` — link OXY-91's doc, summarize rationale (compile-time cost, implicit coherence, rename-safety, overlap with OXY-88/OXY-89), and mark Epic OXY-87's full-auto track as closed. No code change; satisfies OXY-92's acceptance by documenting why zero implementation issues were filed. Mark OXY-92 itself as done with "no implementation needed, see decision doc" and link the Jira resolution.
* [ ] **Epic linkage + estimates** — each filed issue must be a child of Epic OXY-87 (`oxygen-transform`) or linked to OXY-88/OXY-89 as appropriate, with a story-point estimate (Fibonacci) and priority (typically `Normal` or `Lower` per siblings). Include "Verified vs. inferred" note in each issue description citing the doc and the code paths above.
* [ ] **Review + close spike** — get sign-off from Epic owner (OXY-87) on the filed issue set, link the issues from Jira OXY-92 / OXY-90, and transition OXY-90/OXY-91/OXY-92 to Done if the recommendation is approved. Ensure `jira-issues/results/OXY-92.md` (this file) is the audit trail and that OXY-92's issues can be executed without re-doing the research.

**If the doc recommends proceeding — what full-auto implementation would touch (context for the issues OXY-92 files, not OXY-92's own work):**

* [ ] `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala` — add the opt-in auto derivation mechanism (e.g., `object auto { given derivedAuto... }` or `inline def auto[From, To]` helper). Must avoid a global high-priority generic `given` in the companion.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/TransformOrFail.scala` — same treatment for fallible path, if in scope.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/generic/` — reuse `TransformMacros` vs. new macro that delegates to existing `DeriveProductTransform`/`DeriveSumTransform`; ensure `atField`/`atSubType` error paths still compose for the fallible variant. Verify macro `Quotes` implicit scoping / `ValDef` caching pattern (`DeriveProductTransform.scala:23-24`) is replicated correctly.
* [ ] `modules/general/transform/src/main/scala/oxygen/transform/extensions.scala` — potential `autoTransformInto` / `transformIntoAuto` extension if the doc chooses the call-site helper variant.
* [ ] Tests: `modules/general/transform/src/test/scala/oxygen/transform/TransformSpec.scala` / `TransformOrFailSpec.scala` — add suites for the auto path (happy path + mismatched field diagnostics + `Option`/`Seq`/`Pure` priority interactions + sum types).
* [ ] Docs: `docs/docs/metaprogramming/index.md` or module README — document when to use explicit vs. auto, the import required to enable auto, and migration for existing `given Transform = Transform.derived` sites.
* [ ] Example: `example/apps/web-server/src/main/scala/oxygen/example/conversion/` and `modules/domain-impl/.../conversion/` — demonstrate before/after (remove explicit givens if auto is adopted).

**Verified vs. inferred:**

* Verified: checklist line + type/priority, parent OXY-90 Architecture spike, Epic OXY-87 owning module `oxygen-transform` (`build.sbt:491` `CrossProject` at `modules/general/transform`), owning files (`Transform.scala:56`, `TransformMacros.scala:10`, `DeriveProductTransform.scala:43`, `extensions.scala:4`), codec `autoTransform` prior (`JsonCodec.scala:25`, `RequestCodec.scala:283`), conversion sites in `example/.../conversion/`, sibling trilogy OXY-88/89/90 — all read from repo at triage time.
* Inferred: how many issues OXY-92 should file, their exact titles/scopes, story-point splits, whether Jira tickets must be physically created in Atlassian vs. listed in the doc, doc location, and the precise full-auto variant chosen — assumed from OXY-91's inferred recommendation structure and repo conventions; to be confirmed by Epic owner after OXY-91 lands.

## Estimates & Autonomy (only if Confidence >= 3)

* **Story points:** 1 (Fibonacci) — paperwork / backlog-filing subtask.
  * *Justification:* OXY-92 itself writes no production code — it reads OXY-91's doc and files 1–4 well-scoped Jira issues with titles, descriptions, epic links, and estimates. The follow-up implementation issues that OXY-92 files would be sized separately: typically 2–3 points for a call-site helper, 3–5 for a global opt-in `given` with benchmark, 1–2 for example/docs updates, 2–3 for `TransformOrFail` mirroring. Aggregated follow-up epic would be ~8–13 if full-auto is fully adopted across both `Transform` and `TransformOrFail`. Comparable to OXY-91 (2 points, doc-only) but smaller because OXY-92 is downstream of that doc. If "Create Issues" is interpreted as also implementing the first issue, re-estimate to 3.)
* **Autonomy:** 5 / 6 — largely autonomous with this briefing + repo + OXY-91's doc.
  * *Justification:* Agent can read OXY-91's decision doc, map its recommendation to the templates above, and draft Jira issue bodies that cite the verified code paths without human pairing. Needs a single review pass to confirm the issue count/scope aligns with the team's stance on implicit magic (same product/taste question as OXY-91).
* **Ambiguity-to-resolve:** 2 / 6 — low; ready to start once OXY-91 is done.
  * *Justification:* OXY-92 is mechanically downstream of OXY-91 — ambiguity is bounded to "what did OXY-91 recommend?" which will be answered by the doc. No additional product/design questions beyond those already tracked in OXY-91's Open Questions. The only pre-start clarification is whether "Create Issues" requires actually creating tickets in Atlassian or just listing them in the doc / markdown (either satisfies the triage's "file" wording — assume listing with Jira-ready titles + descriptions is sufficient unless the team requires live Jira creation).

*Epic filter note:* OXY-92 traces to Epic **OXY-87 oxygen-transform** (verified owning module `oxygen-transform` in `build.sbt:491`, `CrossType.Pure` at `modules/general/transform`). No epic-filter exclusion applies — OXY-87 is **In Progress** (one of 7 In Progress epics per `checklist.md`), and this subtask is its backlog-filing gate after the OXY-91 research doc. Not deferred.

## Open Questions

* **Scope of "Create Issues" — Jira vs. markdown:** Does OXY-92 require actually creating tickets in `https://kr-oxygen.atlassian.net` (needs credentials / API), or is a Jira-ready list in the decision doc / `jira-issues/results/OXY-92.md` sufficient? The triage assumes the latter satisfies the deliverable with links, but live creation would require access confirmation.
* **Branching on OXY-91 outcome:** If OXY-91 recommends "reject" (no full-auto), should OXY-92 file a single "no-op / decision recorded" issue and close, or simply resolve OXY-92 as Done with a comment linking the doc? Both satisfy the spike's acceptance criterion ("if 'reject,' rationale is recorded"), but Jira workflow may expect one.
* **Issue count and granularity:** Should OXY-92 file one umbrella issue ("Implement full-auto transform") or 2–4 sliced issues (helper + failible + benchmark + example/docs)? Slicing is assumed per the repo's preference for small, reviewable PRs, but the team's Jira hygiene may prefer one.
* **Overlap with OXY-88 / OXY-89:** Should the issues OXY-92 files also scope partial (OXY-88) and configurable (OXY-89) interaction — e.g., "full-auto only for exact field-name match; delegate lenient/renamed cases to partial/configurable" — or keep OXY-92's issues strictly to full-auto with cross-links? Clarifying avoids duplicate design.
* **Guardrails if proceeding:** Same as OXY-91 — if full-auto is adopted, should it be opt-in (`import oxygen.transform.auto.given`) to avoid global implicit search cost and preserve rename-safety, or global? The issue templates assume opt-in, but the decision doc may choose otherwise.
* **Epic linkage:** Should follow-up issues be children of Epic OXY-87 (`oxygen-transform`) specifically, or also linked to OXY-88/OXY-89 as related? Assumed children of OXY-87 with `relates to` links to siblings.
* **OXY-92 handoff detail:** Does OXY-92 expect fully estimated issues (titles + descriptions + story points + acceptance criteria + files-to-touch) or just rough scopes? The triage produces the former (actionable issues) since the cost is low and it unblocks scheduling.
