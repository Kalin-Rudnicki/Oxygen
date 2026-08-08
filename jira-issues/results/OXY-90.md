# OXY-90 — Figure out if any form of full-auto transform makes sense

## Original
- **Key:** OXY-90
- **Checklist line:** `- [ ] [OXY-90](https://kr-oxygen.atlassian.net/browse/OXY-90) — **Architecture** · Low — Figure out if any form of full-auto transform makes sense`
- **Type:** Architecture
- **Priority:** Low
- **Title (verbatim):** Figure out if any form of full-auto transform makes sense
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-90
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** An architecture spike / decision record for the `oxygen-transform` module (Epic **OXY-87 — oxygen-transform**, In Progress). The module today provides explicit, opt-in derivation via `Transform.derived` / `TransformOrFail.derived` (see `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala:56`, `TransformOrFail.scala:97`, `generic/TransformMacros.scala:10-46`). Call sites must declare a `given`:

```scala
given Transform[Api.user.RegisterRequest, Domain.user.Register] = Transform.derived
extension (self: Api.user.RegisterRequest) def toDomain = self.transformInto
```

(Verified in `example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala:14`, `domainToApi.scala:47`, `modules/domain-impl/.../domainToDb.scala:54`, `dbToDomain.scala:53`.)

Siblings define the planned evolution: **OXY-88 — Add support for partial transformer** (allow missing/extra fields, Option/default handling) and **OXY-89 — Add support for configurable transformer** (per-field renames, ignores, custom mappings, naming conventions). **OXY-90 asks whether a third mode — "full-auto" — should exist at all, and if so what shape it should take.** Subtasks **OXY-91** ("Create Document") and **OXY-92** ("Create Issues") are the downstream deliverables of this decision.

"Full-auto" in this context almost certainly means **zero-boilerplate automatic derivation without any explicit `given` or `autoTransform` call at the use site**. Candidate meanings to evaluate (the spike must compare them):

1. **Call-site auto-derivation** — `foo.transformInto[Bar]` implicitly derives `Transform[Foo, Bar]` at the call site if no `given` is in scope (inline implicit search + macro fallback), similar to Chimney's `foo.into[Bar].transform` or `foo.transformInto[Bar]` with auto-magic. Today `extensions.scala:4` requires `using transform: Transform[From, To]`; full-auto would make that parameter auto-derived or provide an `inline def transformIntoAuto[To]: To = ${ derive }` alternative.
2. **Global transparent derivation** — any `Transform[A, B]` is automatically available via a low-priority `given` that macro-derives for any two case classes / sealed hierarchies with matching structure (field-name equality, Option/Seq/Pure wrappers as in `DeriveProductTransform.scala:14-73`). No import or declaration needed anywhere.
3. **Cross-cutting auto-integration** — `JsonCodec`, `PageCodec`, `RequestCodec`, `RowRepr`, `SchemaLike` etc. already have `autoTransform` helpers that use `ProductGeneric.deriveTransform` for tuple<->case-class bridging (`JsonCodec.scala:25`, `PageCodec.scala:39`, `RequestCodec.scala:283/308`, `SchemaLike.scala:40`, `RowRepr.scala:48`). Full-auto would extend this so that `Transform` is never written manually — codecs/schemas auto-derive their internal transforms from the shape of `A`/`B`.
4. **Convention-based mapping** — automatic field-name convention adaptation (e.g. `snake_case` DB/API ↔ `camelCase` domain) without explicit per-field config, possibly combined with the configurable transformer.

The spike's job is **not to implement** any of these, but to answer: does any variant justify its compile-time cost, implicit-resolution complexity, and debuggability trade-off given that explicit `Transform.derived` is already cheap and that partial/configurable cover the realistic use cases (optional fields, renames)? Low priority + Architecture type signals this is intentionally a low-urgency, judgment-heavy research question — the answer may be "no, explicit is better."

**Who it affects:** Authors of the `api ↔ domain ↔ db` conversion layers (`example/.../conversion/`, `modules/domain-impl/.../conversion/`) and future users of `oxygen-transform` as a public library. The decision determines whether Oxygen embraces Chimney-style ergonomics or stays with explicit derivation (Scalafix-friendly, predictable compile errors).

**Why it matters:** The example app currently has ~15 explicit `given Transform = Transform.derived` declarations plus manual `Transform.apply { case class mapping }` for `FullUser ↔ UserRow` where field names/types differ (`hashedPassword`, `optStripeCustomerId` ↔ `stripeCustomerId`). If full-auto were adopted, most of the `Transform.derived` givens disappear, but error messages become less localized and incremental compilation may suffer. The architecture decision blocks or unblocks OXY-88/OXY-89 design (e.g. if full-auto is rejected, partial/configurable should remain explicit).

**Inferred acceptance criteria:**
1. Short decision document exists (subtask OXY-91) that enumerates at least 2–3 full-auto designs, compares them on ergonomics, compile-time cost, implicit coherence, error-message quality, and interaction with partial/configurable transformers, and ends with a clear recommendation (adopt one variant, adopt none, or prototype first).
2. If the recommendation is "adopt," follow-up implementation issues are filed (subtask OXY-92) with scope/effort for the chosen variant; if "reject," the rationale is recorded and OXY-91 closes the spike with no further issues.
3. Document references actual `oxygen-transform` code paths (`TransformMacros`, `DeriveProductTransform`, `TransformOption`/`TransformSeq`/`TransformPure` given chains) and at least one alternative library's approach (e.g. Chimney, ModelMapper) as a point of comparison — or explicitly states why no external comparison was needed.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title phrase "full-auto transform" is terse, but sibling structure is strong: OXY-87 (Epic oxygen-transform) + OXY-88 (partial transformer) + OXY-89 (configurable transformer) + OXY-90 (full-auto) form a coherent progression from least to most automatic. That framing makes "full-auto = zero-boilerplate derivation" the only interpretation that fits the trilogy.
  - Code signal is good: `Transform.derived` is explicitly required at every conversion site (verified in 4 example conversion objects), `ProductGeneric.deriveTransform` already powers `autoTransform` for codecs (verified in `JsonCodec`/`PageCodec`/`RequestCodec`), so the gap "why not auto everywhere?" is a concrete, repo-grounded question.
  - Type `Architecture` + priority `Low` + subtasks "Create Document" / "Create Issues" confirm this is a research/spike, not an implementation task — consistent with "figure out if it makes sense."
  - Remaining uncertainty keeps it at 4 not 5–6: without the Jira body, whether "full-auto" means call-site auto-derivation vs. global implicit vs. codec integration vs. naming-convention auto-mapping cannot be known with certainty; all four variants above are plausible readings of the same title.

## Required Changes

This is a **research/architecture-decision task — no production code changes expected** unless the spike recommends a prototype. Concrete work inferred from repo conventions and the transform module's current design:

- [ ] **Survey current state** — document existing transform capabilities and their explicitness cost:
  - `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala` / `TransformOrFail.scala` — `Transform`/`TransformOrFail` traits, `TransformOption`/`TransformSeq`/`TransformPure` implicit chains, `derived` macro entry points.
  - `modules/general/transform/src/main/scala/oxygen/transform/generic/TransformMacros.scala`, `DeriveProductTransform.scala` (field-name equality + per-field `Transform[F,T]` search, error on missing field), `DeriveProductTransformOrFail.scala` (field-scoped `atField` errors), `DeriveSumTransform.scala` / `DeriveSumTransformOrFail.scala` (case-name equality).
  - `modules/general/transform/src/main/scala/oxygen/transform/extensions.scala` — `transformInto` / `transformIntoOrFail` requiring `using`.
  - `modules/general/transform/src/test/scala/oxygen/transform/TransformSpec.scala`, `TransformOrFailSpec.scala`, `transformers.scala`, `models.scala` — current derived usage, tests for Option/Seq wrappers and sum types.
  - Call-site pattern in `example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala`, `domainToApi.scala`, `modules/domain-impl/src/main/scala/oxygen/example/conversion/domainToDb.scala`, `dbToDomain.scala` — explicit `given Transform = Transform.derived` per pair plus manual lambdas where field names diverge.
  - Existing `autoTransform` precedent: `JsonCodec.scala:25`, `PageCodec.scala:39`, `RequestCodec.scala:283/308`, `ResponseCodecNoStatus.scala:32`, `SchemaLike.scala:40`, `RowRepr.scala:48`, `modules/general/core/.../ProductGeneric.scala:223` — tuple<->case class bridging without per-field givens.

- [ ] **Create decision document (OXY-91)** — new markdown file, e.g. `agent-docs/transform-full-auto-decision.md` or `docs/docs/transform/full-auto.md` (follow `agent-docs/http-docs/` and `agent-docs/mcp/mcp-feature-plan.md` precedent). Should cover:
  - At least 3 candidate designs with sketch API (code snippets showing before/after at a conversion site), e.g.:
    1. `inline def transformIntoAuto[To]: To` that falls back to `Transform.derived` if no given in scope.
    2. Low-priority `given autoDerived: [From, To] => Transform[From, To] = Transform.derived` (global transparent).
    3. Chimney-style `foo.into[Bar].withFieldComputed(...).transform` / `Transformer` DSL (opt-in full-auto with escape hatches).
  - Comparison matrix: boilerplate saved, compile-time / incremental compilation impact, quality of error messages (missing field vs. implicit-not-found), interaction with `TransformOrFail` / `TransformOption` / `TransformSeq`, coherence/orphan-instance risk, and fit with OXY-88 (partial) and OXY-89 (configurable) — does full-auto subsume them or conflict?
  - Brief survey of prior art (Chimney, Scala 3 `Mirror`-based auto derivation, MapStruct/ModelMapper) or explicit statement that no external survey was deemed worthwhile.
  - Clear recommendation: adopt / reject / prototype-then-decide, with rationale. If adopt, outline migration (codemod for existing `given Transform = Transform.derived` sites) and whether full-auto should be opt-in via import (`import oxygen.transform.auto.given`) or on by default.

- [ ] **Create follow-up issues (OXY-92)** — if recommendation is adopt, file 1–3 implementation issues (e.g. "Implement call-site auto-derivation", "Add auto import + benchmark compile times", "Update example conversions to use full-auto"). If reject, file a single closing issue or mark OXY-92 as "no implementation needed, see decision doc" with justification. Issues should be children of Epic OXY-87 or linked to OXY-88/OXY-89 as appropriate.

- [ ] **Optional prototype (only if decision is borderline)** — a branch that implements the preferred full-auto variant behind an `auto` import and measures: (a) lines saved in `example/.../conversion/`, (b) compile-time delta (`sbt 'project oxygen-transformJVM' compile` before/after), (c) error-message quality for a mismatched field. Not required for the spike's acceptance, but could be proposed.

- [ ] **No data-model / schema / migration changes** — purely additive decision doc and (optionally) additive `auto` helpers; no backwards-compat risk to existing explicit `Transform.derived` sites. If full-auto is adopted as opt-in, existing code is untouched.

- [ ] **Tests/docs:** No production tests required for the spike itself. The decision doc is the testable artifact (reviewed). If a prototype is built, a single test asserting `case class A(x: Int); case class B(x: Int); A(1).transformIntoAuto[B] == B(1)` demonstrates the mechanism.

- **Verified vs. inferred:** That `Transform.derived` is explicit at every call site, that `ProductGeneric.deriveTransform` powers `autoTransform` in 6 places, and that partial/configurable/full-auto form a trilogy under Epic OXY-87 were verified by reading the files and checklist. That "full-auto" means zero-boilerplate call-site/global auto-derivation (vs. convention-based naming or codec integration) is inferred from the title and sibling issue names — the Jira body was not fetched.

## Estimates & Autonomy

- **Story points:** 2 (Fibonacci) — half-day spike to read the transform macros, sketch 2–3 API alternatives, and write a 1–2 page decision doc plus 1–2 follow-up issue stubs. If a prototype + compile-time benchmark is included, 3–5. As an Architecture spike (not an Epic implementation), it is intentionally small; Large/Epic sizing would apply to the follow-up implementation epic, not this decision.
  - Justification: Research-only, no production code, bounded scope to one module (`oxygen-transform`) and its example usages.
- **Autonomy:** 4 / 6 — mostly autonomous with light human review on the recommendation.
  - Justification: An agent with repo access can enumerate designs from the codebase and public prior art (Chimney docs), draft the comparison matrix, and propose a recommendation without blocking on humans. Final human judgment is needed because "makes sense" is a product/taste question (compile-time vs. ergonomics trade-off) that benefits from maintainer input before filing implementation issues.
- **Ambiguity-to-resolve:** 4 / 6 — meaningful ambiguity that should be time-boxed, not resolved upfront.
  - Justification: The title is intentionally open-ended ("if any form ... makes sense") and no acceptance criteria are stated. The spike itself exists to resolve that ambiguity, but a brief clarification ("is this about call-site auto-derivation, global implicits, or convention-based mapping? examples of what 'makes sense' would look like?") would sharpen the doc outline and shorten the spike. Without it, the agent must assume the call-site/global reading and document the assumption.

## Open Questions

1. **Intended meaning of "full-auto":** Does the requester mean call-site auto-derivation (`transformIntoAuto` without `given`), global transparent implicits (`given autoDerived`), Chimney-style `into[To].transform` DSL, or convention-based field-name adaptation (camel ↔ snake) — or all of the above evaluated together? The spike currently assumes #1/#2 as frontrunners.
2. **Scope vs. OXY-88/OXY-89:** Should full-auto subsume partial (allow missing/extra fields) and configurable (renames/ignores), or is full-auto specifically "exact field-name match, no config" with partial/configurable as the escape hatches for non-trivial cases? This determines whether full-auto can be evaluated in isolation.
3. **Opt-in vs. on-by-default:** If any full-auto variant is adopted, should it be opt-in (`import oxygen.transform.auto.given`) to preserve explicitness by default, or on by default with an opt-out? The latter has larger coherence/compile-time implications.
4. **Interaction with `TransformOrFail`:** Should full-auto also cover fallible transforms (`String` → `PositiveInt` via `TransformOrFail.fromEitherF`) or remain `Transform`-only (infallible)? The current `TransformOrFail` derivation already handles `Option`/`Seq` fallibly — should auto-derivation try `TransformOrFail` first and degrade?
5. **Compile-time budget:** What compile-time regression is acceptable? Full-auto via low-priority `given` would trigger implicit search on every `transformInto` call site, potentially slowing `example` and downstream services. Is there a target (e.g. <5% compile-time increase)?
6. **Chimney as precedent or anti-precedent:** Is Chimney's ergonomics (and its known incremental-compilation / error-message costs) considered a positive reference or a cautionary tale for Oxygen? Knowing the maintainer's stance would simplify the recommendation.
7. **Deliverable location:** Should the decision document live under `agent-docs/transform/` (spike notes, like `agent-docs/mcp/`) or `docs/docs/transform/` (published docs)? And should OXY-91/OXY-92 be filed as children of Epic OXY-87 or as standalone tasks?
8. **Assumption to confirm:** That this spike is deliberately Low priority / Architecture and may conclude "no, explicit `Transform.derived` is the right default — do not add full-auto" — i.e., a "reject" outcome is acceptable and not a failure of the spike.

