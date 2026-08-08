# OXY-89 — Add support for configurable transformer

## Original
- **Key:** OXY-89
- **Checklist line:** `- [ ] [OXY-89](https://kr-oxygen.atlassian.net/browse/OXY-89) — **Task** · Lower — Add support for configurable transformer`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add support for configurable transformer
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-89
- **Checklist section:** To Do
- **Epic:** OXY-87 — oxygen-transform (In Progress) — Epic filter: PASS — belongs to OXY-87, direct module match `modules/general/transform` (`oxygen-transform` CrossProject in `build.sbt:491-499`), sibling to OXY-88 (partial transformer) and OXY-90 (full-auto architecture)

## Expanded Description

**What this likely is:** Extend `oxygen-transform` (`modules/general/transform`) so a derived `Transform[From, To]` / `TransformOrFail[From, To]` can be **configured per-field** at derivation site, instead of today's all-or-nothing exact-name automatic derivation. Today `Transform.derived[From, To]` (`modules/general/transform/src/main/scala/oxygen/transform/Transform.scala:56`, macro in `generic/TransformMacros.scala:10-27`) requires:

1. `From` and `To` are both `Case` (or both `Sealed`) with the same product/sum structure (`TransformMacros` errors otherwise),
2. Every field in `To` has an identically-named field in `From` (`DeriveProductTransform.scala:38-49` does `fromFieldMap.get(toField.name).getOrElse(error)`), extra `From` fields are silently ignored, and
3. An implicit `Transform[F,T]` (or `TransformOrFail[F,T]`) exists for each field-type pair (`Implicits.searchOption[Transform[F,T]].getOrElse(error)` at `DeriveProductTransform.scala:57-65`).

There is no way to rename a field, compute a field from the whole source, supply a default, ignore a target field, or apply a naming strategy — callers must either make the two case classes agree exactly or write a hand-written `Transform.fromF` / `TransformOrFail.fromEitherF` instance (as in `example/apps/web-server/src/main/scala/oxygen/example/conversion/domainToApi.scala:31` for `apiToDomain` where `CreateComment.postId` extra param breaks derivation and falls back to a manual extension).

A "configurable transformer" is the standard Chimney/MapStruct-style DSL that sits between fully-automatic `derived` and fully-manual `fromF`: a builder/annotations layer that lets the derivation be tweaked without leaving the macro. Concretely this is expected to be a fluent builder similar to `Transform[From,To].withFieldRenamed(_.foo, _.bar).withFieldComputed(_.baz, src => ...) ... .build` or an annotation-driven variant (`@transformField("otherName")`, `@transformIgnore`), or a `TransformConfiguration` value threaded into a new `Transform.derivedWith[From,To](config)` / `Transform.derivedConfigured` overload. The minimal useful surface, inferred from what `DeriveProductTransform(OrFail)` currently cannot do and what sibling `OXY-88` (partial transformer) and `OXY-90` (full-auto) bound, is:

- **Field rename** — `To.bar` populated from `From.foo` where names differ (the most common "configurable" need; today it is a hard compile error at `DeriveProductTransform.scala:43-48`).
- **Computed / constant field** — `To.x` produced from `From` as a whole (`src => ...`) or from a literal/default, rather than from a single same-named field.
- **Ignore / default** — allow `To` fields absent in `From` to be supplied with a default value or `Option` fallback instead of aborting derivation.
- **Type-level customization per field** — override the implicit `Transform[F,T]` for one field with an explicit function (e.g. `String -> Email` via `Email.apply` vs. global given).
- **Naming strategy** — optional global config such as `snake_case <-> camelCase` folding, so `user_name` maps to `userName` without per-field boilerplate.

This is distinct from **OXY-88 `partial transformer`** (Lower) which most naturally means `Transform[From, Partial[To]]` / `TransformOrFail` handling of `Option` fields as "maybe present" (already partially covered by `TransformOrFail.TransformRequireOption` at `TransformOrFail.scala:33-37` but not for `Transform` infallible), and from **OXY-90 `full-auto`** (Architecture) which asks whether derivation could happen with *zero* `given` boilerplate (e.g. `from.transformInto[To]` without an explicit `given Transform[From,To] = Transform.derived` in scope). Configurable is the middle ground: derivation stays explicit and opt-in, but is no longer rigid.

**Who it affects:** Any author using `oxygen-transform` to map between `api` / `domain` / `db` models (`example/apps/web-server/src/main/scala/oxygen/example/conversion/apiToDomain.scala:14-16`, `domainToApi.scala:47`, `domainToDb.scala:54`, `dbToDomain.scala:53-67`). Today teams keep `Api` and `Domain` field names artificially aligned to keep `Transform.derived` compiling; with configurable transforms they can evolve `Api` independently (e.g. `snake_case` JSON vs `camelCase` domain), add API-only computed fields, or map legacy DB column names.

**Why it matters (Lower):** Priority `Lower` (not High/Normal) signals DX sugar inside the `oxygen-transform` Epic OXY-87 (`In Progress`). The core `Transform` / `TransformOrFail` machinery already ships and is usable (used in 10+ places in `example/`); lack of configuration is friction, not a blocker. It becomes more valuable as `OXY-88` and `OXY-90` clarify the spectrum: partial vs configurable vs full-auto.

**Inferred acceptance criteria (from code, not from a fetched Jira body):**

1. A new API allows `Transform[From,To]` (and `TransformOrFail[From,To]`) to be derived with per-field configuration — either a builder DSL (`TransformConfiguration` / `Transformer[From,To].withFieldRenamed` / `withFieldComputed` / `withDefault` chaining) or field annotations (`@rename`, `@ignore`, `@computed`) — without falling back to a hand-written `Transform.fromF` for the whole product.
2. At minimum, **field rename** works for products: `DeriveProductTransform` looks up field mappings through the config, not just `fromFieldMap.get(toField.name)`, and errors remain readable (current `report.errorAndAbort` messages at `DeriveProductTransform.scala:43-48` are preserved for still-unmapped fields). Sum support (`DeriveSumTransform`) remains exact-name; configurable renames are product-field scoped in v1.
3. Existing `Transform.derived` / `TransformOrFail.derived` continue to compile unchanged (backwards compat); configurable form is an additional entry point (`derivedWith`, `configure`, or `Transformer` builder) so `example/` conversions (`apiToDomain.scala:14`, `domainToApi.scala:47`) are not forced to migrate.
4. Tests in `modules/general/transform/src/test/scala/oxygen/transform/TransformSpec.scala` / `TransformOrFailSpec.scala` (or new `TransformConfigurableSpec`) cover rename, computed, and default/ignore cases, plus error-path tests that missing required renamed fields still emit `TransformError` with correct `ScopePath.Field` / `ScopePath.SubType` prefixes (as in `TransformOrFailSpec.scala:132-163`). Infallible `Transform` and fallible `TransformOrFail` both have coverage if both get configurable variants.
5. No change to `TransformError` shape (`modules/general/transform/src/main/scala/oxygen/transform/TransformError.scala`) beyond using existing `atField`/`atSubType` wrappers; configurable renaming preserves accurate path reporting.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Module is unambiguous: title contains `transformer`, `build.sbt:491` defines `oxygen-transform` at `modules/general/transform`, Epic OXY-87 `oxygen-transform` is `In Progress` and siblings OXY-88 (partial), OXY-90 (full-auto) frame a coherent transform feature spectrum — so "configurable" belongs to this module.
  - Code signal for *what is currently not configurable* is strong: `DeriveProductTransform.scala:14-49` and `TransformMacros.scala:10-27` enforce exact name match and implicit-per-field lookup with no hook for renaming/computed/defaults, and `example/` shows the workaround (hand-written function at `apiToDomain.scala:30-31` for `CreateComment` with extra param) — so the gap the issue names is real and located.
  - Downgraded from 4/5 because the title is 4 words with no Jira body/TODO/design-doc fetched and no `grep -r configurable` hit in `modules/general/transform` — the exact shape of "configurable" is underspecified. It could mean (a) Chimney-style fluent DSL, (b) annotations, (c) a global `TransformConfig` with naming strategy / field-name normalization, or (d) per-field typeclass overrides only. All satisfy the title, and the repo gives no pin. Priority `Lower` also gives no scope hint.
  - No network fetch of `https://kr-oxygen.atlassian.net/browse/OXY-89` was attempted; inference rests on `checklist.md` + repo grep + reading `Transform` / `TransformOrFail` / `DeriveProduct*` / tests, per `jira-issues/agent-instructions.md` fallback.

## Required Changes (only if Confidence >= 3)

> Confidence 3 — proceeding with deeper analysis per instructions.

- [ ] **Core API — new configurable entry point (Inferred design, choose one; both are common precedents)**
  - Option A (builder DSL, Chimney-style) — `modules/general/transform/src/main/scala/oxygen/transform/Transform.scala` and `TransformOrFail.scala` gain a builder:
    ```scala
    Transform.derivedWith[From, To]
      .withFieldRenamed(_.foo, _.bar)
      .withFieldComputed(_.baz, src => ...)
      .withDefault(_.qux, 42)
      .build
    ```
    Implemented as a new `Transformer[From,To]` opaque type or `TransformConfiguration` that `TransformMacros.deriveTransformWith` consumes. Preserves existing `Transform.derived` as alias to `Transform.derivedWith(...).build` with empty config for backward compat.
  - Option B (annotation-driven) — introduce annotations `oxygen.transform.annotation.rename("other")` / `@ignore` / `@computed` in `modules/general/transform/src/main/scala/oxygen/transform/annotation.scala` that `DeriveProductTransform` reads via `ProductGeneric.Field.annotations` (cf. `docs/docs/metaprogramming/index.md:48-53` and `oxygen.sql.schema` `@tableName` precedent in `docs/docs/sql/models.md:27`).
  - Decision required (see Open Questions). Either way, the macro layer `generic/TransformMacros.scala` gains `deriveTransformConfigured` / `deriveTransformOrFailConfigured` that thread the config into `DeriveProductTransform(OrFail)`.
  - **Verified vs. inferred:** That `TransformMacros` and `DeriveProductTransform(OrFail)` are the loci of change is verified (they are the only derivation paths). That "configurable" means a builder vs. annotations is inferred from the title and Chimney/MapStruct precedent — no repo `TODO` pins the choice.

- [ ] **Macro — `DeriveProductTransform.scala` + `DeriveProductTransformOrFail.scala` (Verified)**
  - Extend `fromFieldMap` lookup to consult config: for each `toField`, resolve `(fromFieldName, transformExpr)` via config mapping first, else fallback to exact-name `fromFieldMap.get(toField.name)`. For computed/default fields, bypass `fromField` and generate `Expr[T]` directly from the user-supplied function/literal.
  - Thread per-field custom `Transform[F,T]` overrides where config supplies an explicit `Expr[Transform[F,T]]` instead of `Implicits.searchOption`.
  - Preserve error messages (`report.errorAndAbort` at `DeriveProductTransform.scala:43-48`, `57-65`) for fields still unmapped after config, with `toField.pos` positioning.
  - If naming-strategy support is included (e.g. `snake_case` -> `camelCase`), add a `FieldNameMapping` step that normalizes names before lookup, shared between `Transform` and `TransformOrFail` paths.

- [ ] **Sum support — `DeriveSumTransform.scala` / `DeriveSumTransformOrFail.scala` (Verified, Inferred scope)**
  - V1 can leave sum derivation exact-name only (no configurable case renames) and document it; if case-level rename is desired, mirror the product config pattern with `toCaseMap` (`DeriveSumTransformOrFail.scala:8-9`) consulted via config. Call out in docs. Keep `MatchBuilder`-based generation (`DeriveSumTransform.scala:41-48`) unchanged beyond the map lookup.

- [ ] **New type — `TransformConfiguration` / `Transformer` builder or annotation definitions (Inferred)**
  - If builder DSL: new file `modules/general/transform/src/main/scala/oxygen/transform/TransformConfiguration.scala` (or `Transformer.scala`) defining the builder types, `withFieldRenamed` / `withFieldComputed` / `withDefault` methods, and inline macro entry points. Must be `CrossType.Pure` so JS/Native tests compile (`build.sbt:492-493`).
  - If annotation-driven: new file `modules/general/transform/src/main/scala/oxygen/transform/annotation.scala` with `@transformRename`, `@transformIgnore` etc., plus docs on `ProductGeneric` annotation propagation.

- [ ] **Error path — `TransformError.scala` (Verified, no change expected)**
  - Confirm `ScopePath.Field` / `ScopePath.SubType` / `ScopePath.Index` wrapping still yields correct paths for configurable mappings (rename should still report `toField.name`, not `fromField.name`). Add assertion in new tests that `TransformOrFail.derivedWith` failure for a renamed required field reports `ScopePath.Field("toName")` with `Cause.MissingRequired` as in `TransformOrFailSpec.scala:132-139`.

- [ ] **Tests — `modules/general/transform/src/test/scala/oxygen/transform/` (Verified)**
  - New `TransformConfigurableSpec.scala` (or `TransformRenamedSpec.scala`) covering: (a) rename `From.foo -> To.bar`, (b) computed field `To.x = f(From)`, (c) default/ignore for absent `To` field, (d) custom per-field `Transform` override, (e) backward compat `Transform.derived` still passes. Mirror existing helpers `makeTest` / `makeSuccess` / `makeFailure` from `TransformSpec.scala:8` and `TransformOrFailSpec.scala:8-16`. Also test `ScopePath` error prefixing as in `TransformOrFailSpec:148-163`.

- [ ] **Docs — `docs/docs/` or `modules/general/transform/README.md` (Inferred)**
  - Brief section showing before/after: `given Transform[ApiUser, DomainUser] = Transform.derived` (fails on rename) vs. `Transform.derivedWith[ApiUser, DomainUser].withFieldRenamed(_.api_name, _.domainName).build`. Explain why `Lower` priority: convenience, not correctness, and relationship to OXY-88 / OXY-90. If annotations chosen, document them alongside `oxygen.sql.schema` annotation table pattern (`docs/docs/sql/models.md:23-27`).

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 5 (Fibonacci) — lean to 3 if scope is rename-only for `Transform` products without `TransformOrFail` / sums / naming strategy; 8 if full DSL with builder + annotations + naming strategy + both `Transform` and `TransformOrFail` + JS/Native cross-build + docs. Rationale: touches macro-heavy derivation (`TransformMacros`, `DeriveProduct*`, `DeriveSum*`), requires careful implicit search and `Expr` plumbing, and must preserve source positions / error messages. Smaller than full Epic OXY-87 but larger than a single-field `RowRepr.transform` task.
  - Justification: Requires new types + macro overloads + per-field mapping logic in two parallel hierarchies (`Transform` infallible and `TransformOrFail` fallible) plus tests. Well-isolated to `modules/general/transform` (`build.sbt:491-499` `CrossType.Pure`), no DB migration or HTTP schema change, but macro `Quotes` code is easy to get wrong on implicit scoping / `ValDef` caching (`DeriveProductTransform.scala:23-24` pattern must be replicated).

- **Autonomy 1–6:** 3 / 6 — moderately guided (needs human shape decision)
  - Justification: Repo + `checklist.md` + macro code make the *problem* clear, but the *API shape* (builder vs. annotations vs. `deriveWith` config value vs. supporting both) is not pinned by the title and has tradeoffs that affect every call site in `example/` conversions. A sub-agent can implement one coherent option end-to-end, but should confirm the preferred surface with a human before doing macro work to avoid rework.

- **Ambiguity-to-resolve 1–6:** 4 / 6 — significant ambiguity blocks start
  - Justification: Four blocking questions (see Open Questions) must be answered before coding: DSL vs. annotations, whether `TransformOrFail` is in scope for v1, whether sums/cases can be renamed or only product fields, and whether a global naming strategy (snake_case) is required or per-field rename suffices. `Lower` priority gives no guidance, and no design doc exists at `docs/` or in `modules/general/transform` comments.

## Open Questions

1. **DSL shape — builder vs. annotations vs. both?** Chimney uses `Transformer[From,To].define.withFieldRenamed(...).buildTransformer`; MapStruct uses annotations. Which precedent should `oxygen-transform` follow? Builder is more expressive (computed fields, constants) but heavier macro; annotations are lighter but less powerful. Should v1 support both?

2. **Scope — `Transform` only or also `TransformOrFail`?** The issue says "configurable transformer" singular. Does it cover `TransformOrFail.derivedWith` (fallible, with `Option -> required` `MissingRequired` semantics at `TransformOrFail.scala:33`) or only `Transform.derivedWith` (infallible)? Most teams need configurable renames in fallible context too.

3. **Products only or sums too?** Should case renames (`From.Foo -> To.Bar` where enum case names differ) be configurable, or is product-field rename sufficient for v1? `DeriveSumTransform` (`DeriveSumTransform.scala:20-36`) currently requires exact case name match — extending it is extra work.

4. **Naming strategy vs. per-field rename?** Is a global strategy like `FieldNameStyle.SnakeCaseToCamelCase` in scope, or should every mismatch be an explicit `.withFieldRenamed`? The former is often requested for `api (snake_case JSON) -> domain (camelCase)` but was not mentioned in the title.

5. **Computed fields with extra context?** Should `withFieldComputed` receive the whole `From` (e.g. `src => src.a + src.b`) or only the source field (handled today by custom `Transform[F,T]` given)? And should `withConstant` / `withDefault` be separate combinators or unified?

6. **Back-compat guarantee:** Must existing `Transform.derived` call sites (`apiToDomain.scala:14-16`, `domainToApi.scala:47`, `domainToDb.scala:54`) remain valid without migration, or is a breaking `derived -> derivedWith` rename acceptable at Epic level?

7. **Relationship to OXY-88 and OXY-90:** Does `configurable` subsume `partial transformer` (OXY-88) — e.g. `partial` is just `configurable` with `withDefault` / `withOption` — or are they independent features? And does `configurable` preclude or enable `full-auto` (OXY-90)? Clarifying the epic roadmap avoids duplicate work.
