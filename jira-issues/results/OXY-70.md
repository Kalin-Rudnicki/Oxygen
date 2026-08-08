# OXY-70 — Improve representation and differentiation between Option/Specified in json schema

## Original
- **Key:** OXY-70
- **Checklist line:** `- [ ] [OXY-70](https://kr-oxygen.atlassian.net/browse/OXY-70) — **Task** · High — Improve representation and differentiation between Option/Specified in json schema`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Improve representation and differentiation between Option/Specified in json schema
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-70
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Clean up how `oxygen-schema` (`modules/general/schema`) models and surfaces the difference between `Option[A]` and `Specified[A]` inside JSON schemas — both at the type level and in the compiled/schema-emission layer that feeds OpenAPI / MCP JSON Schema (`JsonSchemaEmitter`), API docs (`ApiSpecPage`), and backwards-compat checks (`compat/ComparisonResult`).

Today the codebase has three wrappers with overlapping semantics:
- `Option[A]` — `JsonSchema.OptionSchema` / `IntermediateRepr.JsonOption` / `CompiledSchemaRef.JsonOption` — JSON field is **optional + nullable** (`null` or value; missing decodes as `None`). In `RawCompiledJsonSchema.ProductField` this is encoded as `nullable=true, onMissing=Null`.
- `Specified[A]` — `JsonSchema.SpecifiedSchema` / `JsonSpecified` / `JsonSpecified` — tracks **presence vs. absence** for partial-update / PATCH semantics (three states when combined with `Option`: `WasNotSpecified` = field absent, `WasSpecified(None)` = explicit `null`, `WasSpecified(Some(v))` = value). In product fields: `nullable = underlyingNullable, onMissing = Undefined` (`Specified.WasNotSpecified`).
- `Nullable[A]` — `NullableSchema` / `JsonNullable` — explicit `Nullable[A]` (required-but-nullable) encoded as `nullable=true, onMissing=None` (field must be present, may be `null`).

These distinctions are already partially implemented at the **product-field** level (`CompiledSchemaRef.resolveJsonConcrete` in `CompiledSchemaRef.scala:148`, `RawCompiledJsonSchema.ProductField`), and JSON codecs in `oxygen-json` correctly implement `onMissingFromObject` / `addToObject` (`SpecifiedDecoder` → `WasNotSpecified` on missing; `OptionDecoder` → `None` on missing or `null`; encoders omit when `addToObject=false`). But the **representation** is leaky:

1. **Erasure in `FullCompiledSchemas`.** `FullCompiledSchemas.scala:81-86` resolves `JsonOption`, `JsonNullable`, and `JsonSpecified` refs by immediately delegating to the underlying type (`resolveJson(elemType).value`) with `TODO: might need to keep some representation of nullable`. So outside a product field (e.g. top-level `Option[List[Foo]]`, `Array[Specified[Bar]]`, `Map[K, Specified[V]]`) the wrapper disappears from `FullCompiledSchema` and therefore from `JsonSchemaEmitter`, `FullCompiledSchemas.allSchemas`, and `Compared` diffing.
2. **Emitter conflation.** `JsonSchemaEmitter.scala:18-19` notes wrappers are "already resolved away" and only `ProductField.nullable` + `required` (derived as `onMissing.isEmpty`) drive emitted JSON Schema (`required` array + `maybeNullable` / `anyOf: [type, null]`). This makes `Option[String]` (optional+nullable) look identical to `Specified[String]`-as-field in emitted schema except for nullability nuance, and hides the PATCH intent.
3. **Display conflation.** `ApiSpecPage.displayType` just unwraps to `primaryReference` for these wrappers, and `CompiledSchemaRef.showBase` renders `Option<X>` vs `Specified<X>` correctly at the ref level, but the rendered product field table in the docs site does not surface `onMissing`/`nullable` meaningfully — a required-but-nullable `Nullable[A]` vs optional `Option[A]` vs patch-optional `Specified[A]` look the same to a consumer.
4. **Compat semantics.** `compat/ComparisonResult.FieldComparison` does capture `nullable` + `onMissing` + `underlying`, so breaking-change detection is field-level accurate, but only because the product field preserved it. Any future or non-product usage lacks a comparable signal.

Why it matters (High priority): `Option` vs `Specified` is load-bearing for API correctness. `Option` is the default "field is optional in the JSON" story; `Specified` is the PATCH story (`{}`, `{"field":null}`, `{"field":5}` are intentionally distinct — see `Specified.scala:12-15`). If the schema layer does not crisply distinguish them, generated JSON Schema / OpenAPI is wrong, MCP `inputSchema` is wrong, docs are misleading, and compat checks may allow or reject the wrong evolution (e.g. changing a required `String` field to `Option[String]` vs to `Specified[String]` vs to `Specified[Option[String]]` have different compatibility meanings).

**Inferred acceptance criteria (from code, not from a Jira body):**
- `Option[A]`, `Specified[A]`, `Specified[Option[A]]`, and `Nullable[A]` have distinct, documented semantics in `oxygen-schema` and distinct compiled representations that survive outside product fields (no erasure in `FullCompiledSchemas`).
- Emitted standard JSON Schema (`JsonSchemaEmitter`) faithfully reflects the distinction: `Option[A]` field → not in `required`, schema is `anyOf: [A, null]`; `Specified[A]` where `A` non-nullable → not in `required`, schema is `A` (not nullable); `Specified[Option[A]]` → not in `required`, schema is `anyOf: [A, null]`; required `Nullable[A]` → in `required`, schema is `anyOf: [A, null]` (or equivalent depending on design choice). Or an equivalently justified mapping, documented.
- `ApiSpecPage` / `FullCompiledSchema.toIndentedString` surface `nullable` and `onMissing` (or wrapper kind) per field so a reader can tell Option vs Specified vs Nullable vs required.
- `compat` diffing treats Option↔Specified, `String`↔`Option[String]`↔`Specified[String]`, and `Option[String]`↔`Specified[Option[String]]` correctly as distinct evolutions with appropriate `ExactEqual` / `FromIsMoreSpecific` / `ToIsMoreSpecific` / `NotComparable` results.
- `oxygen-json` codecs already do the right thing on wire (`SpecifiedEncoder.addToObject`, `SpecifiedDecoder.onMissingFromObject`, etc.); any schema change preserves that behaviour and adds tests for the three-state matrix.
- Docs updated (`docs/docs/<schema>` or `modules/general/schema/README`) explaining when to use which wrapper.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Wrapper trio `Option`/`Specified`/`Nullable` is explicit in `JsonSchema.scala:85-87`, `IntermediateRepr:36-38`, `CompiledSchemaRef:132-140`, and `RawCompiledSchema:218-220` — the task maps directly to this code.
  - Product-field encoding (`nullable` + `DecodeMissingAs` = `Null` for `Option`, `Undefined` for `Specified`, `None` for `Nullable`) is the current differentiation point (`CompiledSchemaRef.scala:164-172`, `RawCompiledSchema.scala:275-298`), so the weakness is localized.
  - `FullCompiledSchemas.scala:81-86` TODOs and `JsonSchemaEmitter.scala:18-19` comment ("wrappers are already resolved away") are direct signals that representation is incomplete/erased — strongly supports the "improve representation" reading.
  - Downgraded from 5 because no Jira body was fetched, the emitting/mapping choice for JSON Schema draft 2020-12 is still a design decision (required vs nullable vs `anyOf` tradeoffs), and sibling `OXY-147` (`JsonEncoder.Omit`) suggests an alternative ongoing direction that could overlap/conflict with the chosen fix.

## Required Changes

- [ ] `modules/general/schema/src/main/scala/oxygen/schema/JsonSchema.scala` — clarify/keep `OptionSchema`/`SpecifiedSchema`/`NullableSchema` definitions; ensure `__internalReferenceOf` strings are distinct (they already are: `JsonOption(...)` vs `JsonSpecified(...)` — keep). Optionally add `Omit`-related handling if `OXY-147` lands first.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/intermediate/IntermediateRepr.scala` — keep `JsonOption`/`JsonNullable`/`JsonSpecified` reprs; no change unless wrapper is unified.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compiled/CompiledSchemaRef.scala` — keep `JsonOption`/`JsonSpecified`/`JsonNullable` refs; ensure `primaryReference`/`toConcrete`/`showBase` remain distinct (they are). Verify `resolveJsonConcrete` correctly distinguishes `onMissing` (`Null` vs `Undefined` vs `None`) and `nullable`.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compiled/RawCompiledSchema.scala` — keep `ProductField(nullable, onMissing, fieldType)`; document the meaning of each combination. Ensure `convertRepr` for `JsonProduct` uses `resolveJsonConcrete` for every field.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compiled/FullCompiledSchemas.scala` — **main fix site**: stop erasing wrappers in `conversion.jsonRef` for `JsonOption`/`JsonNullable`/`JsonSpecified` (lines 81-86). Either (a) introduce `FullCompiledJsonSchema.JsonOption`/`JsonNullable`/`JsonSpecified` wrapper nodes that delegate to underlying but preserve identity, or (b) document and justify that erasure is intentional for non-product contexts and ensure `JsonSchemaEmitter`/`Compared` handle the wrapper refs directly instead of delegating. Update `mutableInternalState` maps accordingly.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compiled/JsonSchemaEmitter.scala` — ensure `emit` handles `JsonOption`/`JsonSpecified`/`JsonNullable` refs when they survive (if wrappers preserved), or at least document why product-field handling suffices. Verify `productBody` required/nullability mapping: `required = onMissing.isEmpty`, `maybeNullable` on `nullable`. Add tests for each wrapper combination.
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compiled/FullCompiledSchema.scala` / `RawCompiledSchema.scala` `toIndentedString` — surface per-field `nullable`/`onMissing` (already does: `ProductField.toIndentedString` prints `nullable` + `on-missing`); verify `JsonSchemaEmitter`-independent diagnostics show Option vs Specified clearly.
- [ ] `modules/ui/web/src/main/scala/oxygen/ui/web/apispec/ApiSpecPage.scala` — render field pills/types to distinguish optional vs patch-optional vs nullable (e.g. `Option[String]` vs `Specified[String]` badges, or `required` column derived from `onMissing.isEmpty` vs `nullable` marker).
- [ ] `modules/general/schema/src/main/scala/oxygen/schema/compat/Compared.scala` + `ComparisonResult.scala` — verify diffing of `ProductField` (`nullable` + `onMissing` + `underlying`) correctly classifies Option↔Specified transitions; add branch for top-level wrapper diff if wrappers are preserved as nodes.
- [ ] `modules/general/json/src/main/scala/oxygen/json/JsonDecoder.scala` + `JsonEncoder.scala` — no functional change expected (already correct), but add regression tests for `Specified[Option[A]]` three-state matrix vs `Option[A]` vs `Nullable[A]` at the product-field level and at top-level (`JsonSchema[Specified[A]]` direct encode/decode).
- [ ] Tests — `modules/tests/pre-test-unit-tests/src/test/scala/oxygen/schema` (and/or `modules/general/schema/src/test`) — add/update: (a) `IntermediateRepr` → `RawCompiledJsonSchema` → `FullCompiledSchemas` round-trips for each wrapper combo, (b) `JsonSchemaEmitter` golden-file tests for a product with fields of types `String`, `Option[String]`, `Specified[String]`, `Specified[Option[String]]`, `Nullable[String]`, (c) `compat` comparison matrix, (d) `oxygen-json` `Specified[Option[_]]` wire tests.
- [ ] Docs — `docs/docs/schema` or module README — add section documenting when to use each wrapper (Option = optional field, Specified = patch field, Nullable = required-but-nullable) with wire examples `{}` / `{"f":null}` / `{"f":val}`.

**Verified vs inferred:** File/method names and current encodings were verified by reading `Specified.scala`, `JsonSchema.scala`, `IntermediateRepr.scala`, `CompiledSchemaRef.scala`, `RawCompiledSchema.scala`, `FullCompiledSchemas.scala`, `JsonSchemaEmitter.scala`, `FullCompiledSchema.scala`, `Compared.scala`, and `oxygen-json` encoders/decoders listed above. The *choice* to preserve wrappers as `FullCompiledJsonSchema` nodes vs keep product-field-only differentiation is inferred — the current TODOs point to (a) but either is defensible if justified and tested.

## Estimates & Autonomy
- **Story points:** 5 (Fibonacci) — medium, cross-cutting schema internals change: wrapper representation in `FullCompiledSchemas`, emitter, compat, docs, and tests. No migration, but touches 5-7 files plus test goldens; design ambiguity about emitter mapping keeps it from being a 3.
- **Autonomy:** 3 / 6 — moderate autonomy. An agent can trace the existing encoding and implement one of the two defensible representations (preserve wrappers vs document product-field-only), but the JSON Schema drafting choice (how `Specified` vs `Option` appears in `required`/`anyOf`) and overlap with `OXY-147` (`Omit`) need a human confirm before merging.
  *Justification: repo gives strong mechanical signal (TODOs + ProductField), but the user-facing schema mapping is a product decision and OXY-147 may change the target state.*
- **Ambiguity-to-resolve:** 4 / 6 — meaningful ambiguity before start.
  *Justification: exact desired JSON Schema output for `Specified` vs `Option` vs `Nullable` (including `Specified[Option[A]]`) is not specified; whether wrappers should survive as `FullCompiledSchema` nodes or stay erased; whether `Nullable` is part of the improvement or out of scope; and whether `Omit` (OXY-147) should be included/aligned.*

## Open Questions
- What is the intended JSON Schema (draft 2020-12) rendering for each wrapper? Specifically: should `Specified[String]` (patch-optional, non-nullable) emit as plain `{"type":"string"}` with no `required` and no `anyOf null`, while `Option[String]` emits `{"anyOf":[{"type":"string"},{"type":"null"}]}` with no `required` — or is a different `anyOf`/`type:["string","null"]` form preferred?
- Should `FullCompiledSchemas` preserve `JsonOption`/`JsonNullable`/`JsonSpecified` as distinct `FullCompiledJsonSchema` wrapper nodes (so they survive outside product fields), or is the product-field-only `nullable`+`onMissing` encoding the intended long-term representation with erasure justified?
- Is `Nullable[A]` (required-but-nullable) in scope for this improvement, or should only `Option` vs `Specified` be touched? Tightening its semantics (currently `onMissing=None` → required) may be a separate breaking decision.
- Relationship to `OXY-147` (`JsonEncoder.Omit` / `@jsonOmit`): should this issue also introduce or align with an `Omit` wrapper, or should it be deferred? If both land, how do `Omit` and `Specified` compose?
- For `oxygen-json`, `SpecifiedEncoder.encodeJsonAST(WasNotSpecified)` currently returns `Json.Null` but `addToObject` suppresses it — should this instead be an `Option[Json]` / omission at the encoder level that dedicated tests lock in?
- Does `ApiSpecPage` need a visible UI distinction (e.g. `required` column + `nullable` badge) or is diagnostic `toIndentedString` output sufficient for this task?
- Are there any existing golden files or downstream consumers (MCP tool `inputSchema`, OpenAPI export) whose expected output must be updated as part of this change, and should compat treat `String` → `Specified[String]` as non-breaking (more permissive) or breaking?
