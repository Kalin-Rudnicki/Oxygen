# OXY-72 — Add schema representation for Tuple

## Original
- **Key:** OXY-72
- **Checklist line:** `- [ ] [OXY-72](https://kr-oxygen.atlassian.net/browse/OXY-72) — **Task** · High — Add schema representation for Tuple`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Add schema representation for Tuple
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-72
- **Checklist section:** To Do

## Expanded Description

**What this is:** Add `JsonSchema` (and the associated compiled/IR) support for Scala 3 `Tuple` types (`EmptyTuple`, `Tuple1[A]`, `(A, B)`, `(A, B, C)`, ... generic `Tuple`). Currently the `oxygen-json` layer already encodes/decodes tuples as JSON arrays via `JsonEncoder.TupleEncoder` / `JsonDecoder.TupleDecoder` (`modules/general/json/src/main/scala/oxygen/json/JsonEncoder.scala:231`, `JsonDecoder.scala:291`, `JsonCodec.scala:75`), but `oxygen-schema` (`modules/general/schema/src/main/scala/oxygen/schema/JsonSchema.scala`) has **no** `JsonSchema` for any `Tuple` — there is no `given tuple`, no `TupleSchema`, and no handling in the intermediate/compiled pipeline. This blocks deriving schemas for types that contain tuple fields, using tuples directly as endpoint request/response bodies, and round-tripping tuple values through the schema-compiled spec.

**Who it affects:** Any consumer of `oxygen-schema` / `oxygen-http` that wants to model heterogeneous fixed-length sequences. Typical uses: composite return types from SQL joins (`(UserRow, AddressRow)`), CLI positional tuple params (`(A, B)` in `docs/docs/executable/cli.md:54` already documents tuple CLI support), endpoint bodies that are naturally a tuple, and generic derivation that encounters a `Tuple` field.

**Why it matters (Priority High):** Without this, `derives JsonSchema` fails when a product contains a `Tuple` field (missing given), and `JsonSchema.derived` for a tuple type itself does not compile. The JSON codec layer already advertises tuple support, so the schema layer is inconsistent. Sibling issues OXY-148 (`oxygen-schema` epic, In Progress) and OXY-70 (Option/Specified differentiation) indicate the schema module is under active investment — tuple support is a foundational missing shape.

**Inferred acceptance criteria:**
1. `JsonSchema[(A, B)]` (and arities 1..N plus `EmptyTuple` and generic `Tuple`) resolves via givens, reusing `JsonEncoder.TupleEncoder` / `JsonDecoder.TupleDecoder` for codec behavior — a `Tuple` round-trips as a JSON array (`[elem0, elem1, ...]`) with per-element heterogeneous typing.
2. The schema participates in `Derivable` / `TypeTag` / `ReferenceBuilder` infrastructure (`SchemaLike.__internalReferenceOf`) so it deduplicates and shows up in compiled specs.
3. The compiled pipeline (`IntermediateRepr` → `RawCompiledJsonSchema` / `CompiledSchemaRef` → `FullCompiledSchema` → `JsonSchemaEmitter`) has a tuple representation (likely `JsonTuple` / `JsonTupleSchema` carrying `ArraySeq[CompiledSchemaRef.JsonLike]` for element schemas), with JSON Schema 2020-12 emission as `{ "type": "array", "prefixItems": [ ...elemSchemas ], "items": false }` (or `prefixItems` + `minItems`/`maxItems` for fixed length; empty tuple → `{ "type": "array", "maxItems": 0 }`).
4. Compatibility / diffing (`modules/general/schema/src/main/scala/oxygen/schema/compat/Compared.scala`) handles tuple comparison (arity and per-element compatibility).
5. At least one `JsonSchema.derived` / `derives JsonSchema` test and one `JsonSchemaEmitter` test prove the feature, plus a skipped-test or TODO that currently hints at the gap is resolved.
6. No breaking change to existing product/sum/array/map schemas; tuple encoding remains `Json.Arr`-based (matching `JsonEncoder.TupleEncoder`).

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner; some design details still inferred.

**Justification:**
- **Strong positive signal:** `oxygen-json` tuple codecs exist and are well-structured (`TupleEncoder.Append` / `TupleDecoder.Append` + `Empty` base cases, `JsonCodec.given tuple` at `JsonCodec.scala:75`), and `JsonSchema` has analogous per-shape givens (`ArraySchema`, `MapSchema`, `OptionSchema`, etc. at `JsonSchema.scala:88-91`, `260-301`) — the missing `TupleSchema` is the obvious gap. Grep across `modules/general/schema` for `Tuple` returns zero hits, confirming no existing support.
- **Compiled-layer pattern is clear:** Every `JsonSchema` shape has a corresponding `IntermediateRepr.JsonRepr` variant, a `RawCompiledJsonSchema.Repr` variant, a `CompiledSchemaRef.JsonLike` case, a `FullCompiledJsonSchema` case, and an emitter branch (`JsonSchemaEmitter.scala:49-60`, `RawCompiledSchema.scala:214-244`, `IntermediateRepr.scala:33-44`). Tuple would follow the same 5-file pattern — the design is mechanical.
- **Why not 5–6:** No code TODO, skipped test, or design doc explicitly says "add Tuple schema" — the title alone is the spec. Whether the compiled repr should be a new `JsonTuple` distinct from `JsonArray` (heterogeneous vs homogeneous), how `EmptyTuple` should be represented (zero-length tuple vs `JsonAST` vs dedicated `EmptyTuple` repr), and whether `PlainTextSchema` for tuples is in scope are all inferred. No `oxygen-schema` test file was found under `src/test` to anchor a golden example, and the Jira body was not fetchable (checklist is canonical per instructions).
- **Why not 1–2:** The module ownership (`oxygen-schema` / `modules/general/schema`) is unambiguous, the JSON semantics (tuple → JSON array) are fixed by the existing codecs, and the implementation pattern is strongly constrained by sibling shapes.

## Required Changes (only if Confidence >= 3)

**Module ownership:** `oxygen-schema` (`modules/general/schema`, cross-compiled JVM/JS/Native) is the primary owner. `oxygen-json` (`modules/general/json`) is already done (tuple codecs exist). `oxygen-http` / `oxygen-schema` compat and emitter are secondary consumers.

- [ ] **Live schema — `modules/general/schema/src/main/scala/oxygen/schema/JsonSchema.scala` (Verified — no tuple given exists; Inferred — new code)**
  - Add a `TupleSchema[A <: Tuple]` (or `TupleSchema` family) extending `JsonSchema.NonProductLike[A]` (tuples are non-product-like in this codebase — cf. `ArraySchema`, `MapSchema` at `JsonSchema.scala:260-301`). It should hold per-element `JsonSchema`s and delegate `jsonEncoder`/`jsonDecoder` to the existing `JsonEncoder.TupleEncoder[A]` / `JsonDecoder.TupleDecoder[A]` (available via `summon[JsonEncoder.TupleEncoder[A]]` / `summon[JsonDecoder.TupleDecoder[A]]`) so codec behavior stays consistent.
  - Provide givens: `given emptyTuple: JsonSchema[EmptyTuple]` and `given tupleCons: [H, T <: Tuple] => (JsonSchema[H], JsonSchema[T]) => JsonSchema[H *: T]` (recursive Mirror-style derivation). Alternatively a single `given tuple: [A <: Tuple: {JsonEncoder.TupleEncoder, JsonDecoder.TupleDecoder, TypeTag}] => JsonSchema[A]` that materializes via inline/macro tuple recursion — mirror `JsonEncoder.TupleEncoder`'s `Append`/`Empty` structure. Ensure `__internalReferenceOf` emits something like `JsonTuple(elemType0, elemType1, ...)` via `builder.referenceOf` per element.
  - Verify `TypeTag` derivation for tuples (likely `TypeTag.derived` works for `EmptyTuple`/`*:` as it does for `ArraySeq[A]`/`Map[K,V]`).
  - Consider `given tuple1: [A: JsonSchema] => JsonSchema[Tuple1[A]]` ergonomics — `Tuple1` is a distinct class in Scala 3 and may need explicit handling.

- [ ] **Intermediate repr — `modules/general/schema/src/main/scala/oxygen/schema/intermediate/IntermediateRepr.scala` (Verified — `JsonRepr` sealed trait at line 32; Inferred — new case)**
  - Add `final case class JsonTuple(elemRefs: ArraySeq[IntermediateTypeRef.Json]) extends JsonRepr` (and `JsonField`/`JsonCase` analogues not needed — just refs).
  - Extend `compileJson` (`IntermediateRepr.scala:101-209`) with a `case schema: JsonSchema.TupleSchema[?] =>` branch that iterates elements, compiles each via `compileJson(elemSchema, CompileInput(...))`, collects `gen.ref`s, and emits `JsonTuple(elemRefs)`. Handle `EmptyTuple` as `JsonTuple(ArraySeq.empty)`.

- [ ] **Compiled refs — `modules/general/schema/src/main/scala/oxygen/schema/compiled/CompiledSchemaRef.scala` (Inferred — new cases)**
  - Add `JsonTuple(elemTypes: ArraySeq[JsonLike])` (or `JsonTuple` holding `ArraySeq[CompiledSchemaRef.JsonLike]`) to the `JsonLike` hierarchy. Add corresponding `mapTypeIdentifier` handling. Decide whether `EmptyTuple` gets a dedicated `EmptyTuple` ref or is just `JsonTuple(Nil)`.

- [ ] **Raw compiled — `modules/general/schema/src/main/scala/oxygen/schema/compiled/RawCompiledSchema.scala` (Verified — `RawCompiledJsonSchema` at line 139; Inferred — new repr)**
  - Add `final case class JsonTuple(elemTypes: ArraySeq[CompiledSchemaRef.JsonLike]) extends Repr` with `mapTypeIdentifier`.
  - Extend `convertRepr` (`RawCompiledSchema.scala:205-244`) with `case IntermediateRepr.JsonTuple(elemRefs) => (typeIdentifier, Lazy(JsonTuple(elemRefs.map(resolveJson))))`.

- [ ] **Full compiled — `modules/general/schema/src/main/scala/oxygen/schema/compiled/FullCompiledSchema.scala` (Inferred — new case)**
  - Add `final case class JsonTuple(...)` to `FullCompiledJsonSchema` (likely `case class JsonTuple(ref: CompiledSchemaRef.JsonLike, elemTypes: ArraySeq[Lazy[FullCompiledJsonSchema]])` mirroring `JsonArray`/`JsonProduct` structure).
  - Wire `FullCompiledSchemas` resolution for `JsonTuple`.

- [ ] **JSON Schema emitter — `modules/general/schema/src/main/scala/oxygen/schema/compiled/JsonSchemaEmitter.scala` (Verified — `emitJson` at line 49; Inferred — new branch)**
  - Add `case s: JsonTuple => obj("type" -> Json.string("array"), "prefixItems" -> Json.Arr(s.elemTypes.map(e => emitJson(e.value))), "items" -> Json.boolean(false))` with `EmptyTuple` special-casing (`maxItems: 0` or empty `prefixItems`). This is draft 2020-12's tuple validation (heterogeneous fixed-length arrays use `prefixItems`; `items: false` forbids extra items). Alternative `items` as `false` vs omitted is a spec detail to confirm.

- [ ] **Compat / diffing — `modules/general/schema/src/main/scala/oxygen/schema/compat/Compared.scala` (Verified — TODOs for `JsonOneOf` at line 316; Inferred — new diff)**
  - Add `diffJsonTuple` handling: arity change is incompatible (or major break), per-element diff recurses. Wire into the main `diffJson` dispatch. Add `Compared` test coverage.

- [ ] **Schema compiler — `modules/general/schema/src/main/scala/oxygen/schema/compiled/SchemaCompiler.scala` / `Compiled.scala` / `SchemaType.scala` (Inferred — if tuple participates in distinct-type or depth logic, include it)**
  - Ensure `ignoreWhenComputingDistinctTypes`, `references`, and `toIndentedString` handle the new tuple cases.

- [ ] **Tests — `modules/general/schema/src/test` (Inferred — no test dir found, but pattern exists in `oxygen-json` and `oxygen-schema` spec files) + `modules/general/schema/compiled/JsonSchemaEmitterSpec` (if exists)**
  - Live schema: `JsonSchema[EmptyTuple]`, `JsonSchema[(Int, String)]`, `JsonSchema[(Int, String, Boolean)]`, `JsonSchema[Tuple1[UUID]]`, nested `((Int, String), Boolean)`, and a product containing a tuple field (`case class Foo(pair: (Int, String)) derives JsonSchema`) — all derive, encode/decode via `Json.Arr`, and produce stable `__internalReferenceOf`.
  - Compiled: `Compiled.json(JsonSchema[(Int, String)]).compiled` produces a `RawCompiledJsonSchema.JsonTuple` with two element refs; `FullCompiledSchemas` resolves; `JsonSchemaEmitter.emitStandalone` produces `prefixItems` with correct per-element schemas and `items: false`.
  - Compat: `Compared` for same-arity compatible element change vs arity mismatch.

- [ ] **Docs — `docs/docs/schema` or `docs/docs/json` (Inferred — optional)**
  - Brief note that `Tuple` ↔ JSON array with `prefixItems` fixed-length validation; example `derives JsonSchema` for `(A, B)` and `EmptyTuple`.

## Estimates & Autonomy

- **Story points:** 5 — New shape across 5–6 files in the compiled pipeline plus live givens and emitter/compat. Each file change is small and mechanical (mirrors `ArraySchema`/`JsonArray`), but tuple's heterogeneous, recursive `*: ` structure requires careful implicit derivation and testing across arities. Comparable to OXY-149 (separate `Int` JsonType) but larger than a single-field task due to cross-cutting emitter/compat and `EmptyTuple` edge case.

- **Autonomy:** 4 / 6 — An agent with this briefing + the repo can implement autonomously once the `prefixItems` vs `items` emission choice and the `EmptyTuple` vs `JsonTuple(Nil)` representation are confirmed. The pattern to follow is fully visible in `ArraySchema`/`JsonArray` and `TupleEncoder`/`TupleDecoder`. Needs human sign-off on whether `PlainTextSchema` tuple support is in scope and on the JSON Schema keyword choice (`prefixItems` + `items: false` vs `items` array).

- **Ambiguity-to-resolve:** 3 / 6 — Moderate. Core task is clear (tuple ↔ JSON array), but 3–4 design decisions block start (see Open Questions). None are product-blocking, but picking the wrong emission keyword or ref shape would require rework of emitter and compat.

## Open Questions

1. **JSON Schema keyword for heterogeneous tuples:** Should the emitter use draft 2020-12 `prefixItems` + `items: false` (fixed-length tuple), `prefixItems` + `items` as element schema (variable-length tuple-like array), or legacy `items: [ ... ]` (draft 7)? The current `JsonSchemaEmitter` already targets `2020-12` (`dialect = "https://json-schema.org/draft/2020-12/schema"` at `JsonSchemaEmitter.scala:147`), so `prefixItems` is the natural choice — confirm.
2. **EmptyTuple representation:** Is `EmptyTuple` a distinct `RawCompiledJsonSchema` case or just `JsonTuple(Nil)`? And should `JsonTuple` emit `{ "type": "array", "maxItems": 0 }` or `{ "type": "array", "prefixItems": [] }`?
3. **Arity coverage:** Is `Tuple1[A]` explicitly in scope (Scala 3's `Tuple1` is a class, not `*:`)? Should large arities (e.g. `Tuple22`) be supported, or is `EmptyTuple` + `*:` recursion sufficient (which covers arbitrary arities via `Tuple.Concat` derivation)?
4. **Nullable/optional tuple elements:** Should `(Option[Int], String)` be supported where an element's schema is nullable? The emitter's `maybeNullable` already handles product fields — confirm tuple elements should go through the same `resolveJsonConcrete` nullable handling or be emitted inline.
5. **PlainTextSchema for tuples?** Out of scope presumably — tuples only make sense as `JsonSchema` (JSON arrays). But if a tuple of `PlainTextSchema` elements were desired (e.g. for CSV), should it be explicitly excluded?
6. **Backwards compatibility / `withDefault` / `secret`:** Should `JsonSchema[Tuple]` support `.secret` and `withDefault` wrappers (like `WithDefaultSchema` / `NonProductLikeSecret` at `IntermediateRepr.scala:151-156`)? Likely yes by inheriting `NonProductLike` — confirm no special handling is needed.
7. **Sibling coordination:** Does this depend on or duplicate any planned `oxygen-schema` epic (OXY-148) roadmap for collection shapes? Should the tuple representation be aligned with OXY-70's Option/Specified work (e.g. `Specified[(A, B)]`)?

