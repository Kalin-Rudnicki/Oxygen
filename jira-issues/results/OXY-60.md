# OXY-60 — Support json parsing and decoding at the same time

## Original
- **Key:** OXY-60
- **Checklist line:** `- [ ] [OXY-60](https://kr-oxygen.atlassian.net/browse/OXY-60) — **Task** · Low — Support json parsing and decoding at the same time`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Support json parsing and decoding at the same time
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-60
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Refactor `modules/general/json` so that a JSON string can be parsed *and* decoded into `A` in a single pass, without materializing the intermediate `Json` AST.

Today every string decode takes two allocations/traversals:

```scala
// modules/general/json/src/main/scala/oxygen/json/JsonDecoder.scala:21-23
// TODO (KR) : implement in-line parser
final def decodeJsonString(string: String): Either[JsonError, A] =
  Json.parse(string).flatMap(decodeJsonAST)   // step 1: String -> Json, step 2: Json -> A
```

`JsonParser` (`JsonParser.scala`) is a hand-written recursive-descent parser that returns `Json` (`Str`/`Number`/`Bool`/`Arr`/`Obj`/`Null`), and each `JsonDecoder[A]` then pattern-matches that `Json`. The comment on the TODO and the issue title are verbatim the same intent: eliminate the double walk by decoding directly from the character stream.

The desired shape is a new decoding path that reads the characters and drives the `JsonDecoder` (or a new `StringJsonDecoder`) inline — e.g.:

```scala
trait JsonDecoder[A] {
  def decodeJsonString(string: String): Either[JsonError, A]            // fast inline path, default delegates to old two-step
  def decodeJsonAST(ast: Json): Either[JsonError, A]                    // retained for already-parsed ASTs
}
// or a new low-level trait the macros can derive:
trait DirectStringDecoder[A] { def decodeFromParser(p: JsonParser): Either[JsonError, A] }
```

Concrete motivations in this repo:
- **Performance:** `Json` AST allocation is pure overhead for the hot path (http body handling, config/YAML-to-JSON ingest, local-storage). Single-pass avoids intermediate `ArraySeq[Json]`/`ArraySeq[(String, Json)]` plus `BigDecimal` boxing for numbers that are immediately re-validated by `BigDecimalDecoder.narrow`.
- **Error quality:** Today a parse error is `JsonError(Cause.InvalidJson(idx, ...))` with no field path, while a decode error is `JsonError(path, Cause.InvalidType/DecodingFailed)` with no character offset. A combined path could report both — e.g. `foo.bar[2]: Invalid type at char 145` — which is the "at the same time" benefit the title implies.
- **Consistency with encode side:** `JsonEncoder` already has `encodeJsonStringCompact/Pretty` which goes `A -> Json -> String` via `encodeJsonAST` + `Json.showCompact`. A symmetric `decodeJsonString` that is truly direct would close the loop. `JsonCodec`/`StringCodec` bridges (`toStringCodec`, `fromDerivedJsonCodec`) also currently compose the two-step decode transitively.

Scope is almost certainly `modules/general/json` core — *not* `oxygen-sql` JSONB, `oxygen-http` body handling, or `oxygen-schema` — because the TODO lives in `JsonDecoder.scala` and the title says "json parsing and decoding" (the two nouns are the two methods on `JsonDecoder`). The sibling `modules/general/yaml` (`YamlParser.parseJsonOf` -> `dec.decodeJsonAST`) would benefit secondarily but is not the target.

**Who it affects:** Any caller of `JsonDecoder[A].decodeJsonString`, `String.fromJsonString[A]`, `LocalStorage.StorageKey.json`, `YamlParser.parseJsonOf`, or http-layer JSON bodies (`modules/general/json` is used transitively via `modules/http/zio` schemas). Low priority signals this is an optimization/clean-up, not a correctness blocker.

**Why it matters:** For large payloads or high-QPS services (e.g. Pulsar event payloads in `modules/events`, http server routes), the extra AST doubles allocations and GC pressure. The perf-testing epic `OXY-55` is a natural consumer — a direct parser/decoder would be measurably faster in benchmarks. It also unifies error reporting.

**Inferred acceptance criteria:**
1. `JsonDecoder[A].decodeJsonString(String)` has a direct implementation that does not require `Json.parse` to succeed first as a separate step — i.e. either a new inline parser or an optimized override per decoder that reads from the string/char-buffer directly. The default two-step path may remain as a fallback for custom decoders, but at least primitive, collection, option/nullable/specified, map, tuple, and macro-derived product/sum decoders take the fast path.
2. Behaviour parity: `decodeJsonString(s)` and `Json.parse(s).flatMap(decodeJsonAST)` agree on success value and on failure `JsonError` (allowing richer path+offset on the new path, but not silently swallowing or altering valid results). Existing `JsonDecoder`/`JsonCodec` derivation for case classes (`DeriveProductJsonDecoder`, `DeriveSumJsonDecoder`) continues to work.
3. `JsonCodec` and `JsonDecoder.derived` / `JsonDecoder.deriveWrapped` expose the same fast string path (since `StringDecoder`/`StringCodec` delegate via `toStringDecoder`/`toStringCodec`).
4. No API break: `decodeJsonAST(Json)` and `Json.parse(String)` remain available. New method is additive (default impl delegates to old logic so hand-written custom decoders are not forced to implement the new path).
5. Tests assert equivalence on representative payloads (primitives, nested objects, arrays, enums, tuples, `Option`/`Nullable`/`Specified`, flattened fields, secrets) and measure that invalid JSON still reports a useful `JsonError` (at least not worse than before).
6. Docs/`modules/general/json` README or code comment updated to note the inline path and when it applies.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Direct code signal: `modules/general/json/src/main/scala/oxygen/json/JsonDecoder.scala:21` contains `// TODO (KR) : implement in-line parser` immediately above `def decodeJsonString(string: String): Either[JsonError, A] = Json.parse(string).flatMap(decodeJsonAST)`. The phrasing "parsing and decoding at the same time" is a near-verbatim paraphrase of "in-line parser" that collapses the two steps.
  - No competing interpretation found in grep: no other `TODO` mentions simultaneous parse+decode, no skipped test or design doc suggests an http-middleware "parse+decode" helper, and `modules/general/json` is the only module whose public API exposes both `parse` (`Json.parse`) and `decode` (`JsonDecoder.decodeJsonAST`) as separate steps that users currently compose.
  - The decode path is textbook overhead (allocate full `Json` then walk again) — a standard optimization request — so the inferred intent is not speculative domain knowledge.
  - Capped at 4 not 5/6 because the exact design choice is not typed anywhere: whether the fix should be (a) a new `DirectDecoder[A]` trait, (b) an abstract `decodeFromString` with default, (c) a streaming `JsonParser` callback API, or (d) simply a convenience `JsonCodec.fromString` helper. The title alone does not pin down the API shape, and the Jira body was not retrievable.

## Required Changes (only if Confidence >= 3)

- [ ] **Core API — `modules/general/json/src/main/scala/oxygen/json/JsonDecoder.scala`**
  - Add a direct string path to the trait, e.g. `def decodeJsonStringInline(string: String): Either[JsonError, A]` or override `decodeJsonString` with a faster default, plus a hook like `def decodeFromParser(parser: JsonParser): Either[JsonError, A]` that decoders can implement. Keep the existing `decodeJsonAST` path for AST callers and as fallback. Update combinators (`Mapped`, `MappedOrFail`, `OrElse`, `WithDefault`, `MapJsonInput`, etc.) to forward the fast path when their inner decoder supports it, so composition does not regress to the slow path.
  - *Verified:* current combinators only override `decodeJsonAST`; *Inferred:* they need a parallel `decodeJsonString`/`decodeFromParser` dispatch.

- [ ] **Parser refactor — `modules/general/json/src/main/scala/oxygen/json/JsonParser.scala`**
  - Refactor the private `JsonParser` from "always builds `Json`" to a dual-mode reader that can drive a `JsonDecoder` directly. Options: (a) extract a reusable cursor (`idx`, `skipWhiteSpace`, `parseRemainingString/Number`, `parseArray/Object` loops) that a decoder can call, or (b) add `parseWith[A](decoder: DirectDecoder[A]): Either[JsonError, A]` that dispatches on `string(idx)` and calls decoder hooks. Must preserve current `def parse(string: String): Either[JsonError, Json]` (used by `YamlToJson`, tests, and `Json.parseOrJsonString`) as the AST path.
  - Handle the existing `// TODO (KR) : this is going to break if parsing a root json` in `parseRemainingNumber` while refactoring — ensure numbers at top-level and in arrays/objects share the same fast path.
  - *Verified:* parser is `private[json] final class JsonParser` with mutable `idx`/`StringBuilder`; *Inferred:* widen visibility or extract a `private[json]` cursor trait.

- [ ] **Primitive decoders — `JsonDecoder.scala` companion (`StrDecoder`, `BooleanDecoder`, `BigDecimalDecoder`, `AnyJsonDecoder`, `JsonSubtypeDecoder`)**
  - Give each a `decodeJsonString`/`decodeFromParser` override that reads directly from the char buffer: `StrDecoder` can call `parseRemainingString` without wrapping in `Json.Str`; `BigDecimalDecoder` can parse `BigDecimal` without allocating `Json.Number`; `BooleanDecoder` can match `true`/`false` literals directly. This is where the allocation win comes from.

- [ ] **Collection / wrapper decoders — `OptionDecoder`, `NullableDecoder`, `SpecifiedDecoder`, `ArraySeqDecoder`, `MapDecoder`, `OrderedMapDecoder`, `TupleDecoder`**
  - Add fast paths: `ArraySeqDecoder` loops over `[` elements and calls inner `decodeFromParser` with index tracking for error paths (`atIndex`); `MapDecoder`/`OrderedMapDecoder` reuse `parseObjectPair` loop and `JsonFieldDecoder` for keys. `Option`/`Nullable` need to handle `null` literal inline.

- [ ] **Generic derivation — `modules/general/json/src/main/scala/oxygen/json/generic/DeriveProductJsonDecoder.scala` and `DeriveSumJsonDecoder.scala`**
  - Extend the macro-generated `ObjectDecoder` to emit both `decodeJsonAST` and `decodeFromParser`/`decodeJsonObjectAST` vs `decodeFromObjectParser`. Product decoder currently generates `decodeJsonObjectAST(ast, fieldMap)` by iterating cached field decoders; the fast path would generate a branch that reads object keys sequentially via the parser and dispatches to per-field decoders (respecting `@jsonField`, `@jsonFlatten`, `@jsonStrict`, default values via `withDefault`/`onMissingFromObject`). Sum decoder (`DeriveSumJsonDecoder`) needs analogous handling for discriminator keys.
  - *Verified:* derivation uses `ProductGeneric.cacheVals[JsonDecoder]` and `DeriveProductJsonDecoder` with `withCustomDisjointInstances`; *Inferred:* second derived method body required.

- [ ] **Codec / bridge — `modules/general/json/src/main/scala/oxygen/json/JsonCodec.scala`, `modules/general/json/src/main/scala/oxygen/json/extensions.scala`**
  - Ensure `JsonCodec.derived`, `JsonCodec.deriveWrapped`, and `StringCodec.usingJsonCodec`/`StringDecoder.usingJsonDecoder` forward the fast path so `String.fromJsonString[A]` and http/schema bridges benefit automatically. No new public type needed — reuse `JsonCodec.encoder`/`decoder` pair.

- [ ] **JSON AST retain — `modules/general/json/src/main/scala/oxygen/json/Json.scala`**
  - No change expected except ensuring `Json.parse` stays stable. Document that `JsonDecoder.decodeJsonString` is now the preferred entry point over `Json.parse(...).flatMap(_.fromJsonAST)`.

- [ ] **Tests — `modules/general/json/src/test` (currently absent) or `modules/tests` / new `JsonDecoderInlineSpec`**
  - Add equivalence tests: for a table of JSON strings (valid + invalid), assert `decoder.decodeJsonString(s) == Json.parse(s).flatMap(decoder.decodeJsonAST)` for primitive, collection, product, sum, flattened, secret, and tuple cases. Include error-path tests (missing required, invalid type, invalid json at idx, extra keys in strict mode).
  - Add a simple allocation/throughput micro-benchmark (ties to `OXY-55` perf framework if available) that shows the fast path avoids `Json` allocation for a ~1KB nested object.

- [ ] **Docs — `modules/general/json/README.md` or inline scaladoc on `JsonDecoder`**
  - Note the new fast path, when it applies (derived vs. hand-written decoders), and that `Json.parse` is still the way to obtain a `Json` AST when the untyped tree is needed.

- [ ] **Out of scope / follow-ups**
  - Rewriting `JsonEncoder` to a symmetric "encode directly to string without `Json` AST" is a natural follow-up but not required by this issue's wording (which only mentions parsing+decoding). Could be noted as a future `encodeJsonString` optimization.
  - Streaming / async input (`InputStream`, `fs2.Stream[Char]`) or `zio-json` interop — defer.

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 5
  - Justification: Touches the core JSON trait + parser + ~8 decoder cases + 2 macro derivation files + codec bridges. Each change is small individually, but correctness requires parity testing and careful error-path merging (`JsonError.Path` + `InvalidJson idx`). Fits a single focused PR in one module; 8 if a full zero-copy `CharBuffer`/`ByteBuffer` streaming API plus benchmark harness is required instead of a minimal inline override.
  - Comparable: larger than `OXY-53` (Lens, 2 pts) but smaller than an Epic; similar in surface to `OXY-6` (array+unnest, 5 pts) which also spans DSL + model + generation.

- **Autonomy:** 3 / 6 — moderately autonomous, design choice needed first
  - Justification: The mechanics are well-scoped (the two-step path is isolated to `JsonDecoder`+`JsonParser`), so an agent can implement an inline decoder once the API shape is chosen. But the shape itself (new trait vs. default-method override vs. parser callback) and the error-merging strategy (how to combine `InvalidJson(idx)` with `Path`) need a 10-minute maintainer decision to avoid rework.

- **Ambiguity-to-resolve before start:** 3 / 6 — moderate, blocks API shape
  - Justification: Three open questions (see below) about trait design and whether wrapped/secret/flattened sum decoders must also fast-path before the issue is "done." Code signal does not disambiguate; a brief design review unblocks the rest.

## Open Questions

1. **API shape for the fast path:** Should the trait gain `def decodeJsonString(string: String)` as the new primary with a default that delegates to `Json.parse`, plus an internal `def decodeFromParser(p: JsonParser)` hook, or should a new `DirectJsonDecoder[A]` super-trait be introduced so old hand-written decoders are not forced to implement it? The TODO suggests the former (inline parser *inside* `JsonDecoder`).
2. **Error model:** When inline parsing fails, should the error carry both the character offset (`InvalidJson idx`) and the decoded field path (`inField`/`atIndex`), e.g. `foo.bar[2] at char 145 : Invalid type ...`? Or is it acceptable to keep the current split errors (parse errors lose path, decode errors lose offset)?
3. **Coverage threshold:** Must *every* decoder have a fast path before this issue is considered done, or is "primitives + collections + derived products/sums" sufficient with a fallback for custom/combinator decoders (`MapJsonInput`, `MappedOrFail`, `OrElse`)? The fallback would still be correct but slower.
4. **Encoder symmetry:** Should `JsonEncoder.encodeJsonStringCompact` get a symmetric "encode directly to `StringBuilder` without `Json` AST" optimization in the same PR, or tracked separately?
5. **Scope vs. http/schema:** Is `modules/general/json` the only target, or should `modules/general/yaml/YamlToJson` and http body helpers (`ReadOnlyCachedHttpBody.asJsonFromCodec`) also be updated to prefer the new `decodeJsonString` path?
6. **Performance validation:** Does this issue require a benchmark result (e.g. via `OXY-55` perf framework) proving allocation reduction, or is functional parity sufficient for acceptance?

