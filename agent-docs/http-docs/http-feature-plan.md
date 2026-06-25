# HTTP Docs — Feature Plan

> Status: **Phases 1–3 implemented (1 & 2 tested); Phase 4 pending**
> Modules: `modules/http/zio` (oxygen-http), `modules/general/schema` (oxygen-schema)

## Implementation status

| Phase | Status | What landed |
|---|---|---|
| 1 — Compiled HTTP spec | ✅ done | `oxygen.http.schema.compiled`: model (`RawCompiledApiSpec` & friends, `CompiledParamType`, `CompiledExpectedStatuses`) in pure cross src; `FullCompiledApiSpec` (resolved graph + `show`); `CompiledApiSpec.compile(Seq[EndpointSchema])` compiler in `.jvm`. Compiles JVM + JS. |
| 2 — Serve via middleware | ✅ done | `ApiSpecEndpointMiddleware` (`.jvm/server`): compiles applied endpoints' schemas and appends a `GET /oxygen/api-spec` endpoint serving the spec as JSON. **Wired into `example-web-server`** (`WebServerMain` → `CompiledEndpoints.endpointLayer(endpointMiddleware = new ApiSpecEndpointMiddleware())`). |
| 3 — UI rendering | ✅ done | `oxygen.ui.web.apispec.ApiSpecPage` (`RoutablePage.NoParams[RawClient]`): GETs `/oxygen/api-spec` via `RawClient`, decodes with `JsonCodec`, builds `FullCompiledApiSpec`, renders endpoints (grouped by API; method/path/params/bodies) + schema graph (products→fields, sums→cases; other variants fall back to `toIndentedString`). Registered in `example/apps/ui` (`UIMain`, route `/page/api-spec`) with a `RawClient.default` layer. Compiles JVM + JS. |
| 4 — Compat detection | ⬜ pending | Decided: persist + diff via `oxygen.schema.compat`. Not started. |

**Tests:** `modules/http/it-test/.../CompiledApiSpecSpec.scala` (5 tests, green) — compiles all
6 `UserApi` endpoints, dedupes shared `User`, JSON round-trips, `FullCompiledApiSpec.show`
renders, and the middleware serves a decodable spec over a fabricated request.

**Files added:**
- `modules/http/zio/src/main/scala/oxygen/http/schema/compiled/model.scala`
- `modules/http/zio/src/main/scala/oxygen/http/schema/compiled/FullCompiledApiSpec.scala`
- `modules/http/zio/.jvm/src/main/scala/oxygen/http/schema/compiled/CompiledApiSpec.scala`
- `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/ApiSpecEndpointMiddleware.scala`
- `modules/http/it-test/src/test/scala/oxygen/http/CompiledApiSpecSpec.scala`
- `modules/ui/web/src/main/scala/oxygen/ui/web/apispec/ApiSpecPage.scala`
- (edited) `example/apps/ui/.../UIMain.scala` — registered page + `RawClient` layer

**UI refinements (done):** aligned 3-column attribute tables (name | type | pills); color-coded
method badges; semantic presence/nullability pills; colored status badges moved up to the
SUCCESS/ERROR label; structured schemas (products/sums, incl. transform-aliases) ordered first;
`Transformed` schemas unwrapped to what they actually wrap (no nested dump); per-case error rows
link `code → CaseName` straight to the case's sub-type schema; uses `OxygenStyleSheet.Scrollable`.

**Per-case error status codes (done):** resolved the `StatusCodes` "should contain a schema of
which types return which codes" TODO. `StatusCodes[A]` now exposes `caseStatuses` (caseName →
Status), threaded `ResponseSchema.caseStatuses` → `RawCompiledResponse.caseStatuses`
(`RawCompiledCaseStatus(caseName, code)`) → UI. For sum-typed responses the viewer renders the
per-case breakdown (status pill + case name) instead of the aggregate badges. Schema layer keys
by `(String, Status)` since it can't depend on core (core depends on schema).

**Phase 3 implementation notes / decisions:**
- UI route is `/page/api-spec` (page prefix `page` + `path = Seq("api-spec")`); the *server*
  path it fetches is `/oxygen/api-spec`. Fetch resolves relative to origin (matches the example
  app's `Client.Config.relativeUrl` convention), so serve the UI same-origin as the API.
- Type references render as **clickable monospace links** (`id`/`href` anchors) that jump to the
  schema definition; recursion handled by name-only refs with full definitions in the Schemas
  section.
- **Swagger-style redesign (done):** custom scroll layout (flex column, fixed height, inner
  `overflow-y:auto` + `min-height:0`) — this fixed the original non-scrollable blocker. Color-coded
  HTTP method badges, endpoint cards, schema cards (products→fields, sums→cases), centered
  max-width column, themed via `OxygenStyleVars` (`S.*`).
- **Remaining polish (follow-ups):** sticky left table-of-contents / endpoint sidebar; search /
  filter; collapsible endpoint cards; hover states; structured rendering for non-product/sum schema
  variants (currently a `toIndentedString` `<pre>` fallback). Note: UI not visually verified in this
  environment — compiles JVM+JS; eyeball in a browser.

## Goal

Produce a **first-class, type-safe HTTP API spec** from the schema info oxygen-http
already derives per endpoint — using **our own spec format**, *not* OpenAPI.

> **Decision — no OpenAPI.** OpenAPI is rejected: it's a poor fit from a type-safe / FP
> perspective. We define our own compiled spec format, mirroring how `oxygen.schema.compiled`
> already models compiled type schemas (serializable IR + resolved graph + `compat` diffing).

### The four missing pieces

1. **A `compiled` version of an HTTP spec** — the analogue of `RawCompiledSchemas` /
   `FullCompiledSchemas`, but for endpoints/APIs (structural endpoint info + refs into a
   shared compiled-schema bundle). This is the foundation; everything else builds on it.
2. **Surface the compiled spec via an API** — a server endpoint that serves the compiled
   spec (so tooling/UI can fetch it).
3. **A UI element to render the spec** — renders the compiled schema/endpoints in the UI.
4. **API-incompatibility detection** *(eventually)* — same paradigm as the just-completed
   DB migration test: persist the compiled spec, diff in CI, fail on breaking changes.
   Reuses the existing `oxygen.schema.compat` layer.

---

## Current State (as of 2026-06-25)

### oxygen-schema — `modules/general/schema`

A schema is a **live, typeclass-style description of a type** that knows how to
encode/decode and can be *compiled* into a serializable, reference-resolved IR.

**Live schema layer** (`oxygen.schema`)
- `SchemaLike[A]` — base trait: `typeTag`, `encode`/`decode`, `transform*` combinators,
  recursive-schema detection via `schemaId`/`referenceName`.
- Two concrete families:
  - `PlainTextSchema[A]` — single-string values (path/query/header params, enums, bearer tokens, formatted/encoded text).
  - `JsonSchema[A]` — JSON values (products, sums/oneOf, AST, numbers, arrays, maps, option/nullable/specified wrappers, string-encoded).
- `AnySchema = PlainTextSchema[?] | JsonSchema[?]`, `AnySchemaT[A]` the typed union.
- `JsonSchema` derives via `Derivable`; rich set of givens for stdlib + oxygen core types.

**Compiled schema layer** (`oxygen.schema.compiled`) — *this is the spec/IR engine*
- `Compiled[A]` — a free monad describing "compile these schemas"; `.compiled` runs it
  through `SchemaCompiler`, producing `Compiled.Output(value, RawCompiledSchemas)`.
- `intermediate.*` — `IntermediateRepr`/`IntermediateTypeRef`/`IntermediateReprs`: the
  raw graph captured during compilation, before reference resolution.
- `RawCompiledSchema` (`RawCompiledPlainSchema` / `RawCompiledJsonSchema`) — flat,
  **JSON-serializable** per-type representation. Products carry fields (name, description,
  nullable, onMissing, fieldType-ref); sums carry discriminator + cases; etc.
- `CompiledSchemaRef` — the **reference** form used to point between compiled schemas
  (`PlainRef`/`JsonRef` root refs + structural wrappers: `JsonArray`, `JsonMap`,
  `JsonOption`, `JsonNullable`, `EncodedText`, `BearerToken`, …).
- `RawCompiledSchemas` — a collection of all raw schemas (the serializable bundle).
- `FullCompiledSchemas` / `FullCompiledSchema` — raw schemas with refs *resolved* into a
  navigable in-memory graph (mutable internal state to handle recursion). Has `.show`
  for an indented human-readable dump.
- `TypeIdentifier`, `SchemaType` — type-name modeling (packages/objects/generics, unions,
  intersections), independent of source line numbers (`withoutLineNos` for stable output).

**Compatibility layer** (`oxygen.schema.compat`)
- `Compared` / `ComparisonResult` / `util` — diff two compiled schemas for compatibility.
- This is the **only current consumer** of the compiled layer (plus tests:
  `CompatSpec`, `DeriveSchemaSpec`). Mirrors the avro-style "persist compiled schema,
  fail CI on drift" paradigm referenced in the sql-migration plan.

### oxygen-http — `modules/http/zio`

- Endpoints are derived (macros) into `Endpoint[Api]` carrying an **`EndpointSchema`**:
  `apiName`, `endpointName`, `requestSchema`, `successResponseSchema`,
  `errorResponseSchema`, `doc`.
- `RequestSchema` — method, path segments (`RequestPathsSchema`: consts + single/rest
  params), query params, headers, body. `ResponseSchema` — expected statuses, headers,
  body. Body/param schemas hold a live **`AnySchema`** plus `name`/`doc`/`ParamType`.
  (`ResponseSchema` is flagged `// TODO: this needs lots of additional details`.)
- Built at derivation time by `DerivedServerEndpointImpl.makeSchema` via
  `requestCodec.schemaAggregator.unsafeBuild(...)` and `ResponseCodec.unsafeBuild(...)`.
- **What the schema is used for today: essentially nothing but metric labels.**
  `EndpointSchema.metricLabels` emits `oxygen.api-name` / `oxygen.endpoint-name`.

## The gap (relationship between the two)

```
  oxygen-http                         oxygen-schema
  -----------                         -------------
  EndpointSchema                      SchemaLike / AnySchema   (live)
   ├─ RequestSchema  ──holds──▶ AnySchema ─┐
   ├─ ResponseSchema ──holds──▶ AnySchema ─┤   ??? no bridge ???
   └─ doc                                  │
                                           ▼
                                    Compiled → RawCompiledSchema(s)   (serializable IR)
                                           ▼
                                    FullCompiledSchemas (resolved graph)
                                           ▼
                                    compat.Compared   ← only consumer today
```

- oxygen-http **has** all the structural endpoint info and the live `AnySchema`s.
- oxygen-schema **has** a mature pipeline to turn an `AnySchema` into a serializable,
  reference-resolved spec IR — and to diff two such IRs.
- **Nothing connects them.** There is no OpenAPI/Swagger output, no rendered HTML/markdown
  docs, no "compile the endpoint schemas" step. `grep` for `openapi|swagger|api-doc`
  across all modules returns nothing.

## Implications for the feature

The building blocks already exist; the work is a **bridge (live → compiled HTTP spec) +
an API to serve it + a UI to render it + (later) compat diffing**. The compiled-schema
machinery (`Compiled`, `RawCompiledSchemas`, `FullCompiledSchemas`, `compat`) is reused
as-is for the *type* layer; we add the *endpoint/API* layer on top of it.

## Direction (decided)

- **Own spec format**, no OpenAPI. Model it after `oxygen.schema.compiled`, in a
  parallel package: **`oxygen.http.schema.compiled`** (compiled is a subpackage of
  `oxygen.http.schema`, just as `oxygen.schema.compiled` nests under `oxygen.schema`).
- **Two-stage, matching oxygen-schema:**
  - *Compile-time (already exists):* endpoints are macro-derived into in-memory
    `EndpointSchema`s (analogous to `JsonSchema.derived[A]` via `Derivable`).
  - *Runtime (new):* a pure-runtime compiler (no macros — mirrors `SchemaCompiler`, which
    has zero `quoted`/`inline`/`macro`) walks the in-memory `EndpointSchema`s and converts
    them to the compiled HTTP spec, threading their `AnySchema`s through `Compiled`.
- Compiled HTTP spec = structural endpoint info (method, path segments, params, headers,
  statuses, docs) where every type reference is a `CompiledSchemaRef` into a shared
  `RawCompiledSchemas` bundle. Two tiers, mirroring schema:
  - `RawCompiled{Endpoint,Api}Spec` — flat, JSON-serializable, ref-based.
  - `FullCompiled{Endpoint,Api}Spec` — refs resolved into a navigable graph.
- Generation bridges existing `EndpointSchema` (per endpoint) → compiled spec, running the
  endpoints' `AnySchema`s through `Compiled` so shared types dedupe into one bundle.

## Plan (phased)

### Phase 1 — Compiled HTTP spec (foundation)
- Define the raw + full compiled spec model for endpoints/APIs (alongside `compiled`).
- Build the bridge: `EndpointSchema` / `RequestSchema` / `ResponseSchema` → compiled spec,
  threading all `AnySchema`s through a single `Compiled`/`SchemaCompiler` run.
- JSON codecs (reuse `derives JsonCodec` style) so the spec serializes round-trip.
- Resolve `ResponseSchema`'s `// TODO: needs lots of additional details` to whatever the
  spec needs (statuses, content types, headers).

### Phase 2 — Serve the spec via an API  → **via an `EndpointMiddleware`**
- **Decided: serve it through an `EndpointMiddleware`.** Its signature is
  `AppliedEndpoints => URIO[Scope, AppliedEndpoints]`, and each `AppliedEndpoint` carries
  its full `EndpointSchema` — so a middleware can (a) read every endpoint's schema,
  (b) compile them into the compiled HTTP spec, and (c) **append a new endpoint** that
  serves it. It composes via `>>>` and runs under `Scope` (compile once at startup).
- **Layering:** the *compiler* lives in shared `oxygen.http.schema.compiled`; the
  middleware is a thin wiring layer (read schemas → compile → append endpoint(s)). Matches
  the existing TODO in `Endpoints.scala:26` ("compile schemas … put it somewhere else shared").
- The served spec naturally excludes the spec endpoint itself (middleware reads the
  incoming set, then appends) — desired behavior.
- Same shared compiler feeds Phase 4 (persist-and-diff); the middleware is just one consumer.

### Phase 3 — UI rendering  → **`oxygen.ui.web` (dogfood our own framework)**
- **Decided stack:** oxygen's own Scala.js UI framework (`oxygen.ui.web`: `Page` / `PWidget` /
  component library — `Section`, `Table`, `NavBar`, etc.). No third-party frontend.
- **Decided location:** the `oxygen-ui-web` module. No new build wiring — it **already
  depends on `oxygen-http.js`** (build.sbt:478), and `oxygen-http` / `oxygen-schema` are pure
  cross-projects, so `RawCompiledApiSpec` + the http JS client are already on the classpath.
- Shape: a `Page`/component that fetches the served `RawCompiledApiSpec` (Phase 2 endpoint)
  via the http JS client, wraps it in `FullCompiledApiSpec`, and renders endpoints +
  resolved schema graph with existing components.
- **Decisions (locked):**
  - **Fetch = A (raw + JsonCodec):** UI GETs `/oxygen/api-spec` via `oxygen.http.client.RawClient`
    and decodes with `JsonCodec[RawCompiledApiSpec]` — no new server/schema code. (Option B, a
    derived typed `OxygenApiSpecApi` client, was deferred: it would need `JsonSchema` instances
    across the whole compiled model incl. `oxygen-schema` `compiled` types.)
  - **Location = reusable** library page in `oxygen.ui.web.apispec`; the example app
    (`example/apps/ui`) registers it to demo against the live example server.
  - **MVP scope = full:** first cut renders endpoints (grouped by API) AND the resolved schema
    graph (products→fields, sums→cases), with type refs as links; recursion handled by
    rendering refs as names rather than inlining.
- **Reference template:** the existing example UI app at `example/apps/ui` (`UIMain extends
  PageApp`, pages extend `RoutablePage`, fetches via `DeriveClient.clientLayer[...]`).

### Phase 4 — API-incompatibility detection (eventually)
- Persist the compiled spec (avro/sql-migration paradigm), diff via `oxygen.schema.compat`
  extended to the endpoint/API layer, fail CI on breaking changes; env-var-gated update.

---

## Phase 1 — detailed design

### How `oxygen.schema.compiled` works (the pattern we copy)

- `Compiled[A]` is a **monad**. You build up a description by calling `Compiled.json(schema)`
  / `Compiled.plain(schema)` (each yields a `Compiled[CompiledSchemaRef.*]`) and combining
  with `map`/`flatMap`. Calling `.compiled` runs the whole thing through one
  `SchemaCompiler`, returning `Compiled.Output(value: A, schemas: RawCompiledSchemas)`.
- The accumulator is shared: **every schema touched across the whole computation lands in
  one `RawCompiledSchemas` bundle**, deduped by `TypeIdentifier`. The `value` you built
  holds only `CompiledSchemaRef`s into that bundle.
- So compiling an entire API is "for every `AnySchema` on every endpoint, emit a
  `Compiled[CompiledSchemaRef]`, assemble the structural spec around those refs, `.compiled`".

### New types — `oxygen.http.schema.compiled`

Mirror the schema layer's **two tiers**. All `Raw*` types `derives JsonCodec` (serializable);
type references are `CompiledSchemaRef` into the shared bundle.

**Raw tier (serializable, ref-based) — this is the served/persisted/diffed artifact:**

```
RawCompiledApiSpec(
  apis:    ArraySeq[RawCompiledApi],     // grouped by apiName
  schemas: RawCompiledSchemas,           // the one shared type bundle
) derives JsonCodec

RawCompiledApi(name: Option[String], doc: Option[String], endpoints: ArraySeq[RawCompiledEndpoint])

RawCompiledEndpoint(
  name: String, doc: Option[String],
  request:         RawCompiledRequest,
  successResponse: RawCompiledResponse,
  errorResponse:   RawCompiledResponse,
)

// Request and Response are intentionally near-identical — both are just "headers + body",
// with a couple of side-specific fields. Shared sub-parts: RawCompiledParam, RawCompiledBody.
RawCompiledRequest(
  method:      Option[Method],
  paths:       NonEmptyList[RawCompiledPathSegment],   // request-only: Const | SingleParam(ref) | RestParam(ref)
  queryParams: ArraySeq[RawCompiledParam],             // request-only
  headers:     ArraySeq[RawCompiledParam],             // name, doc, ParamType.Param, ref
  body:        RawCompiledBody,                        // Empty | Single(ref) | Sse(ref) | LineStream(ref)
)

RawCompiledResponse(
  headers: ArraySeq[RawCompiledParam],
  body:    RawCompiledBody,
  status:  ExpectedStatuses,                           // response-only, de-emphasized — see below
)
```

- **All refs are general `CompiledSchemaRef`** (JSON or plain) — path/query/header params are
  **not** restricted to plain-text. Every `AnySchema` goes through the same `compileAny`
  bridge regardless of where it sits.
- **Status codes are deliberately an afterthought.** Unlike tapir/OpenAPI, oxygen-http does
  *not* model behavior around HTTP status codes. What matters is that the response **body**
  decodes — error responses are discriminated JSON sum types, decoded via standard
  discriminators, not by branching on status. So `status` is just a minor descriptive field
  on the response; the spec is built around the body (and headers), exactly like the request.
- This also **resolves the `ResponseSchema` "needs more details" TODO**: the missing detail is
  *not* per-status body mappings / richer status-code machinery. A response is simply
  headers + body + a status annotation — symmetric to the request minus path/query.

**Full tier (resolved graph) — for the UI renderer + `.show`:**

```
FullCompiledApiSpec(raw: RawCompiledApiSpec) {
  val schemas: FullCompiledSchemas = raw.schemas.toFullCompiledSchemas
  // resolve endpoint refs → FullCompiledSchema via schemas.resolve(ref)
  def show: String = ...
}
```

### The compiler (pure runtime, no macros)

Lives in `oxygen.http.schema.compiled`, modeled on `SchemaCompiler`:

```
object CompiledApiSpec {
  def compile(endpoints: Seq[EndpointSchema]): RawCompiledApiSpec = {
    val program: Compiled[ArraySeq[RawCompiledApi]] = /* fold endpoints, see helper */
    val Compiled.Output(apis, schemas) = program.compiled        // or compiledWithoutLineNos
    RawCompiledApiSpec(apis, schemas)
  }

  // the one bridge from live AnySchema → ref, dispatching the union
  private def compileAny(schema: AnySchema): Compiled[CompiledSchemaRef] =
    schema match
      case p: PlainTextSchema[?] => Compiled.plain(p)   // CompiledSchemaRef.PlainLike
      case j: JsonSchema[?]      => Compiled.json(j)    // CompiledSchemaRef.JsonLike
}
```

- Input is just `Seq[EndpointSchema]` → **decouples the compiler from any live server**, so
  both the Phase 2 middleware (`AppliedEndpoints` → schemas) and the Phase 4 CI test feed it.
- `compiledWithoutLineNos` for the persisted/diffed variant (stable output), per the schema
  layer's guidance.

## Open questions

- ~~Phase 1 module placement~~ → **decided: `oxygen.http.schema.compiled`.**
- ~~Compiled spec scope~~ → **decided: compiler takes `Seq[EndpointSchema]`, server-independent.**
- ~~Plain-only params?~~ → **decided: no — all refs are general `CompiledSchemaRef`.**
- ~~`ResponseSchema` TODO~~ → **decided: response = headers + body + de-emphasized status;
  symmetric to request. No per-status-code machinery.**

- ~~Phase 3 UI stack/location~~ → **decided: `oxygen.ui.web`, in the `oxygen-ui-web` module
  (already depends on `oxygen-http.js`).**

_(All Phase 1–3 planning questions resolved. Ready to implement.)_
- Phase 4 (later): spec persisted as one bundle file, or per-API/per-endpoint files?
