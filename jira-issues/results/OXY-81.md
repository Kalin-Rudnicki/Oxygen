# OXY-81 — Add docs for oxygen-http

## Original
- **Key:** OXY-81
- **Checklist line:** `- [ ] [OXY-81](https://kr-oxygen.atlassian.net/browse/OXY-81) — **Documentation** · High — Add docs for oxygen-http`
- **Type:** Documentation
- **Priority:** High
- **Title (verbatim):** Add docs for oxygen-http
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-81
- **Checklist section:** To Do
- **Epic filter:** PASS — `oxygen-http` maps to In Progress epics **OXY-2 `oxygen-http-server`** and **OXY-3 `oxygen-http-client`** (both `Epic · Normal · In Progress`). Issue is under active epic surface, not deferred.

## Expanded Description

**What this likely is:** Write the missing user-facing documentation for `oxygen-http` so that a developer can adopt the HTTP client and server without reading source. Today all three pages under `Oxygen HTTP` in `docs/mkdocs.yml` are 4-line placeholders (`TODO : Oxygen HTTP` / `TODO : Oxygen HTTP Client` / `TODO : Oxygen HTTP Server`), while every other top-level section (`Oxygen SQL`, `Oxygen Executable`, `Oxygen UI`) has 75–600-line overviews with quick-starts, mental-model tables, and sub-page navigation. This issue is the doc counterpart to the In Progress epics OXY-2/OXY-3 delivering the implementation.

Verified code surface that docs must cover (from `modules/http/zio/src/main/scala/oxygen/http/` + `modules/http/it-test` + `example/apps/web-server`):

- **HTTP Overview** (`docs/docs/http/index.md`) — what `oxygen-http` is, its dependency split (`oxygen-http` generic DSL vs `oxygen-http-zio` / `oxygen-http-zio-jvm` bindings over `zio-http`), and how the two sides (`Server` + `Client`) share the same schema DSL.
- **Server** (`docs/docs/http/server/index.md`) — defining an `Api`/`Endpoint` via annotations (`@path`, `@query`, `@header`, `@body`, etc. in `core/annotations.scala`, `core/generic/*`, `schema/*`), compiling it (`FullCompiledApiSpec` / `CompiledEndpoints`), serving via `Server` + `ZioHttpServer` (`Server.Config` / `ServerErrorConfig` / `Server.layer`), error handling (`ServerErrors`, `HttpDecodingFailure`, `ClientErrorHandler`/`ServerErrorConfig`), middleware (`ApiSpecEndpointMiddleware`, `McpEndpointMiddleware`), SSE (`ServerSentEvents`), and static/resource APIs (`ResourceApi`, `UIApi`).
- **Client** (`docs/docs/http/client/index.md`) — creating a typed client via `DeriveClient` / `Client.layer` / `RawClient` (`Client.Config`, `RawClient`, `ZioHttpClient`, `RequestMiddleware`/`ResponseMiddleware`, `SendRequest`), calling compiled endpoints, error decoding, and integration-test patterns from `ViaHttpSpec`/`CompiledApiSpecSpec`.

Sibling issue OXY-82 (`Documentation · High — Create video for oxygen-http`) is the video companion; OXY-29/OXY-38 (`Create compatibility spec for HTTP Schema` / `Add compatibility checking for endpoint schema`, both `Higher`) imply the docs should also explain schema compatibility guarantees, even if the spec itself is a separate task.

**Who it affects:** Every consumer of `oxygen-http` (server authors, client consumers, and library contributors). Without docs, onboarding requires reading macro-heavy DSL source and `example/apps/web-server` as the only exemplar.

**Why it matters (High priority):** Documentation is the adoption bottleneck for the two In Progress HTTP epics. `High` (vs `Low` for `OXY-113 Add docs for oxygen-sql` and `Lower` for logging/metrics tasks OXY-74–77) signals stakeholders consider HTTP docs release-blocking for the next user-facing milestone, consistent with OXY-82 video being similarly `High`.

**Inferred acceptance criteria:**

1. `docs/docs/http/index.md` replaced from `TODO` with a real overview: module coordinates/dependencies, "what you get" table (schema DSL, server, client), and a minimal end-to-end sketch (define an `Api`, implement it, serve it, call it from a derived client) paralleling `Oxygen SQL` quick-start style.
2. `docs/docs/http/server/index.md` expanded from `TODO` into a server guide: annotation catalogue, route/param/body codecs (`RequestCodec`/`ResponseCodec`/`Partial*Codec`), compiling and serving, config (`Server.Config` + `ServerErrorConfig`), error handling, middleware/MCP/resource endpoints, and a pointer to `example/apps/web-server`.
3. `docs/docs/http/client/index.md` expanded from `TODO` into a client guide: `Client` vs `RawClient` vs `ZioHttpClient`, `Client.Config` + `DeriveClient`, middleware, error handling, and a minimal typed-call example, with a note on `MultiClientSpec`-style multi-service clients.
4. Navigation already exists in `docs/mkdocs.yml` (`HTTP Overview` / `Client` / `Server`) — no nav change needed, but docs must render cleanly under `material` theme (code fences, admonitions, tables as in `docs/docs/sql/*` and `docs/docs/executable/*`).
5. No code change required — pure docs. Examples should compile conceptually against current `modules/http/zio` API; docs should not invent APIs not present in source.
6. Consistent with OXY-82: written docs land first so the video can reference them.

## Confidence
- **Rating:** 5 / 6 — strong evidence (code TODO + docs + sibling issues align)
- **Justification:**
  - `docs/docs/http/index.md`, `client/index.md`, `server/index.md` each verified as exactly 4 lines with `TODO : Oxygen HTTP*` placeholder — unambiguous signal that "Add docs for oxygen-http" means fill these three pages.
  - `docs/mkdocs.yml` nav already wires `Oxygen HTTP` → Overview/Client/Server, and sibling sections (`sql/*`, `executable/*`, `ui/*`) provide a clear style/length template to emulate.
  - `modules/http/zio/src/main/scala/oxygen/http/{api,client,core,schema,model}` plus `it-test` and `example/apps/web-server` enumerate the API surface that must be documented; no ambiguity about which module "oxygen-http" refers to (epics OXY-2/OXY-3 confirm scope).
  - Downgraded from 6/6 because no Jira body, design doc, or skipped doc test was fetched (network not used), and exact page outline / depth / code-example choices remain author judgment rather than prescribed spec.

## Required Changes

Pure documentation — no source-code migration. `Verified` = confirmed by reading file; `Inferred` = required by convention but not spelled out in placeholder.

- [ ] **`docs/docs/http/index.md` (Verified — currently `TODO : Oxygen HTTP`, 4 lines)** — Replace placeholder with HTTP overview. Suggested outline mirroring `docs/docs/sql/index.md` and `docs/docs/executable/index.md`:
  - Short intro: `oxygen-http` as typed, macro-driven HTTP layer over `zio-http`; depends on `oxygen-http` (schema DSL) + `oxygen-http-zio` (+ `-jvm` for `Server`/`ZioHttpServer`).
  - Dependency snippet (`libraryDependencies += "io.github.kalin-rudnicki" %% "oxygen-http-zio" % ...`).
  - Quick-start sketch: define `trait MyApi { @get("/hello/:name") def hello(name: String): String }`-style endpoint, `DerivedClient` usage and `Server` serving (keep minimal; link to sub-pages for detail).
  - Sections pointer: `Server` (`http/server/index.md`) / `Client` (`http/client/index.md`) / `Schema` compatibility note (pointer to OXY-29/OXY-38).
  - Mental-model table similar to SQL's: e.g. `Endpoint schema | RequestSchema/ResponseSchema | derives via annotations`, `Compiled API | FullCompiledApiSpec | macro`, `Server | Server + CompiledEndpoints | ZLayer`, `Client | Client/RawClient + DeriveClient | ZLayer`.
  - Inferred: keep `mkdocs.yml` nav unchanged; ensure `assets/` logo handling matches other sections.

- [ ] **`docs/docs/http/server/index.md` (Verified — `TODO : Oxygen HTTP Server`)** — Expand to server guide. Inferred outline from `modules/http/zio/src/main/scala/oxygen/http/{core,schema,model,api}`:
  - Defining APIs: annotations in `core/annotations.scala`, `core/generic/{ApiRepr,RouteRepr,ParamRepr}`, param/path/body codecs (`RequestCodec`, `PartialPathCodec`/`PartialParamCodec`/`PartialBodyCodec`).
  - Compiling: `schema/compiled/FullCompiledApiSpec`, `schema/partial/RequestSchemaAggregator`/`ResponseSchemaAggregator`.
  - Serving: `Server`, `.jvm/Server.scala` + `ZioHttpServer`, `Server.Config` / `ServerErrorConfig`, `CompiledEndpoints.toRoutes`, `Server.layer`.
  - Request/response model: `model/{rawRequest,rawResponse,ReceivedRequest,ServerErrors,HttpDecodingFailure,PageHtmlResponse,Redirect}`, `BodyUtil`, `ReadOnlyCachedHttpBody`, SSE, `LineStream`.
  - Middleware / extras: `api/ResourceApi` + `api/UIApi`, `server/ApiSpecEndpointMiddleware` + `McpEndpointMiddleware`, `model/ContentWithType`.
  - Minimal runnable server snippet referencing `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala` as exemplar; note config via `Server.Config` (cross-ref OXY-34 cleanup but do not block on it).
  - Testing note: pointer to `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala` pattern.

- [ ] **`docs/docs/http/client/index.md` (Verified — `TODO : Oxygen HTTP Client`)** — Expand to client guide. Inferred outline from `modules/http/zio/src/main/scala/oxygen/http/client/*`:
  - `Client` vs `RawClient` vs `ZioHttpClient`: when to use each.
  - `Client.Config` (`kind`/`path`/`logLevel` vs `baseUrl` view) + `Client.layer` variants, `RawClient.default`, `ZioHttpClient`.
  - Deriving a typed client: `DeriveClient` / `client/generic/{EndpointRepr,DerivedClientEndpointImpl}`.
  - Sending requests: `SendRequest`, `RequestMiddleware` / `ResponseMiddleware`, `ClientErrorHandler`, `ExpectedStatuses`, `HttpDecodingFailure`.
  - Minimal typed-call snippet; multi-client pattern (`MultiClientSpec`).
  - Note pending HTTPS/middleware TODOs (`Client.scala:11-12 TODO ssl/middlewares`) as known limitations, deferring to OXY-80/OXY-76 rather than documenting unimplemented surface.

- [ ] **Docs hygiene (Inferred)**
  - Ensure markdown renders with `material` theme extensions (`admonition`, `pymdownx.superfences`, etc. already enabled in `docs/mkdocs.yml`).
  - Cross-link to `docs/docs/executable/` for `@envConfig` app wiring if server example uses `CliApp`.
  - Optionally add `docs/docs/http/schema.md` or expand sub-pages if overview grows too long — but default is to keep the three existing pages per current nav (matches SQL's flat section structure of 4 pages; HTTP can stay at 3).

- [ ] **Tests / verification (Inferred — docs-only)**
  - No code tests; verify via `mkdocs serve` / `mkdocs build` (Dockerfile + `mkdocs.yml` at `docs/`), and link-check that `nav` entries resolve.
  - If repo has a `docs` CI job, ensure it still passes.

- **Verified vs. inferred:** The three `TODO` placeholders, their `mkdocs.yml` nav entries, and the existence/breadth of `modules/http/zio` source were verified by direct reads. That the docs should mirror `Oxygen SQL` / `Oxygen Executable` quick-start + mental-model + sections structure, and the specific codec/middleware/client classes to highlight, are inferred from sibling docs and code enumeration.

## Estimates & Autonomy

- **Story points:** 5 — Three pages of substantive technical writing requiring code reading to produce accurate, copy-pasteable examples, but no implementation, migration, or cross-module refactoring. Analogous to other doc tasks: larger than a `Low` 3-point single-page doc, smaller than an 8-point epic slice. `High` priority elevates urgency, not sizing.
  - Justification: `docs/docs/sql/*` totals ~631 lines across 5 pages with code fences/tables; HTTP docs will be comparable (~400–600 lines across 3 pages) plus example validation.

- **Autonomy:** 5 / 6 — largely autonomous given briefing + repo
  - Justification: Placeholders make scope obvious; strong templates exist (`sql/index.md`, `executable/index.md`, `ui/index.md`); the `modules/http` source and `example/apps/web-server` + `it-test` provide canonical examples. An agent can draft all three pages without human pairing; only style/narrative polish might benefit from a maintainer pass.

- **Ambiguity-to-resolve:** 2 / 6 — low, mostly editorial
  - Justification: Module mapping (OXY-2/OXY-3) and `mkdocs.yml` nav are settled; the only open choices are doc depth/outline and which code snippets to canonize (e.g., how much of `DeriveClient` vs `RawClient` to surface, whether to preview `OXY-34`/`OXY-35` config cleanup). No product or schema decision blocks starting; docs can note forward-looking TODOs as known gaps.

## Open Questions

- How opinionated should the overview's quick-start be — a single unified snippet vs separate server/client snippets? Recommendation: one minimal round-trip snippet in overview, with full server/client detail deferred to sub-pages (matches SQL's pattern).
- Should docs cover `schema` compatibility (OXY-29/OXY-38) in detail or just note that compatibility checking is forthcoming? Recommendation: brief "Compatibility" admonition in overview pointing to those issues, without duplicating unimplemented spec.
- Should `oxygen-http` docs include an `MCP` subsection (given `McpEndpointMiddleware`/`McpAuthServiceSpec` exist) or treat MCP as advanced? Recommendation: mention MCP as an available middleware with a link to its source, keep main narrative on REST endpoints.
- Desired code-fence style: `scala` with `oxygen.http.*` imports vs fully-qualified? Follow `docs/docs/sql/index.md` style (`import oxygen.sql.query.*` etc.) — i.e., wildcard imports, `derives` where applicable.
- Relationship to OXY-82 video: should written docs land synchronously with video script or ahead of it? Current assumption: docs first (OXY-81), video (OXY-82) references them.
