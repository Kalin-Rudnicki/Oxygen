# OXY-32 — Create basic `/docs` html endpoint middleware

## Original
- **Key:** OXY-32
- **Checklist line:** `- [ ] [OXY-32](https://kr-oxygen.atlassian.net/browse/OXY-32) — **Task** · High — Create basic `/docs` html endpoint middleware`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Create basic `/docs` html endpoint middleware
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-32
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Add an `EndpointMiddleware` that serves a **human-readable HTML view of the compiled HTTP API spec** at `GET /docs` (configurable), parallel to the JSON spec endpoint. This is the HTML counterpart to **OXY-31** (`Create simple /docs.json endpoint middleware` — Higher) and builds directly on the infrastructure landed in `cfe88137 Added auto-docs to oxygen-http`.

Today the repo already has the full pipeline for type-safe API docs:

1. **Compiled spec** — `oxygen.http.schema.compiled` (`modules/http/zio/src/main/scala/oxygen/http/schema/compiled/model.scala`, `CompiledApiSpec.scala`) compiles every `EndpointSchema`'s live `AnySchema`s through `oxygen.schema.compiled.Compiled` into a single `RawCompiledApiSpec(apis, schemas)` bundle (endpoints grouped by `apiName`, refs into one shared `RawCompiledSchemas`). Verified in `modules/http/it-test/.../CompiledApiSpecSpec.scala`.
2. **JSON serving** — `ApiSpecEndpointMiddleware` (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/ApiSpecEndpointMiddleware.scala`) is an `EndpointMiddleware` (`AppliedEndpoints => URIO[Scope, AppliedEndpoints]`) that compiles the applied endpoints once at startup and appends a `GET /oxygen/api-spec` endpoint returning `RawCompiledApiSpec` as `application/json`. Verified: already wired in `example/apps/ui` and `example/apps/webServer`.
3. **JS UI rendering** — `ApiSpecPage` (`modules/ui/web/src/main/scala/oxygen/ui/web/apispec/ApiSpecPage.scala`) is a `RoutablePage.NoParams[RawClient]` at `/page/api-spec` that `GET`s `/oxygen/api-spec` via `RawClient`, decodes with `JsonCodec[RawCompiledApiSpec]`, wraps as `FullCompiledApiSpec` (resolved graph + `show`), and renders Swagger-style endpoint cards + schema graph with the `oxygen.ui.web` component library. Verified: compiles JVM+JS, registered in `UIMain`.

**What OXY-32 adds** is the server-side **HTML** surface. The title's three clues map directly:

- **`/docs`** — the mount path (contrast `ApiSpecEndpointMiddleware` default `/oxygen/api-spec` and OXY-31's `/docs.json`). Must be configurable with a sensible default (likely `List("docs")`). Whether it should be `/docs`, `/oxygen/docs`, or an alias that coexists with `/oxygen/api-spec` is unspecified — `/docs` is taken verbatim.
- **`html`** — the response is `text/html`, not `application/json`. The body is an HTML document rendering the compiled spec — at minimum the text of `FullCompiledApiSpec.show` (or the same endpoint/param/body/schema structure `ApiSpecPage` renders) wrapped in a valid HTML shell. The word **"basic"** signals scope: no search/filter, no collapsible cards, no TOC sidebar — just a readable, styled dump (method badges + cards + `show`-like schema section) analogous to Phase 3's MVP but server-rendered.
- **`endpoint middleware`** — same extension point as `ApiSpecEndpointMiddleware`: implements `EndpointMiddleware`, reads `AppliedEndpoints`, compiles once, appends a new `AppliedEndpoint` with a `RequestSchema` constrained to `GET /docs`. Composes via `Middlewares` / `CompiledMiddlewares` (`>>>`) and runs under `Scope` at startup. Used via `Middlewares.endpointMiddlewareFromZLayer` / `ApiSpecEndpointMiddleware.middleware`-style helper.

**Who it affects:** Any team running an `oxygen-http` server who wants browsable docs without deploying the Scala.js UI app. Becomes the default "open in browser" counterpart to the machine-readable JSON spec and the basis for later compat/docs workflows (Phase 4 spec persistence/diffing already planned in `agent-docs/http-docs/http-feature-plan.md`).

**Why it matters:** Completes the 4-phase http-docs plan's missing server-side render step (Phase 3 landed only the client-side JS page). Gives operators a zero-JS, curl-and-browser-friendly HTML docs endpoint with the same single-source-of-truth compiler, keeping docs in sync with the derived `EndpointSchema`s by construction. High priority (vs. OXY-31 Higher) suggests this is the user-facing polish that makes the JSON spec actually discoverable.

**Inferred acceptance criteria:**

1. New `EndpointMiddleware` (e.g. `DocsHtmlEndpointMiddleware` or `ApiSpecHtmlEndpointMiddleware`) in `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/` exists, compiled as `.jvm` (server-only, like `ApiSpecEndpointMiddleware`).
2. Default mount is `GET /docs` (list-of-segments `List("docs")`), configurable via `Config(path, apiName, endpointName, title, ...)` — analogous to `ApiSpecEndpointMiddleware.Config(path = List("oxygen","api-spec"), apiName = Some("oxygen"), endpointName = "apiSpec")`. Supports custom path and is narrowly scoped to that exact path+method (routing check `input.request.method == GET && fullPath == config.path`).
3. Handler compiles `endpoints.arraySeq.map(_.schema)` via `CompiledApiSpec.compile` once at middleware apply time (not per-request), renders to HTML (at least `FullCompiledApiSpec.show` or structured endpoint cards mirroring `ApiSpecPage.renderEndpoint`/`renderSchema`), sets `Content-Type: text/html; charset=utf-8`, returns `Status.Ok`. By construction the served spec excludes the docs endpoint itself (middleware reads incoming set before appending).
4. Coexists cleanly with `ApiSpecEndpointMiddleware` — ordering via `>>>` preserves both endpoints; no route collision when both are installed.
5. `Middlewares` helper provided (`DocsHtmlEndpointMiddleware.middleware`, `defaultMiddleware`) matching `ApiSpecEndpointMiddleware` pattern; wiring example updated or documented (e.g. `WebServerMain` or `Middlewares` usage).
6. At least one `it-test` mirrors `CompiledApiSpecSpec`'s middleware test: `ZIO.scoped(middleware.apply(applied))`, fabricated `GET /docs`, assert `200`, `Content-Type` contains `text/html`, body contains endpoint name + `=== Schemas ===` or equivalent.
7. No new cross-module dependencies beyond existing `oxygen-http` + `oxygen-schema` `compiled` (already cross-compiled). HTML generation is pure string/template — no third-party templating engine required for "basic" scope.

## Confidence
- **Rating:** 5 / 6 — strong evidence, one clear frontrunner; implementation details still inferred.
- **Justification:**
  - **Direct sibling + landed precedent.** OXY-31 (`/docs.json` endpoint middleware) and OXY-32 (`/docs` html endpoint middleware) are an intentional JSON/HTML pair. `ApiSpecEndpointMiddleware` at `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/ApiSpecEndpointMiddleware.scala` already proves the exact pattern — `EndpointMiddleware` that compiles via `CompiledApiSpec.compile` and appends a `GET` endpoint. `git show cfe88137` and `agent-docs/http-docs/http-feature-plan.md` document Phases 1–3 as done with this middleware at `/oxygen/api-spec`; no `/docs` or `docs.json` HTML endpoint exists yet (verified by grep in `modules/http` — zero hits for `/docs`), so OXY-32 is the next obvious increment.
  - **HTML rendering design is already in-repo.** `ApiSpecPage.scala` (456 lines) and `FullCompiledApiSpec.show` provide two ready-made render strategies — server-side `show` text or a port of `renderEndpoint`/`renderSchema` into an HTML string. The "basic" qualifier maps cleanly to the minimal `show`-in-`<pre>` or lightly-carded variant, avoiding speculation about a full JS-driven docs site.
  - **Middleware extension point is fully specified.** `EndpointMiddleware` (`EndpointMiddleware.scala:5`), `Middlewares.addEndpoints`/`makeEndpoints` (`Middlewares.scala:24,57`), `CompiledMiddlewares` (`CompiledMiddlewares.scala:6,30`), and `CompiledEndpoints.compile` (`CompiledEndpoints.scala:57`) together define one narrow way to implement this. The `>>>` composition and `Scope`-once compilation are already tested.
  - **Downgraded from 6** because no Jira body was fetched and the exact path (`/docs` vs `/oxygen/docs` vs alias for `/oxygen/api-spec`), HTML fidelity ("just text dump" vs "styled cards mirroring `ApiSpecPage`"), and whether OXY-32 should embed the spec JSON for client-side hydration are not typed anywhere. The feature plan never mentions `/docs`; the titles use `/docs.json`/`/docs` while the landed code uses `/oxygen/api-spec`, leaving the alias/default-path decision ambiguous.

## Required Changes (only if Confidence >= 3)

- **Module ownership:** `oxygen-http` (`modules/http/zio` — `.jvm` server source) is primary owner; `oxygen-http it-test` for tests; no other `oxygen-*` module needs production code changes. `docs/docs/http/` for one-paragraph middleware docs (currently stub `TODO : Oxygen HTTP Server`).

- **New file — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/DocsHtmlEndpointMiddleware.scala` (or `ApiSpecHtmlEndpointMiddleware.scala` — naming needs decision, see Open Questions) — *inferred; verify `.jvm` placement matches `ApiSpecEndpointMiddleware`:***
  - [ ] Mirror `ApiSpecEndpointMiddleware` structure: `final case class DocsHtmlEndpointMiddleware(config: Config) extends EndpointMiddleware` with `override def apply(endpoints: AppliedEndpoints): URIO[Scope, AppliedEndpoints]` that:
    - Compiles once: `val spec = CompiledApiSpec.compile(endpoints.arraySeq.map(_.schema).toSeq)` and `val full = spec.toFullCompiledApiSpec` (reuse `FullCompiledApiSpec.show` for the minimal path, or call a new `renderHtml(full): String` helper for lightly styled output). *Verified: `CompiledApiSpec.compile` and `FullCompiledApiSpec` live in `modules/http/zio/src/main/scala/oxygen/http/schema/compiled/` + `modules/http/zio/.jvm/src/main/scala/oxygen/http/schema/compiled/CompiledApiSpec.scala`.*
    - Builds HTML string: at minimum `<!doctype html><html><head>…<style>…</style></head><body><pre>escaped(show)</pre></body></html>`. For the slightly more polished "basic but styled" variant, port `ApiSpecPage`'s `renderApi`/`renderEndpoint`/`renderSchema` logic into string concatenation (method badge colors via inline CSS, pill colors from `ApiSpecPage.methodColor` — `#61affe`/`#49cc90`/etc.). No JS, no external assets — self-contained response is the "basic" bar. *Inferred: exact HTML fidelity is not specified; both extremes satisfy "basic".*
    - Escapes `show`/type names for HTML (`&`, `<`, `>`) — straightforward utility.
    - Creates `RequestSchema` constrained to `GET` + exact `config.path` segments (`NonEmptyList.one(RequestPathsSchema(config.path.map(Const(_)).toArraySeq, None))` — same pattern as `ApiSpecEndpointMiddleware:33`). *Verified shape in `ApiSpecEndpointMiddleware.scala:30-37`.*
    - Lifts `Content-Type: text/html; charset=utf-8` via `BodyUtil.fromString(html, MediaType.text.html)` plus explicit header (mirrors `ApiSpecEndpointMiddleware:53-54` which lifts `application/json`).
    - Returns `AppliedEndpoint(schema = …, handle = input => if method==GET && fullPath==config.path then Some(ZIO.succeed(Some(response))) else None)`.
  - [ ] Companion `object DocsHtmlEndpointMiddleware` with:
    - `final case class Config(path: List[String], apiName: Option[String], endpointName: String, pageTitle: String)` — defaults `path = List("docs")`, `apiName = "oxygen".some` (or `None` — needs decision), `endpointName = "docs"`, `pageTitle = "Oxygen API Docs"`. *Inferred: `ApiSpecEndpointMiddleware.Config.default` uses `List("oxygen","api-spec")`; OXY-32 title says `/docs` verbatim, so default path is `List("docs")` not `List("oxygen","docs")`.*
    - `val layer: URLayer[Config, DocsHtmlEndpointMiddleware]` and `def middleware: Middlewares[Config]` / `def defaultMiddleware: Middlewares[Any]` mirroring `ApiSpecEndpointMiddleware:67-76`.
  - [ ] Alternative implementation choice to document: instead of reimplementing render, generate a tiny shell that fetches `/oxygen/api-spec` (or `/docs.json` if OXY-31 lands there) via `fetch` and renders client-side. Rejected for "basic" scope — server-rendered HTML is simpler and requires no JS/relative-URL coordination. Note the decision in a code comment.

- **Existing files to touch — *verified; edits are additive:***
  - [ ] Optionally re-export or group the two docs middlewares — e.g. a `DocsMiddlewares` helper or `ApiSpecEndpointMiddleware` companion with `def html(...)` — so consumers can install `GET /docs.json` + `GET /docs` together with one `>>>` chain. Not required for acceptance but mirrors the JSON/HTML pair. *Inferred.*
  - [ ] If product wants aliases (`/docs` + `/oxygen/api-spec` serving JSON, `/docs.json` + `/docs` for HTML), update `ApiSpecEndpointMiddleware.Config.default` or add an alias `AppliedEndpoint` variant. Decision needed; implementation is trivial (loop over `config.paths`). *Inferred.*

- **Tests — `modules/http/it-test/src/test/scala/oxygen/http/CompiledApiSpecSpec.scala` or new `DocsHtmlEndpointMiddlewareSpec.scala` — *verified pattern exists:***
  - [ ] Extend `CompiledApiSpecSpec` or add a focused spec that: `summon[DeriveEndpoints[UserApi]].appliedEndpoints(...)` → `ZIO.scoped(middleware.apply(applied))` → fabricate `ReceivedRequest.fromRequest(Request.get(URL.decode("/docs").toOption.get))` → `specEp.handle(input)` → assert `status == 200`, header `Content-Type` contains `text/html`, body `contains "userById"` and `contains "=== Schemas ==="` (or structured card markers if styled). Cover custom-path variant and coexistence test (`ApiSpecEndpointMiddleware >>> DocsHtmlEndpointMiddleware` both present). *Verified: `CompiledApiSpecSpec.scala:56-70` is the exact template.*
  - [ ] Edge: `POST /docs` → 404/405, `GET /docs/` trailing-slash behavior, missing `FullCompiledApiSpec` (no endpoints) renders empty state gracefully.

- **Docs — `docs/docs/http/server/index.md` (currently stub `TODO : Oxygen HTTP Server` — *verified*) + `docs/docs/http/index.md`:**
  - [ ] Replace stub with a short "Serving docs" section: how to add `DocsHtmlEndpointMiddleware.defaultMiddleware` to `Middlewares` / `CompiledEndpoints.layer`, default path `/docs`, custom `Config`, relation to `ApiSpecEndpointMiddleware` (`/oxygen/api-spec` JSON vs `/docs` HTML), and that the spec is compiled once at startup and excludes the docs endpoint itself. Link to `http-feature-plan.md` Phases 1–3. *Inferred scope for "basic".*

- **Verified vs. inferred:** That `EndpointMiddleware` is `AppliedEndpoints => URIO[Scope, AppliedEndpoints]`, that `ApiSpecEndpointMiddleware` compiles on apply and appends exactly one `GET` endpoint, that `CompiledApiSpec`/`RawCompiledApiSpec`/`FullCompiledApiSpec` are the compiler + model, and that no `/docs` HTML endpoint exists were verified by reading the files above. That the new middleware should be named `DocsHtmlEndpointMiddleware`, mount at `List("docs")`, render via `FullCompiledApiSpec.show` (vs. a styled port of `ApiSpecPage`), and be `.jvm`-only are inferred from the title plus the landed pattern and remain to be confirmed.

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 3 (Fibonacci) — small feature, "basic" scope.
  - Justification: Strictly parallel to `ApiSpecEndpointMiddleware` (which was part of a larger cfe88137 diff but the middleware itself is ~65 lines). New file (~80–120 lines) + Config/layer/middleware helpers + HTML escaping/template + one it-test asserting 200 + `text/html` + body content. No schema/compiler changes, no cross-module dep graph change, no migration. If product wants the styled card variant (porting `ApiSpecPage`'s 200-line render tree into string templates) push to 5; if just `<pre>` of `show` it is a 2.

- **Autonomy:** 5 / 6 — highly autonomous; an agent can ship this with only the briefing + the repo.
  - Justification: The entire pattern is in-tree and tested: `ApiSpecEndpointMiddleware.scala` is a complete reference implementation, `CompiledApiSpec`/`FullCompiledApiSpec` are stable, and `CompiledApiSpecSpec` is a copy-paste test template. The only choices are path default, HTML fidelity, and file naming — all narrowly scoped and easily made compatible with either answer. No design review blocks start beyond confirming those defaults.

- **Ambiguity-to-resolve:** 2 / 6 — low; little must be resolved before an agent starts.
  - Justification: Title pins down extension point (`EndpointMiddleware`), format (`html`), path (`/docs`), and quality bar (`basic`). Remaining ambiguities (default path `/docs` vs `/oxygen/docs` vs alias, HTML polish level, coexistence with OXY-31 `/docs.json`) are cosmetic, do not block a working implementation, and can be resolved with 2 defaults + a follow-up polish pass.

## Open Questions

1. **Mount path & aliases:** Title says `/docs` verbatim; existing middleware is at `/oxygen/api-spec` (not `/docs.json`). Should the HTML default be `List("docs")` (`GET /docs`), `List("oxygen","docs")`, or an alias serving both? Likewise should the JSON spec gain a `/docs.json` alias so OXY-31 + OXY-32 share a common prefix? Assumption: `List("docs")` matching the title, with an easy alias addition if product prefers `/oxygen/docs`.
2. **HTML fidelity:** Does "basic" mean `FullCompiledApiSpec.show` wrapped in `<pre>` + minimal CSS (fastest, ~20 lines of HTML), or a lightly styled card layout porting `ApiSpecPage.renderEndpoint`/`renderSchema` into server-side string templates (method badges, pills, type links as anchors)? Both satisfy "basic"; the latter is more useful but adds ~100 lines. Which does the team prefer for v1?
3. **Naming:** `DocsHtmlEndpointMiddleware` (mirrors the `/docs` path) vs `ApiSpecHtmlEndpointMiddleware` (mirrors `ApiSpecEndpointMiddleware`)? Either is fine; consistency with the JSON middleware's name matters for discoverability.
4. **Client-side hydration option:** Should the HTML shell also embed the raw `RawCompiledApiSpec` JSON (e.g. `<script type="application/json" id="api-spec">…</script>`) for optional client-side interactivity (search/filter), or stay fully static? "basic" suggests static, but embedding JSON now would ease OXY-31/32 convergence.
5. **Scope vs. Scala.js UI page:** `ApiSpecPage` already renders the spec as a rich JS page at `/page/api-spec`. Is this HTML endpoint intended to replace that for API consumers, or to coexist (e.g. `/docs` is the public docs, `/page/api-spec` remains the internal showcase)? Coexistence is assumed; no code removal proposed.
6. **Config surface:** Should the middleware expose `apiName`/`endpointName` overrides (like `ApiSpecEndpointMiddleware.Config`) even though the HTML page is unauthenticated and ungrouped? Assumed yes for parity, but could omit if the HTML page is truly global.
7. **Assumption to confirm:** That OXY-31 and OXY-32 are a paired JSON/HTML pair for the same compiled spec, reusing `CompiledApiSpec.compile` — not separate specs (e.g. OpenAPI for `/docs.json` and oxygen-native for `/docs`). If they are meant to be OpenAPI vs native, scope changes materially (OpenAPI generation would be a different compiler).
