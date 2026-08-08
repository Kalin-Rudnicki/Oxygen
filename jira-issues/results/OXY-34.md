# OXY-34 — Clean up server configuration paradigm

## Original
- **Key:** OXY-34
- **Checklist line:** `- [ ] [OXY-34](https://kr-oxygen.atlassian.net/browse/OXY-34) — **Task** · Normal — Clean up server configuration paradigm`
- **Type:** Task
- **Priority:** Normal
- **Title (verbatim):** Clean up server configuration paradigm
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-34
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Refactor and unify the HTTP server's configuration story, which today is fragmented across three layers with inconsistent patterns.

Current state (verified by reading `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala`, `ZioHttpServer.scala`, `ServerErrorConfig.scala`, `CompiledEndpoints.scala`, and sibling `Client.scala`):

1. **`Server.Config` is anemic.** It contains exactly one field — `errorConfig: ServerErrorConfig` (`exposeInternalErrors` + fine-grained `includeTraces`/`includeDefectsOnFailure` flags). Port/host/TLS/bind settings are NOT in it. It is provided as `ZLayer.succeed(Server.Config(...))` → `Server.Config` env.

2. **Transport config lives in zio-http types, provided separately.** `ZioHttpServer`'s constructor takes `zioConfig: zio.http.Server.Config` and `nettyConfig: NettyConfig` directly, wired via `Server.layer.customized: URLayer[ZServer.Config & NettyConfig, Server]` → `Server.layer.live: URLayer[ZServer.Config, Server]` (which injects `NettyConfig.default`). The caller must assemble these parallel config layers outside oxygen's abstraction (see `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala:48-83`: `WebServerMain.Config.Http` holds `errors: ServerErrorConfig` + `port: Int`, then does `ZLayer.succeed(Server.Config(errors))` + `Server.layer.simple(port)` as two separate steps, duplicating the port that already exists on `ZServer.Config`).

3. **Middleware/config sprawl.** Each middleware/API exposes its own `Config` case class with inconsistent defaults/layer helpers: `ApiSpecEndpointMiddleware.Config`, `McpEndpointMiddleware.Config`, `UserInfoService.Config`, `FileSystemResourceApi.Config`, `LiveUIApi.Config` — no shared pattern for `default`, `defaultLayer`, or `JsonCodec`-derivation for use with `oxygen-executable`'s `@envConfig`.

4. **No unified derivation or validation.** The example app's `WebServerMain.Config` derives `JsonCodec` (for `@envConfig("APP_CONFIG")` via `oxygen-executable` / `oxygen-cli` + `oxygen-yaml`), but the http module itself does not prescribe how server config should be derived from env/files. Contrast with `DbConfig` style or the `docs/docs/executable/` pattern (`@envConfig("APP_CONFIG") cfg: ClientConfig`).

The phrase "Clean up ... paradigm" (shared verbatim with sibling OXY-35 "Clean up client configuration paradigm" · Normal) indicates the intent is not to add a single feature but to establish a coherent, documented convention for how server config is modeled, assembled, and provided — making it consistent with the client-side cleanup, ergonomic via `oxygen-executable`, and not leaking zio-http internals.

**Who it affects:** Service authors wiring `oxygen-http` server (every consumer of `Server`, `CompiledEndpoints`, and middlewares). Current workaround is the manual two-layer assembly in `WebServerMain` plus direct knowledge of `ZServer.Config`; the cleanup would simplify onboarding and reduce copy-paste across services.

**Why it matters (Normal priority):** Normal (not High/Low) suggests tech-debt / DX friction, not a production bug. As `oxygen-http` matures (Epics OXY-2 In Progress), inconsistent config is a drag on new services and on forthcoming tasks (TLS / HTTPS `OXY-79`, `OXY-80`, logging `OXY-74`, metrics `OXY-75`, docs `OXY-81`). Cleaning the paradigm now prevents each of those from adding ad-hoc config in different shapes.

**Inferred acceptance criteria:**

1. `Server.Config` encompasses the full runtime server surface (at minimum: `port`/`host`, `errorConfig`, and passthrough for relevant `ZServer.Config`/`NettyConfig` fields or a nested `zioHttp: ZServer.Config`-like structure), so callers provide a single `Server.Config` layer rather than three parallel layers. `ServerErrorConfig` remains nested, not flattened.
2. Layer helpers are consistent and minimal: e.g. `Server.Config.default: Server.Config`, `Server.Config.defaultLayer`, `Server.layer.live` (or `Server.live`) takes `Server.Config` alone (internally deriving `ZServer.Config`/`NettyConfig`), with an escape hatch `Server.layer.customized` for advanced users who need raw `ZServer.Config`. Existing `Server.layer.simple(port)` is either deprecated or unified into the new `Server.Config`.
3. Config types derive appropriate codecs/schemas (`JsonCodec`/`JsonSchema` or ZIO Config) so they work out-of-the-box with `@envConfig` / `oxygen-executable` (mirroring `WebServerMain.Config.Http` pattern in `docs/docs/executable/`).
4. Documentation: brief note in `docs/docs/http/server/index.md` (currently just `TODO : Oxygen HTTP Server`) or `agent-docs/` explaining the new paradigm, migration for existing services (`WebServerMain` updated to use the new single-config form), and relationship to OXY-35 (client parity).
5. No behavior change to request handling, error mapping, or routing — purely config/assembly refactoring with backwards compat either preserved (deprecated overloads) or clearly migration-noted.
6. Sibling OXY-35 alignment: server and client config derive from the same convention (naming, `Config` location, `default`/`layer` helpers, `@envConfig` usage) so the two "Clean up ... paradigm" tasks produce a uniform story.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title is 5 words ("Clean up server configuration paradigm") with no Jira body fetched and no `TODO`/`FIXME` comment in `modules/http` mentioning "paradigm" or "clean up" — so interpretation must be inferred from code structure rather than explicit spec.
  - Code signal is moderate-strong for *what* is messy: `Server.Config`'s single-field shape (`errorConfig` only) vs `ZServer.Config` + `NettyConfig` provided via separate ZLayers in `Server.scala:48-60` and example `WebServerMain.scala:79-82` shows two parallel config paradigms (`ZLayer.succeed(Server.Config(...))` + `Server.layer.simple(port)` duplicating `ZServer.Config.default.port(port)`). The sibling OXY-35 title mirrors this for client, reinforcing that both are intentional paradigm consolidations.
  - Downgraded from 4/6 because the title gives zero direction on target shape: whether the fix is (a) folding `ZServer.Config`/`NettyConfig` into `oxygen.http.server.Server.Config`, (b) introducing a `ServerConfig` ADT with `HttpConfig` + `TlsConfig` + `ErrorConfig` sub-configs, (c) adopting ZIO Config / `zconfig` vs keeping `JsonCodec`+`@envConfig`, (d) adding host/TLS/timeout fields, or (e) purely renaming/reorganizing layers without new fields. Any of these satisfies "Clean up paradigm" but implies different scope. Exact acceptance criteria remain inferred.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **Design decision — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala` (Verified — current split, Inferred — target shape)**
  - Decide whether `Server.Config` should own `port`/`host` (and ideally `NettyConfig` relevant fields) or wrap `ZServer.Config` as a nested field (`zioHttp: ZServer.Config`). Recommendation: enrich `Server.Config` to at least `Config(port: Int, host: Option[String], errorConfig: ServerErrorConfig, /* optional: netty: NettyConfig */)` so the example's `WebServerMain.Config.Http` can collapse to a single field (`http: Server.Config` or `http: HttpConfig` that maps directly). Alternatively, keep `Server.Config(errorConfig, zServerConfig)` where `zServerConfig: ZServer.Config` nests the zio-http config — preserves advanced zio-http tuning without re-modeling every field. Document the choice and align with OXY-35's client shape (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:18-47` currently has `Config(kind, path, logLevel)` — decide parity: `port` as `Int` vs `URL` vs `ZServer.Config`).
  - Preserve `ServerErrorConfig` (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/ServerErrorConfig.scala:8-14`) unchanged internally; just ensure its derived codecs (`derives JsonCodec`) continue to work under the new nesting. No change to `includeTraces`/`includeDefectsOnFailure` semantics.

- [ ] **Layer plumbing — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala:46-90` + `ZioHttpServer.scala` (Verified)**
  - Refactor `Server.layer` so there is a primary path `URLayer[Server.Config, Server]` (or `RLayer[Server.Config, Server]`) that internally derives `ZServer.Config` and `NettyConfig`. Today's `customized: URLayer[ZServer.Config & NettyConfig, Server]` and `live: URLayer[ZServer.Config, Server]` remain as escape hatches, but `default`/`simple(port)` should delegate to the new `Server.Config` overload rather than constructing `ZServer.Config` externally.
  - Update `ZioHttpServer` (`ZioHttpServer.scala:7-10`) to accept either the enriched `Server.Config` or to keep `(zioConfig, nettyConfig)` but have its `companion.fromServerConfig` translate `Server.Config` → `(ZServer.Config, NettyConfig)`. Avoid leaking `NettyConfig` to users who don't need it — default it internally.
  - Adjust `CompiledEndpoints.toRoutes` (`CompiledEndpoints.scala:31`) signature if needed — today it takes `Server.Config` solely for `errorConfig`; if `Server.Config` gains port/host fields, keep `toRoutes` focused on `errorConfig` or accept `ServerErrorConfig` directly to avoid threading irrelevant fields.

- [ ] **Middleware/config consistency — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/ApiSpecEndpointMiddleware.scala:78-92`, `mcp/McpEndpointMiddleware.scala:132-142`, `mcp/UserInfoService.scala:23`, `api/FileSystemResourceApi.scala:24-27`, `api/LiveUIApi.scala` (Verified — inconsistent `Config` helpers)**
  - Normalize `Config` companion patterns: each middleware's `Config` should expose `default: Config`, `defaultLayer: ULayer[Config]` (or `fromServerConfig` derivation) and, where applicable, `derives JsonCodec`/`derives JsonSchema` so they compose with `oxygen-executable`'s `@envConfig`. The exact consistency target is inferred — may mean a shared `object Config` trait or just convention — but auditing these 5+ `Config` case classes and fixing naming/default inconsistencies is in scope.
  - Ensure no middleware `Config` embeds `ZServer.Config` — all surface configs should be oxygen types.

- [ ] **Executable integration + example — `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala:36-83` + `docs/docs/executable/index.md` / `cli.md` / `migration-from-v1.md` (Verified — example shows current fragmented usage)**
  - Simplify `WebServerMain.Config.Http` from `(errors: ServerErrorConfig, port: Int)` to use the new `Server.Config` directly (e.g., `http: Server.Config` or `http: oxygen.http.server.Server.Config`). Update `WebServerMain.Env.layer` (`WebServerMain.scala:73-88`) from the current two lines (`ZLayer.succeed(Server.Config(...))` + `Server.layer.simple(port)`) to a single provision (e.g., `ZLayer.succeed(config.http) >>> Server.layer.live >>> CompiledEndpoints.layer >>> Server.layer.serving` or `ZLayer.succeed(config)` with `@envConfig`). Keep `WebServerMain` as the migration exemplar.
  - Verify the result works with `CliApp` + `@envConfig("APP_CONFIG")` (`docs/docs/executable/index.md:224-236`, `migration-from-v1.md:47`) — `Server.Config` (and nested `ServerErrorConfig`) must derive `JsonCodec`/`JsonSchema` so the app can be configured via env var or fallback JSON file without custom decoders.

- [ ] **Docs — `docs/docs/http/server/index.md` (Verified — currently `TODO`) + `docs/docs/http/index.md` (Inferred)**
  - Replace the placeholder with the new paradigm summary: what `Server.Config` contains, how to provide it (`ZLayer.succeed(Config(...))` → `Server.layer.live` vs `@envConfig` in a `CliApp`), and the relationship to client config (OXY-35). Brief migration note for services currently using `Server.layer.simple(port)` + `ZServer.Config`.
  - Optionally add `agent-docs/server-config-paradigm.md` (research-parity spike style per other tasks) if the impl needs a decision record, but primary deliverable is the code/docs.

- [ ] **Tests — `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala:20-27` + `CompiledApiSpecSpec.scala`, `McpServerSpec.scala` (Verified — patterns exist, Inferred — new coverage)**
  - Existing `ViaHttpSpec` must still pass (it currently does `Server.Config.defaultLayer` + `Server.layer.serving`). Add or update a test that provisions `Server.Config` with a custom `port`/`errorConfig` and asserts the server binds correctly and error exposure respects `errorConfig.exposeInternalErrors` (e.g., `ApiSpecEndpointMiddleware` serves spec only when port is configured).
  - No data-model / schema / migration changes — purely additive config refactor; no backwards-compat DB concern.

- **Verified vs. inferred:** The fragmentation of `Server.Config` (single field) + `ZServer.Config`/`NettyConfig` as parallel layers and the example's two-step assembly were verified by reading `Server.scala`, `ZioHttpServer.scala`, and `WebServerMain.scala`. That "Clean up paradigm" means unifying these into a single `Server.Config` with consistent `default`/`layer` helpers and `@envConfig` compatibility — and that docs `TODO` and sibling OXY-35 alignment are in scope — are inferred from the title and checklist sibling structure.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — lean is 2 if only `Server.Config` + `Server.layer` + example/doc updates; 3 if middleware `Config` normalization + `JsonCodec` derivations + migration note are included; 5 only if TLS/host/NettyConfig full modeling and parallel OXY-35 parity negotiation expand the scope
  - Justification: Touches small, well-isolated code (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala`, `ZioHttpServer.scala`, `CompiledEndpoints.scala`, `ServerErrorConfig.scala`, `example/.../WebServerMain.scala`, plus 3–4 middleware `Config` companions and 1 doc file). No new module, no migration/schema, no runtime performance branch — purely type/layer refactoring with a doc update.

- **Autonomy:** 3 / 6 — needs product/design choice before coding
  - Justification: Mechanics are mechanical once the target config shape is fixed (an agent can implement the `Server.Config` enrichment and `Server.layer` refactor autonomously), but the core decision — what `Server.Config` should contain (just `errorConfig+port`, or `errorConfig+ZServer.Config`, or `errorConfig+host+port+tls+netty`) and whether to adopt `JsonCodec`/`@envConfig` vs ZIO Config — is not encoded in the title. Choosing wrong risks rework; a 15-minute human decision on the shape (and confirming OXY-35 parity) would raise autonomy to 5/6.

- **Ambiguity-to-resolve:** 4 / 6 — notable open questions block start
  - Justification: Title is 5 words with no body and no codebase `TODO` pinning the intended paradigm. Four blocking design choices below must be resolved or assumed; the implementation cannot be reviewed without agreeing on them. Lightweight clarification (one paragraph confirming the target `Server.Config` shape and ZIO Config vs `JsonCodec` choice) would drop this to 1–2.

## Open Questions

1. **Target config shape:** Should `Server.Config` become `Config(port: Int, host: Option[String], errorConfig: ServerErrorConfig)` (explicit, typed), or `Config(errorConfig: ServerErrorConfig, zServerConfig: ZServer.Config)` (wrapper preserving zio-http's full surface), or `Config(errorConfig, host, port, tls, netty)` (full modeling)? Should `NettyConfig` be included or defaulted internally? This determines `WebServerMain.Config.Http` migration and backwards-compat story.
2. **Derivation strategy:** Should `Server.Config` derive `JsonCodec`/`JsonSchema` (current `oxygen-executable` `@envConfig` pattern per `docs/docs/executable/` and `WebServerMain.Config`) or adopt ZIO Config (`ConfigProvider`, `DeriveConfig`) or config4s? The choice must align with OXY-35's client plan so server/client don't diverge.
3. **Layer API:** Should the new primary layer be `Server.layer.live: URLayer[Server.Config, Server]` (taking the enriched config) with `Server.layer.customized: URLayer[ZServer.Config & NettyConfig, Server]` kept as escape hatch, or should `Server.layer.live` continue to accept `ZServer.Config` and the `Server.Config` translation happen in `WebServerMain`? Which overloads are deprecated vs removed?
4. **Backwards compatibility:** Must existing `Server.layer.simple(port)` / `Server(Config(errors))` + `Server.layer.simple` remain (deprecated), or is a breaking migration acceptable? Are downstream services expected to update `WebServerMain`-style call sites in the same PR or via a migration guide?
5. **Middleware `Config` scope:** Does "paradigm" include normalizing all middleware `Config`s (`ApiSpecEndpointMiddleware.Config`, `McpEndpointMiddleware.Config`, `UserInfoService.Config`, etc.) for consistent `default`/`defaultLayer`/`JsonCodec` — or only the core `Server.Config`? Should `oxygen-http` prescribe a shared convention for these (e.g., `derives JsonSchema` + `@envConfig` nested under `http:`)?
6. **Sibling OXY-35 coordination:** Server and client paradigms are to be cleaned in parallel — should they be unified into a single `oxygen-http` config ADT (e.g., `HttpConfig(server: Server.Config, client: Client.Config)`) or kept independent (`Server.Config` vs `Client.Config` with matching conventions)? Sequencing with OXY-79/`OXY-80` TLS tasks — should TLS config be included now or left for those issues?
7. **Assumption to confirm:** That "server" refers to the `oxygen-http` server (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala` — Epic OXY-2 In Progress) and not to the `oxygen-executable` `LiveUIApi`/`FileSystemResourceApi` server-side helpers or the `oxygen-sql` test container infra. Confirmed by module proximity, but worth explicit sign-off.
