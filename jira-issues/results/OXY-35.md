# OXY-35 — Clean up client configuration paradigm

## Original
- **Key:** OXY-35
- **Checklist line:** `- [ ] [OXY-35](https://kr-oxygen.atlassian.net/browse/OXY-35) — **Task** · Normal — Clean up client configuration paradigm`
- **Type:** Task
- **Priority:** Normal
- **Title (verbatim):** Clean up client configuration paradigm
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-35
- **Checklist section:** To Do

## Expanded Description

**What this likely is:** Refactor and unify the HTTP client's configuration story, which today is fragmented across two layers with inconsistent patterns and leaks zio-http internals.

Current state (verified by reading `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala`, `RawClient.scala`, `RawClientPlatformSpecific.scala`, `ZioHttpClient.scala`, `modules/http/zio/.jvm/src/main/scala/oxygen/http/client/RawClientPlatformSpecificImpl.scala`, `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala`, `MultiClientSpec.scala`, and `example/apps/ui/src/main/scala/oxygen/example/ui/UIMain.scala`):

1. **`Client.Config` is low-level and leaky.** It stores `kind: URL.Location`, `path: Path`, and `logLevel: LogLevel` — raw zio-http URL pieces rather than a user-facing `baseUrl: String` or `URL`. The `URL.Location` ADT (Absolute vs Relative) and `Path` are zio-http vocabulary. There is no timeout, retry, TLS/SSL, or header-default surface. The `Config` has no `derives JsonCodec` / `JsonSchema` and no `default` value — it must be constructed via `Config.layer(urlString)` which parses the URL at layer-creation time with query/fragment guards.

2. **Transport config lives in a separate layer.** `RawClient` wraps `zio.http.Client` and is provided via `RawClient.default` which internally does `ZLayer.succeed(ZClient.Config.default) >>> requiringConfig` (where `requiringConfig` delegates to `zio.http.ZClient.live` with `NettyConfig.defaultWithFastShutdown` + `DnsResolver.default` on JVM). The caller must assemble *both* `RawClient.default` and `Client.Config.layer(url)` before `Client.layer.live` / `Client.layer.default` / `Client.layer.localPort`. Example call sites show the fragmentation: `ViaHttpSpec` does `Client.layer.localPort` (which itself builds `Config.layer("http://localhost:$port") >>> default`), `MultiClientSpec` does `Client.Config.layer("https://service-a.com") >>> Client.layer.live.project(...)` with a shared `RawClient.default`, and `UIMain` does `ZLayer.succeed(Client.Config.relativeUrl) >>> Client.layer.default` alongside a separate `RawClient.default` used directly.

3. **Two parallel client abstractions.** `RawClient` (thin wrapper over `zio.http.Client`) and `Client` (oxygen's `ZioHttpClient` with `config >> rawClient` URL-prefixing via `def >>(client: RawClient)`) are provided as distinct layers. Users who need raw access (e.g., `ApiSpecPage` fetching `/oxygen/api-spec`) take `RawClient` directly, bypassing `Client`. No guidance on when to use which or how their configs relate (`ZClient.Config` vs `Client.Config`).

4. **Incomplete / TODO surface.** `Client.scala:11-12` has `// TODO (KR) : accept middlewares when creating a client / : ssl` — indicating intended middleware and SSL config that never landed. Related tasks `OXY-80` (easy HTTPS config to client), `OXY-76`/`OXY-77` (logging/metrics to http-client) likely depend on this cleanup.

5. **No unified derivation or validation.** Unlike `WebServerMain.Config` which derives `JsonCodec` for `@envConfig("APP_CONFIG")` via `oxygen-executable`/`oxygen-cli`+`oxygen-yaml`, the http client module prescribes no way to derive client config from env/files. Each consumer rolls its own (e.g., `UIMain` hardcodes `relativeUrl`, `ViaHttpSpec` hardcodes `localPort`).

The phrase "Clean up ... paradigm" (shared verbatim with sibling `OXY-34` "Clean up server configuration paradigm" · Normal) indicates the intent is not to add a single feature but to establish a coherent, documented convention for how client config is modeled, assembled, and provided — making it consistent with the server-side cleanup, ergonomic via `oxygen-executable`, and not leaking zio-http internals.

**Who it affects:** Service/UI authors wiring `oxygen-http` clients (every consumer of `Client`, `RawClient`, and `Client.Config`). Current workaround is the manual multi-layer assembly plus direct knowledge of `URL.Location`/`ZClient.Config`; the cleanup would simplify onboarding, enable multi-client patterns (as in `MultiClientSpec`), and reduce copy-paste across services.

**Why it matters (Normal priority):** Normal (not High/Low) suggests tech-debt / DX friction, not a production bug. As `oxygen-http` matures (Epic `OXY-3` In Progress for `oxygen-http-client`), inconsistent client config is a drag on new consumers and on forthcoming tasks (HTTPS `OXY-80`, logging `OXY-76`, metrics `OXY-77`, `oxygen-ui` web client `OXY-83`). Cleaning the paradigm now prevents each of those from adding ad-hoc config in different shapes.

**Inferred acceptance criteria:**

1. `Client.Config` (or a new `HttpClientConfig`) presents a user-facing shape — at minimum `baseUrl: String` or `URL` plus `logLevel`, with optional `timeout`/`ssl`/`headers` passthrough or a nested `zClient: ZClient.Config` for advanced tuning — so callers provide a single config rather than `ZClient.Config` + `Client.Config` + `RawClient` separately. `URL.Location`/`Path` decomposition is internal, not exposed.
2. Layer helpers are consistent and minimal: e.g., `Client.Config.default` / `Client.Config.fromUrl(String)` / `Client.Config.layer` and `Client.layer.live: URLayer[Client.Config & RawClient, Client]` simplified to `URLayer[Client.Config, Client]` that internally derives `RawClient`/`ZClient.Config`, with `RawClient` kept as escape hatch. Existing `Client.layer.localPort` / `Client.Config.relativeUrl` either deprecated or unified.
3. Config types derive appropriate codecs/schemas (`JsonCodec`/`JsonSchema` or ZIO Config) so they work out-of-the-box with `@envConfig` / `oxygen-executable` (mirroring `WebServerMain.Config` pattern in `docs/docs/executable/`).
4. Documentation: brief note in `docs/docs/http/client/index.md` (currently `TODO` or missing) or `agent-docs/` explaining the new paradigm, migration for existing call sites (`ViaHttpSpec`, `MultiClientSpec`, `UIMain`), and relationship to `OXY-34` (server parity). TODO `ssl`/`middlewares` either resolved or explicitly deferred with tracked follow-ups.
5. No behavior change to request sending, response decoding, or error handling — purely config/assembly refactoring with backwards compat either preserved (deprecated overloads) or clearly migration-noted.
6. Sibling `OXY-34` alignment: server and client config derive from the same convention (naming, `Config` location, `default`/`layer` helpers, `@envConfig` usage) so the two "Clean up ... paradigm" tasks produce a uniform story.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title is 5 words ("Clean up client configuration paradigm") with no Jira body fetched and no `TODO`/`FIXME` comment mentioning "paradigm" or "clean up" — so interpretation must be inferred from code structure rather than explicit spec.
  - Code signal is moderate-strong for *what* is messy: `Client.Config`'s leaky `URL.Location`+`Path` shape vs `ZClient.Config` provided via separate `RawClient` layers in `Client.scala:18-73` and `RawClient.scala:5-13`, plus example/test call sites (`ViaHttpSpec`, `MultiClientSpec`, `UIMain`) showing three different assembly patterns. Sibling `OXY-34` title mirrors this for server, reinforcing that both are intentional paradigm consolidations.
  - Downgraded from 4/6 because the title gives zero direction on target shape: whether the fix is (a) enriching `Client.Config` with `baseUrl: String` + `ZClient.Config` nesting, (b) introducing a `HttpClientConfig` ADT with `baseUrl` + `tls` + `timeout` + `retry` sub-configs, (c) adopting ZIO Config / `zconfig` vs keeping `JsonCodec`+`@envConfig`, (d) unifying `RawClient` into `Client` or keeping them split, or (e) purely renaming/reorganizing layers without new fields. Any of these satisfies "Clean up paradigm" but implies different scope. Exact acceptance criteria remain inferred.
  - The `TODO (KR) : accept middlewares / : ssl` in `Client.scala:11-12` is the only explicit hint of intended scope beyond current code, and it is ambiguous whether it is in-scope for this task or deferred to `OXY-80`/`OXY-76`.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **Design decision — `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:18-52` (Verified — current leaky split, Inferred — target shape)**
  - Decide whether `Client.Config` should become `Config(baseUrl: String, logLevel: LogLevel, /* optional: timeout, ssl, headers */)` (typed, user-facing) or `Config(url: URL, logLevel: LogLevel, zClientConfig: ZClient.Config)` (wrapper preserving zio-http's full surface) or `Config(url: String, logLevel, timeout: Duration, ssl: Option[SslConfig])` (full modeling). Recommendation: at minimum replace `kind: URL.Location` + `path: Path` with `baseUrl: String` (or `URL`) + `logLevel`, with `ZClient.Config` nested optionally. Ensure the multi-client pattern (`MultiClientSpec`) can still express two clients with different base URLs sharing a single `RawClient`/`ZClient.Config`. Document the choice and align with `OXY-34`'s server shape (`Server.Config` enrichment decision there).
  - Preserve `Client.Config >> RawClient` URL-prefixing logic (`Client.scala:23-25`) internally but hide `URL.Location`/`Path` from the public API. The current `Config.layer(urlString)` URL parsing with query/fragment guards should remain but move behind the new `baseUrl: String` constructor.

- [ ] **Layer plumbing — `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:54-73` + `RawClient.scala` + `ZioHttpClient.scala` (Verified)**
  - Refactor `Client.layer` so there is a primary path that takes the enriched `Client.Config` alone (internally deriving `RawClient`/`ZClient.Config`). Today's `live: URLayer[RawClient & Config, Client]` and `raw: URLayer[RawClient, Client]` and `default: RLayer[Config, Client] = RawClient.default >>> live` should converge to `Client.layer.live: URLayer[Client.Config, Client]` or `RLayer[Client.Config, Client]` that builds `ZClient.Config.default` / `NettyConfig.defaultWithFastShutdown` internally. Keep `customized`-style escape hatch for users who need raw `ZClient.Config`/`RawClient`.
  - Update `ZioHttpClient` (`ZioHttpClient.scala:7-10`) to accept the enriched config or keep `(rawClient, logLevel)` but have a `fromClientConfig` translator `Client.Config => (RawClient, LogLevel)`. Avoid leaking `RawClient` to users who don't need it — default it internally.
  - Normalize `RawClient` vs `Client` relationship: clarify whether `RawClient` remains public (for `ApiSpecPage` raw GET) or becomes internal with `Client` as the sole public surface. If kept public, ensure its `ZClient.Config` is also reachable via `Client.Config` so `ZClient.Config` tuning does not require dropping to `RawClient`.
  - Address `TODO (KR) : accept middlewares when creating a client / : ssl` (`Client.scala:11-12`) — either implement (add `middlewares`/`ssl` fields to `Client.Config` and wire them) or explicitly defer to `OXY-80`/`OXY-76` with documented decision.

- [ ] **Codec derivation + executable integration — `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala` + `example/apps/ui/src/main/scala/oxygen/example/ui/UIMain.scala:26-35` + `example/apps/web-server/.../WebServerMain.scala` (Verified — no codec, example hardcodes `relativeUrl`; Inferred — target)**
  - Add `derives JsonCodec` / `JsonSchema` to the new `Client.Config` (and nested `ZClient.Config` wrapper if present) so it composes with `oxygen-executable`'s `@envConfig("APP_CONFIG")` (`docs/docs/executable/index.md:224-236`). Ensure `URL`/`baseUrl` serialization is handled (likely `String`-based).
  - Simplify `UIMain` (`UIMain.scala:26-35`) from `ZLayer.succeed(Client.Config.relativeUrl) >>> Client.layer.default` plus separate `RawClient.default` to a single config provision (e.g., `ZLayer.succeed(Client.Config(...)) >>> Client.layer.live`). Keep `UIMain` as the client-side migration exemplar alongside `WebServerMain`.
  - Verify the result works with `CliApp` + `@envConfig` — `Client.Config` must derive the codec so the app can be configured via env var or fallback JSON file without custom decoders.

- [ ] **Tests — `modules/http/zio/src/test/scala/oxygen/http/client/MultiClientSpec.scala` + `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala` (Verified — patterns exist, Inferred — new coverage)**
  - Existing `MultiClientSpec` (multi-client with different base URLs) and `ViaHttpSpec` (via-http integration) must still pass. Update them to use the new `Client.Config` shape; add or update a test that provisions `Client.Config` with a custom `baseUrl`/`logLevel` and asserts `client.config` reflects it and requests prefix correctly.
  - No data-model / schema / migration changes — purely additive config refactor.

- [ ] **Docs — `docs/docs/http/index.md` / `docs/docs/http/client/index.md` (Verified — currently `TODO`) (Inferred scope)**
  - Replace the placeholder with the new paradigm summary: what `Client.Config` contains, how to provide it (`ZLayer.succeed(Config(...))` → `Client.layer.live` vs `@envConfig` in a `CliApp`/`PageApp`), relationship to server config (`OXY-34`), and the `RawClient` vs `Client` distinction (when to use which).
  - Brief migration note for consumers currently using `Client.Config.layer(urlString)` + `RawClient.default` + `Client.layer.live`.

- **Verified vs. inferred:** The leaky `URL.Location`/`Path` shape + `ZClient.Config`/`RawClient` as parallel layers and the three different call-site assembly patterns (`ViaHttpSpec`, `MultiClientSpec`, `UIMain`) were verified by reading `Client.scala`, `RawClient.scala`, and those consumers. That "Clean up paradigm" means unifying these into a single user-facing `Client.Config` with consistent `default`/`layer` helpers and `@envConfig` compatibility — and that docs `TODO`, sibling `OXY-34` alignment, and `ssl`/`middlewares` TODO resolution are in scope — are inferred from the title and checklist sibling structure.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — lean is 2 if only `Client.Config` + `Client.layer` + example/doc updates; 3 if `RawClient`/`ZClient.Config` nesting + `JsonCodec` derivations + migration notes are included; 5 only if TLS/SSL + middleware acceptance + timeout/retry full modeling and parallel `OXY-34` parity negotiation expand the scope
  - Justification: Touches small, well-isolated code (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala`, `RawClient.scala`, `ZioHttpClient.scala`, plus `ViaHttpSpec`/`MultiClientSpec`/`UIMain` and 1 doc file). No new module, no migration/schema, no runtime performance branch — purely type/layer refactoring with a doc update. Slightly smaller than `OXY-34` if `ssl` is deferred.

- **Autonomy:** 3 / 6 — needs product/design choice before coding
  - Justification: Mechanics are mechanical once the target config shape is fixed (an agent can implement the `Client.Config` enrichment and `Client.layer` refactor autonomously), but the core decision — what `Client.Config` should contain (`baseUrl: String` vs `URL` vs `URL.Location+Path`, whether to nest `ZClient.Config`, whether to include `ssl`/`timeout`/`middlewares`) and whether to adopt `JsonCodec`/`@envConfig` vs ZIO Config — is not encoded in the title. Choosing wrong risks rework or premature `OXY-80` scope creep. A 15-minute human decision on the shape (and confirming `OXY-34` parity) would raise autonomy to 5/6.

- **Ambiguity-to-resolve:** 4 / 6 — notable open questions block start
  - Justification: Title is 5 words with no body and no codebase `TODO` pinning the intended paradigm beyond the `ssl`/`middlewares` hint. Four blocking design choices below must be resolved or assumed; the implementation cannot be reviewed without agreeing on them. Lightweight clarification (one paragraph confirming the target `Client.Config` shape and `RawClient` vs `Client` unification) would drop this to 1–2.

## Open Questions

1. **Target config shape:** Should `Client.Config` become `Config(baseUrl: String, logLevel: LogLevel)` (simple, stringly-typed) or `Config(baseUrl: URL, logLevel: LogLevel)` (typed) or `Config(baseUrl: String, logLevel, timeout: Duration, ssl: Option[SslConfig], headers: Map[String,String])` (full modeling)? Should `ZClient.Config` be nested (`zClient: ZClient.Config`) or defaulted internally? This determines migration for `ViaHttpSpec`/`MultiClientSpec`/`UIMain` and backwards-compat story.
2. **RawClient vs Client unification:** Should `RawClient` remain a public layer (for `ApiSpecPage`-style raw GET) or be internalized with `Client` as the sole public surface? If unified, how is `Client.Config >> RawClient` URL-prefixing preserved for the `relativeUrl` dev case (`UIMain`'s `Client.Config.relativeUrl`)? Should `RawClient`'s `ZClient.Config` tuning remain accessible?
3. **Derivation strategy:** Should `Client.Config` derive `JsonCodec`/`JsonSchema` (current `oxygen-executable` `@envConfig` pattern per `docs/docs/executable/` and `WebServerMain.Config`) or adopt ZIO Config (`ConfigProvider`, `DeriveConfig`) or config4s? The choice must align with `OXY-34`'s server plan so server/client don't diverge.
4. **Layer API:** Should the new primary layer be `Client.layer.live: URLayer[Client.Config, Client]` (taking the enriched config, deriving `RawClient` internally) with `RawClient`-based overloads kept as escape hatches, or should `Client.layer.live` continue to accept `RawClient & Config` and the `ZClient.Config` translation happen in the app? Which overloads are deprecated vs removed? How does `localPort(port)` compose with the new shape?
5. **Backwards compatibility:** Must existing `Client.Config.layer(urlString)` / `Config.relativeUrl` / `Client.layer.default` / `Client.layer.localPort` remain (deprecated), or is a breaking migration acceptable? Are downstream consumers (`UIMain`, `ViaHttpSpec`, `MultiClientSpec`) expected to update in the same PR or via a migration guide?
6. **SSL / middleware scope:** Does "paradigm" include resolving the `TODO (KR) : accept middlewares / : ssl` in `Client.scala:11-12` (adding `ssl`/`middlewares` fields) or is that deferred to `OXY-80` (easy HTTPS config to client) and `OXY-76`/`OXY-77` (logging/metrics)? Should `oxygen-http` prescribe a shared convention for these?
7. **Sibling OXY-34 coordination:** Server and client paradigms are to be cleaned in parallel — should they be unified into a single `oxygen-http` config ADT (e.g., `HttpConfig(server: Server.Config, client: Client.Config)`) or kept independent (`Server.Config` vs `Client.Config` with matching conventions)? Sequencing with `OXY-80` TLS task — should TLS config be included now or left for that issue?
8. **Assumption to confirm:** That "client" refers to the `oxygen-http` client (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala` — Epic `OXY-3` In Progress) and not to some other client (e.g., `oxygen-sql` `DbConfig`, `oxygen-pulsar` `PulsarClient.Config`, `oxygen-ui` web client). Confirmed by module proximity and sibling `OXY-34` server pairing, but worth explicit sign-off.

## Open Questions
_Note: see above — consolidated in Estimates & Autonomy section. Duplicate heading kept for template compliance._

- See directly above (7 questions). No additional questions beyond those.

