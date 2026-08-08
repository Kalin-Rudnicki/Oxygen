# OXY-79 — Add easy HTTPS configuration to server

## Original
- **Key:** OXY-79
- **Checklist line:** `- [ ] [OXY-79](https://kr-oxygen.atlassian.net/browse/OXY-79) — **Task** · Lowest — Add easy HTTPS configuration to server`
- **Type:** Task
- **Priority:** Lowest
- **Title (verbatim):** Add easy HTTPS configuration to server
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-79
- **Checklist section:** To Do
- **Epic filter:** Not an Epic — Task under `oxygen-http-server` Epic OXY-2 (In Progress). Not filtered.

## Expanded Description

**What this likely is:** Expose an ergonomic, oxygen-idiomatic way to enable TLS/HTTPS on `oxygen-http` server without requiring callers to construct raw `zio.http.SSLConfig` / `ZServer.Config` + `NettyConfig` manually.

**Current state (verified):**

- `oxygen.http.server.Server.Config` (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala:22-26`) contains exactly one field: `errorConfig: ServerErrorConfig`. Port/host/TLS are *not* in it. Transport config lives outside oxygen's abstraction as `zio.http.Server.Config` + `zio.http.netty.NettyConfig` passed via `Server.layer.customized: URLayer[ZServer.Config & NettyConfig, Server]` / `Server.layer.live: URLayer[ZServer.Config, Server]` / `Server.layer.simple(port)` (`Server.scala:48-60`). Callers (e.g. `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala:48-83`) must assemble two parallel layers: `ZLayer.succeed(Server.Config(errors))` + `Server.layer.simple(port)` — the latter internally does `ZLayer.succeed(ZServer.Config.default.port(port))`.
- `zio.http.Server.Config` (`zio-http 3.7.4`, `zio/http/Server.scala:61`) already has `sslConfig: Option[SSLConfig]` with helpers `def ssl(sslConfig: SSLConfig): Config`. `zio.http.SSLConfig` (`zio/http/SSLConfig.scala`) is rich: `Data.Generate` (self-signed, dev), `Data.FromFile(certPath, keyPath, trustCert)`, `Data.FromResource`, `Data.FromJavaxNetSsl` (JKS file/resource + optional truststore), plus `HttpBehaviour` (Redirect/Accept/Fail), `ClientAuth` (None/Optional/Required), `Provider` (JDK/OpenSSL). None of this is surfaced through `oxygen.http.server.Server`.
- `oxygen.http.client.Client.Config` (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:16`) has `// TODO (KR) : ssl` — the client-side sibling `OXY-80` ("Add easy HTTPS config to client" · Lowest) mirrors this task. Docs are `TODO` (`docs/docs/http/server/index.md`, `docs/docs/http/client/index.md`).
- No `TODO`/`FIXME` mentioning `https`/`ssl`/`tls`/`pem`/`keystore` exists in `modules/http` except the client TODO above. Searched entire `modules/` — only pre-existing hits were in `modules/events/pulsar` (Pulsar TLS) and `Stripe` URL strings.

**Interpretation of "easy":** The word "easy" (vs. just "Add HTTPS support") implies the goal is *not* to re-implement TLS, but to wrap `zio.http.SSLConfig` with a smaller, discoverable oxygen API that works with the existing `Server.Config` / `ZLayer` / `@envConfig` story. Likely shape is one of:

- (a) `Server.Config` gains an optional `https: Option[HttpsConfig]` (or `ssl: Option[SSLConfig]` / `tls`) field that `Server.layer` translates to `ZServer.Config#ssl(...)` internally, so a service can enable HTTPS with a single case-class field rather than dropping to raw `ZServer.Config`.
- (b) Alternatively, a small oxygen ADT `HttpsConfig` (e.g. `Disabled | SelfSigned | FromFile(certPath, keyPath) | FromResource | FromKeyStore(...)`) that maps to `SSLConfig` — hiding `HttpBehaviour`/`Provider`/`ClientAuth` defaults while still exposing an escape hatch `HttpsConfig.Custom(sslConfig: SSLConfig)`.

Either satisfies "easy HTTPS configuration to server" with Lowest priority (DX sugar, not production blocker). The task is the server counterpart to OXY-80; both should share naming/convention.

**Who it affects:** Any service author using `oxygen-http-server` who wants `https://` (local dev with self-signed, prod with PEM/JKS, or mTLS). Today they must know `ZServer.Config.default.ssl(SSLConfig.fromFile(...))` and how to thread `ZServer.Config` through `Server.layer.customized`. The wrapper would let them stay inside `oxygen` types and configure via `JsonCodec`/`@envConfig("APP_CONFIG")` like `WebServerMain.Config.Http`.

**Why it matters (Lowest):** Lowest (not High/Normal) signals tech-debt / DX polish, not outage. As `oxygen-http-server` Epic OXY-2 is `In Progress`, HTTPS is a natural pre-1.0 requirement but not urgent vs. correctness tasks. Cleaning it up now prevents each future service from inventing its own `ZServer.Config` SSL wiring and aligns with companion tasks: `OXY-34` (Clean up server configuration paradigm · Normal), `OXY-74/75` (logging/metrics), `OXY-81` (docs for oxygen-http).

**Inferred acceptance criteria:**

1. A developer can enable HTTPS on an oxygen server by setting a single `Server.Config` field or a small dedicated config case class — without importing `zio.http.SSLConfig` directly — with helpers for common cases: disabled (default), `generate` / self-signed (dev), `fromFile` (PEM cert+key path), `fromResource`, `fromKeyStore` (JKS), and an `custom(SSLConfig)` escape hatch.
2. The config type derives `JsonCodec`/`JsonSchema` (or ZIO Config) so it composes with `oxygen-executable`'s `@envConfig("APP_CONFIG")` / `CliApp` JSON/YAML env config (mirroring `WebServerMain.Config.Http` + `ServerErrorConfig derives JsonCodec`).
3. `Server.layer` translates the new field to `ZServer.Config` internally (e.g. a new `URLayer[Server.Config, Server]` primary path that does `ZServer.Config.default.port(...).copy(sslConfig = ...)`), preserving existing `Server.layer.customized` / `live` / `simple(port)` overloads for backwards compat or deprecating them with a migration note. `ZioHttpServer` (`ZioHttpServer.scala:7-10`) may gain a `fromServerConfig` helper.
4. Default remains HTTP (no SSL) so existing services/tests (`modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala:25`, `CompiledApiSpecSpec`, `McpServerSpec`) continue to pass unchanged; `ViaHttpSpec` which does `Server.Config.defaultLayer >>> Server.layer.serving` keeps working.
5. Documentation updated (`docs/docs/http/server/index.md` — currently `TODO : Oxygen HTTP Server` — or `docs/docs/http/index.md`) with a short example: how to enable HTTPS via config file / env var and via code, and the relationship to OXY-80 (client-side).
6. Parity with OXY-80 considered: server and client `HttpsConfig` / `TlsConfig` naming and derivation are aligned, even if implemented as separate tickets.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title is concise (6 words) but unambiguous in module context ("Add easy HTTPS configuration to server" + Lowest priority + location inside `oxygen-http-server` epic OXY-2) and sibling `OXY-80` ("Add easy HTTPS config to client" · Lowest) confirms intent is DX wrapper for both layers, not a protocol implementation.
  - Code signal is strong for *what is missing*: `Server.Config` is anemic (`errorConfig` only, `Server.scala:22-26`), transport lives in `ZServer.Config.sslConfig: Option[SSLConfig]` (`zio-http 3.7.4` verified via `jar tf` + sources), and `Client.scala:16` has explicit `// TODO (KR) : ssl` — so "easy HTTPS config" maps directly to surfacing `SSLConfig` through oxygen.
  - Downgraded from 5/6 because "easy" is underspecified: exact ADT shape (nest `SSLConfig` directly vs. oxygen `HttpsConfig` ADT vs. enriching `Server.Config` with `port/host/ssl` together per OXY-34's broader paradigm cleanup), plus choices around `HttpBehaviour`/`ClientAuth`/`Provider` defaults, `JsonCodec` vs. ZIO Config derivation, and whether to include mTLS/JKS vs. just PEM `fromFile`, are not encoded in the title and have no `TODO`/design-doc pin. Any subset satisfies the title; the deeper pass below picks the most defensible interpretation.
  - Secondary confirmation: no grader/Jira body was fetched; inference rests entirely on `checklist.md` + repo grep + zio-http 3.7.4 sources, which is the prescribed fallback.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **Design decision — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala:22-34` (Verified — anemic `Config`, Inferred — target shape)** — Decide the config model and align with `OXY-34` (Clean up server configuration paradigm · Normal):
  - Recommendation (minimal, non-breaking): add `https: Option[HttpsConfig] = None` (or `ssl: Option[SSLConfig]` if direct re-export is preferred) to `Server.Config`, keeping `errorConfig` nested: `final case class Config(errorConfig: ServerErrorConfig, https: Option[HttpsConfig] = None)`. Alternatively fold `port`/`host` into `Server.Config` here if doing OXY-34 at the same time, but OXY-79 alone should at least add the HTTPS field with a default of `None` to preserve backwards compat.
  - Define `HttpsConfig` (or reuse `SSLConfig`): small ADT, e.g. `sealed trait HttpsConfig` with `case object Generate` (maps to `SSLConfig.generate`), `case class FromFile(certPath: String, keyPath: String, trustCertPath: Option[String] = None)`, `case class FromResource(...)`, `case class FromKeyStore(...)` (JKS), `case class Custom(sslConfig: SSLConfig)` escape hatch, plus `behaviour: HttpBehaviour = Redirect` / `clientAuth` defaults. Keep JVM-only (`/.jvm/`) if it references `java.net` / `javax.net.ssl` types.
  - Derive `JsonCodec` (and ideally `JsonSchema`) for `HttpsConfig` / `SSLConfig.Data` wrapper so it works with `CliApp @envConfig("APP_CONFIG")` (pattern in `example/apps/web-server/.../WebServerMain.scala:36-68` and `ServerErrorConfig derives JsonCodec` at `ServerErrorConfig.scala:8`). If `SSLConfig` itself does not derive `JsonCodec`, the wrapper must provide codecs + `ZServer.Config` translation.
  - Align naming with OXY-80 client side (`Client.Config` — `Client.scala:18-47`); if client introduces `Client.HttpsConfig` / `Client.TlsConfig`, server should mirror it. Document the choice in a brief decision note.

- [ ] **Layer plumbing — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala:48-90` + `ZioHttpServer.scala:7-30` (Verified)** — Make `Server.layer` derive `ZServer.Config` from `Server.Config` internally:
  - Add or refactor a primary `URLayer[Server.Config, Server]` (or `RLayer[Server.Config, Server]`) that translates `config.https: Option[HttpsConfig]` → `Option[SSLConfig]` → `ZServer.Config.default.copy(sslConfig = ...)` (preserving port/binding from existing config if OXY-34 also enriches `Server.Config` with `port`/`host`). Today's `customized: URLayer[ZServer.Config & NettyConfig, Server]` and `live: URLayer[ZServer.Config, Server]` remain as escape hatches; `simple(port)` delegates to the new path.
  - Update `ZioHttpServer` to accept the translated `ZServer.Config` (no change to `serveInternal` at `ZioHttpServer.scala:19-26` beyond receiving the SSL-enabled `zioConfig`). Optionally add `ZioHttpServer.fromServerConfig(config: Server.Config): ZioHttpServer` helper.
  - Ensure `CompiledEndpoints.toRoutes(config: Server.Config)` (`CompiledEndpoints.scala:31`) is unaffected — it only reads `errorConfig`; new `https` field is ignored there.

- [ ] **Client parity note — `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:14-16` (Verified — `// TODO (KR) : ssl`)** — Do not implement OXY-80 here, but ensure the server `HttpsConfig` shape is compatible with a future client `HttpsConfig` (both mapping to `zio.http.ClientSSLConfig` / `SSLConfig` on the client). Mention the pairing in docs/code comment.

- [ ] **Executable + example — `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala:48-88` (Verified — `Config.Http(port, errors)` + `ZLayer.succeed(Server.Config(errors))` + `Server.layer.simple(port)`)** — Update exemplar to show HTTPS usage (either as a second example config `Config.Http(errors, port, https = None)` or a documented `example-ws-https.json`). Keep the default example HTTP-only. If `Server.Config` now owns `port`, collapse `Config.Http` to `http: Server.Config` as a natural OXY-34 convergence — but at minimum show `Server.Config(errors, https = HttpsConfig.FromFile(...).some)` wiring.

- [ ] **Docs — `docs/docs/http/server/index.md` (Verified — `TODO : Oxygen HTTP Server`) + `docs/docs/http/index.md` (Verified — `TODO`) (Inferred — scope)** — Replace placeholder with short HTTPS section: how to enable (1) code: `Server.Config.default.copy(https = HttpsConfig.Generate.some)` + `Server.layer.live`, (2) config file/env: JSON example for `fromFile`/`fromKeyStore` via `@envConfig("APP_CONFIG")`, (3) note on self-signed `Generate` for dev vs. PEM/JKS for prod, (4) pointer to `zio.http.SSLConfig` for advanced `HttpBehaviour`/`ClientAuth` tuning, (5) link to OXY-80 for client side.

- [ ] **Tests — `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala:20-27` + `CompiledApiSpecSpec.scala`, `McpServerSpec.scala` (Verified — patterns exist, Inferred — new coverage)** — Keep existing `ViaHttpSpec` passing (HTTP default, `Server.Config.defaultLayer >>> Server.layer.serving`). Add at least one new integration test that starts a server with `HttpsConfig.Generate` (self-signed) on an ephemeral port and asserts a route is reachable via an HTTPS client (or at least that the server binds without error and `sslConfig` is set). If full TLS e2e is heavy, a unit test asserting `Server.Config(...).toZServerConfig.sslConfig == Some(...)` translation is acceptable. JVM-only.

- **Verified vs. inferred:** The narrowness of `Server.Config` (single field), the raw `ZServer.Config` + `NettyConfig` parallel layers, the existence of `ZServer.Config.sslConfig: Option[SSLConfig]` with `SSLConfig.generate/fromFile/fromResource/fromJavaxNetSsl`, and the client `// TODO : ssl` were all verified by reading files / `jar tf` of `zio-http_3-3.7.4`. That "easy" means a `JsonCodec`-deriving oxygen wrapper + single-field `Server.Config` enablement + `Server.layer` translation + docs/test is inferred from the title wording and sibling OXY-80/OXY-34 structure.

## Estimates & Autonomy

- **Story points:** 2 (Fibonacci) — 2 if limited to `HttpsConfig` ADT + `Server.Config` field + `Server.layer` translation + one test + doc paragraph; 3 if also folding `port`/`host` into `Server.Config` (OXY-34 overlap), adding JKS/mTLS support, or doing full PEM+resource+keystore e2e test matrix; 1 if just re-exporting `Option[SSLConfig]` with `JsonCodec` passthrough and no ADT.
  - Justification: Touches 2–3 small files in `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/` (`Server.scala`, `ZioHttpServer.scala`, new `HttpsConfig.scala` or inline in `Server.scala`), plus one example/docs update and one it-test. No new module, no migration/schema, no runtime perf branch — purely additive config + layer wiring. The lightest coherent shape (wrapper ADT + translation) is a half-day task.

- **Autonomy:** 4 / 6 — mostly autonomous once shape is agreed
  - Justification: Mechanics are straightforward — define `HttpsConfig`, derive codec, map to `SSLConfig`, thread through `Server.layer` — and `zio-http 3.7.4` API is well-documented and available offline in coursier cache. The only human-preferred decision is the ADT surface (names, which `Data` variants to expose, defaults for `HttpBehaviour`/`ClientAuth`, and whether OXY-34's broader `port`/`host` fold-in should happen concurrently). With a 10-minute decision on the shape (or by adopting the recommended minimal `https: Option[HttpsConfig]` with `Generate`/`FromFile`/file-resource + `Custom` hatch), an agent can implement without further pairing.

- **Ambiguity-to-resolve:** 3 / 6 — moderate, does not block start
  - Justification: Title is 6 words with no Jira body and no in-repo design doc pinning "easy". Three questions should be confirmed before or during impl (see Open Questions), but all have reasonable defaults so an agent can start with assumptions and refine via review. Notably, OXY-34 ("Clean up server configuration paradigm" · Normal) is the broader config refactor that may want to land first or together — doing OXY-79 in isolation risks rework if OXY-34 later moves `port`/`host` into `Server.Config`.

## Open Questions

1. **ADT shape — wrap or re-export?** Should `Server.Config` carry `Option[SSLConfig]` directly (thin re-export) or an oxygen `HttpsConfig` ADT (opinionated, `JsonCodec`-friendly)? Recommendation: ADT with `Custom(SSLConfig)` hatch — but human should confirm.
2. **Scope of `Data` variants.** Which `SSLConfig.Data` sources are in scope for "easy"? Minimum useful is `Generate` (dev) + `FromFile(certPath, keyPath)` (prod PEM). Should `FromResource` and `FromJavaxNetSsl` (JKS file/resource + truststore) be included in v1 or deferred? Include at least `Generate` + `FromFile`; add JKS if client mTLS (`OXY-80`) needs it.
3. **Defaults for `HttpBehaviour` / `ClientAuth` / `Provider`.** `SSLConfig` defaults to `HttpBehaviour.Redirect` + `Provider.JDK` + `clientAuth = None`. Should the oxygen wrapper expose these or hide them behind sensible defaults (expose only `behaviour` and `clientAuth` as optional overrides, default `JDK`)? Recommendation: hide `Provider`, expose `behaviour`/`clientAuth` as optional fields defaulting to `Redirect`/`None`.
4. **Interaction with OXY-34.** OXY-34 wants to clean up the server configuration paradigm (likely folding `port`/`host` into `Server.Config`). Should OXY-79 be implemented on top of that new `Server.Config(port, host, errorConfig, https)` shape, or delivered independently as additive `https: Option[HttpsConfig]` on the current single-field `Server.Config`? Doing it independently is safer and keeps the diff small; doing it together is cleaner but couples two tickets. Human should sequence: recommend OXY-79 *after* or *together with* OXY-34, or explicitly scope OXY-79 to additive-only.
5. **Client parity (OXY-80).** Should server and client share a single `oxygen.http.TlsConfig` type or have distinct `Server.HttpsConfig` / `Client.HttpsConfig`? Since server uses `SSLConfig` and client uses `ClientSSLConfig` in `zio-http`, separate types that map to different zio-http configs may be cleaner, but naming (`HttpsConfig` vs `TlsConfig` vs `SslConfig`) should be aligned. Confirm naming before implementing both.
6. **Testing strategy.** Is an e2e HTTPS integration test (self-signed `Generate` + client that trusts it) required for acceptance, or is a unit translation test (`HttpsConfig.FromFile("cert.pem","key.pem")` → `SSLConfig.FromFile` → `ZServer.Config.sslConfig`) sufficient? Recommendation: at minimum a translation unit test; e2e is nice-to-have given Lowest priority and the existing `ViaHttpSpec` pattern.
7. **Docs vs. code priority.** Given `docs/docs/http/server/index.md` is still `TODO`, should OXY-79 include the first real docs page for HTTP server (beyond just HTTPS), or limit docs to a small HTTPS subsection? Recommendation: small subsection now; full HTTP server docs belong to OXY-81.

