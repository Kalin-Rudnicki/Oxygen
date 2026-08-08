# OXY-76 — Add logging to http-client

## Original
- **Key:** OXY-76
- **Checklist line:** `- [ ] [OXY-76](https://kr-oxygen.atlassian.net/browse/OXY-76) — **Task** · Low — Add logging to http-client`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Add logging to http-client
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-76
- **Checklist section:** To Do
- **Epic:** OXY-3 — oxygen-http-client (In Progress) — Epic filter: PASS (belongs to OXY-3, direct module match `modules/http/zio/src/main/scala/oxygen/http/client/`)

## Expanded Description

**What this likely is:** Enhance the `oxygen-http` client (`oxygen-http-client`, Epic OXY-3 In Progress) with configurable, structured request/response logging. Today the client has minimal, hard-coded logging; this task is to make it production-grade and symmetric with the planned server logging (OXY-74).

**Current state (verified by reading code):**

1. **Existing logging is thin (2 lines).** `modules/http/zio/src/main/scala/oxygen/http/client/ZioHttpClient.scala:25-27` does:
   ```scala
   _ <- ZIO.logAtLevel(logLevel)(s"Sending request [${rawRequest.method}] ${rawRequest.url.encode}", Cause.Empty)
   response <- rawClient.client.request(rawRequest)
   _ <- ZIO.logAtLevel(logLevel)(s"Response status: ${response.status}", Cause.Empty)
   ```
   `logLevel` comes from `Client.Config(logLevel: LogLevel)` (`Client.scala:18-21`). No duration, no api/endpoint in the message itself (though `ZIOAspect.annotated("api-name" -> extras.apiName, "endpoint-name" -> extras.endpointName)` at `ZioHttpClient.scala:30` adds structured annotations), no error/throwable logging, no body/header logging, no timing.

2. **Config shape is primitive.** `Client.Config` is `Config(kind: URL.Location, path: Path, logLevel: LogLevel)` with factories `relativeUrl`, `layer(urlString)`, `fromClient` (`Client.scala:28-47`). It mirrors the anemic `Server.Config(errorConfig)` pattern being cleaned up in OXY-34/OXY-35. There is no `Logging` sub-config akin to `oxygen.sql.DbConfig.Logging(queryLogLevel, logSql)` (`modules/sql/core/src/main/scala/oxygen/sql/DbConfig.scala:41-47` derives `JsonSchema`, defaults `Trace`+`true`, drives `ZIO.logAtLevel` in `Database.scala:37-38`). The `TODO (KR) : accept middlewares when creating a client / : ssl` at `Client.scala:11-12` suggests middleware-based extensions (logging as middleware) were anticipated but not implemented — `RequestMiddleware`/`ResponseMiddleware` (`Client.scala` companions) are currently empty `TODO (KR)` stubs.

3. **Sibling symmetry.** Checklist siblings form a 2x2: logging `OXY-74`(server)/`OXY-76`(client) both Low, metrics `OXY-75`(server)/`OXY-77`(client) both Lower. `OXY-35` analysis notes `OXY-76`/`OXY-77` likely depend on the OXY-35 client config cleanup. Server doc placeholder `docs/docs/http/index.md: "TODO : Oxygen HTTP"` and `docs/docs/http/client/index.md: "TODO : Oxygen HTTP Client"` confirm http docs are not yet fleshed out.

**Who it affects:** Consumers of `oxygen.http.client.Client` / `DeriveClient` (every service-to-service call via `ViaHttpSpec`-style typed clients, plus `UIMain` browser client). Today they get a single `logLevel` knob that logs at the same level for success and failure, with no way to log duration, errors, or redact sensitive headers, and no way to tune per-api/endpoint.

**Why it matters (Low priority):** Low (not High) indicates DX/observability improvement, not a production bug. As `oxygen-http-client` matures (Epic OXY-3), ad-hoc logging in each service (`ZIO.logInfo` around `client.send`) is copy-paste and inconsistent. Structured client logging (with `api-name`/`endpoint-name` annotations already partially present) enables tracing correlation and pairs with server logging (OXY-74) for end-to-end request visibility. Low priority suggests it is not blocking OXY-3 release but is a polish item before `oxygen-http` docs (OXY-81).

**Inferred acceptance criteria:**

1. Client logging is configurable via `Client.Config` (either enriching existing `logLevel: LogLevel` to a `Logging` sub-config like `Logging(level: LogLevel, logBody: Boolean, logHeaders: Boolean)` mirroring `DbConfig.Logging`, or adding `logLevel` semantics that differentiate success vs. failure, or a logging middleware). At minimum, duration and error cases are logged, and `apiName`/`endpointName` annotations remain.
2. No breaking change to `DeriveClient`/`DerivedClientEndpointImpl` call sites beyond `Client.Config` evolution — existing `Client.layer.localPort` / `Config.layer(urlString)` patterns continue (deprecated) or are migrated in `ViaHttpSpec`/`MultiClientSpec`/`UIMain`.
3. Logging respects redaction: headers/body not logged by default or gated behind an explicit flag (to avoid leaking auth tokens), consistent with `DbConfig.Logging.logSql` precedent.
4. Tests or a doc note describe the logging behavior (what is logged at which level, how to configure via `@envConfig`/env, and relationship to OXY-74 server logging and OXY-77 metrics).
5. No behavior change to request encoding / response decoding / `ClientErrorHandler` — purely observability.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title+module give strong signal: "http-client" unambiguously maps to `modules/http/zio/src/main/scala/oxygen/http/client/` (Epic OXY-3), the only client module in the repo. Priority Low + Task type aligns with an observability enhancement, not a bug.
  - Code signal is strong for *what exists vs. what is missing*: `ZioHttpClient.scala:25-30` shows exactly two `logAtLevel` lines with `Cause.Empty` (no error cause, no duration), and `Client.Config`'s single `logLevel` field vs. `DbConfig.Logging`'s `queryLogLevel + logSql` pattern shows the gap. Sibling issues OXY-74/OXY-77 and the `TODO` middlewares corroborate the intended middleware/logging direction.
  - Downgraded from 5/6 because the title gives zero direction on *target shape*: whether the fix is (a) enriching `Client.Config` to `Logging(level, logBody, logHeaders)`, (b) a dedicated `RequestMiddleware`/`ResponseMiddleware` logging implementation, (c) adding duration + error-cause logging only, (d) adopting `zio-logging` structured fields vs. plain `ZIO.logAtLevel`, or (e) just documenting the existing `logLevel`. Any satisfies "Add logging" but implies different scope. The `logLevel` already in `Config` also creates ambiguity whether this task is to *add* logging (it already exists) or to *improve* it.
  - No Jira body fetched, no skipped test, no design doc — so exact log format remains inferred.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **Config enrichment — `modules/http/zio/src/main/scala/oxygen/http/client/Client.scala:18-47` (Verified — current `logLevel: LogLevel` only; Inferred — target shape)**
  - Decide whether to keep `Config(kind, path, logLevel)` and expand `logLevel` semantics, or introduce `Config.Logging(level: LogLevel, logBody: Boolean, logHeaders: Boolean, logDuration: Boolean)` mirroring `oxygen.sql.DbConfig.Logging` (`DbConfig.scala:41-47` derives `JsonSchema`, `default = Logging(Trace, true)`). Recommendation: `final case class Logging(level: LogLevel = LogLevel.Info, logDuration: Boolean = true, logBody: Boolean = false) derives JsonSchema` with `Config(logging: Logging)` or `Config(kind, path, logging: Logging)` to avoid leaking `URL.Location`/`Path` (see OXY-35) while giving a `logBody` gate like `DbConfig.Logging.logSql`. Keep `Config.relativeUrl` / `Config.layer(urlString)` / `Config.fromClient` overloads delegating to the new shape with defaults; add `derives JsonCodec`/`JsonSchema` if aligning with `oxygen-executable` `@envConfig` (as OXY-35 proposes).
  - Preserve `def >>(client: RawClient): RawClient` URL-prefixing (`Client.scala:24-26`) unchanged — logging change is additive, not a layer-restructuring (that is OXY-35's concern).

- [ ] **Client implementation — `modules/http/zio/src/main/scala/oxygen/http/client/ZioHttpClient.scala:7-31` (Verified)**
  - Enhance `ZioHttpClient.send` to log: (a) request start at `logging.level` with `method + url.encode + apiName + endpointName`, (b) on success: response status + duration (measure with `Clock`/`ZIO.timed`), (c) on failure: log at `LogLevel.Error` (or `logging.level` mapped via error) with `Cause` (not `Cause.Empty`), mirroring `Database.scala:37-38` conditional on `logSql`/`logBody`. Keep the existing `ZIOAspect.annotated("api-name", "endpoint-name")` (`ZioHttpClient.scala:30`) and ensure the log message also includes those fields plainly for non-structured backends.
  - Optionally implement logging as a `RequestMiddleware`/`ResponseMiddleware` pair (both currently `TODO (KR)` stubs at `RequestMiddleware.scala:14` / `ResponseMiddleware.scala:14`) so logging can be composed via `Client.scala:15-16` `TODO : accept middlewares when creating a client`. Decision: either inline in `ZioHttpClient` (simpler) or extract to `LoggingMiddleware` that is wired by default via `Client.layer.live`. Document the choice; do not leave both.
  - Handle redaction: when `logBody`/`logHeaders` is true, truncate or omit `Headers`/`Body` that may contain secrets; never log full body by default. This mirrors `logSql: Boolean` gating in `Database.scala:37-38`.

- [ ] **Layer plumbing — no layer signature change required (Verified — `Client.layer.live: URLayer[RawClient & Config, Client]` at `Client.scala:57-58`)**
  - If config gains a `Logging` sub-case class, no `Client.layer` signature change — it already takes `Config`. Ensure `RawClient` remains separate (OXY-35's scope) and logging does not require a new ZLayer. If a logging middleware is introduced, add `Client.layer.withLogging` or accept `RequestMiddleware` via `Client.scala:15` TODO, but keep `Client.layer.default`/`localPort` working.

- [ ] **Tests — `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala` + `modules/http/zio/src/test/scala/oxygen/http/client/MultiClientSpec.scala` (Verified — patterns exist; Inferred — new coverage)**
  - Existing `ViaHttpSpec` (via-http integration) and `MultiClientSpec` (multi-client with different base URLs) must still pass. Add or update a test that provisions `Client.Config` with custom `Logging`/`logLevel` and asserts logs are emitted (e.g., via `ZIO TestLogger` or `ZTestLogger` asserting `logAtLevel` output contains `api-name`/`endpoint-name` and status). Consider a `ZioHttpClient` unit test that mocks `RawClient` to assert duration logging and error-cause logging.

- [ ] **Docs — `docs/docs/http/client/index.md` (Verified — currently `TODO : Oxygen HTTP Client`) + `docs/docs/http/index.md` (Inferred scope)**
  - Replace placeholder with logging summary: what is logged (request method+url, status, duration, error cause), how to configure (`Client.Config(Logging(...))` or `Config.layer(url, logLevel)`), how `api-name`/`endpoint-name` annotations correlate with server logs (OXY-74), and redaction guidance. Brief note on `RequestMiddleware`/`ResponseMiddleware` if logging is middleware-based.

- **Verified vs. inferred:** The two-line `ZIO.logAtLevel` with `Cause.Empty`, the `logLevel: LogLevel` field, the `TODO` middlewares, and the `DbConfig.Logging` precedent were verified by reading `ZioHttpClient.scala`, `Client.scala`, `RequestMiddleware.scala`, `ResponseMiddleware.scala`, and `DbConfig.scala`. That "Add logging" means adding duration + error-cause + structured annotations + configurable `Logging` sub-config (and possibly body/header gating) — and that it should mirror `DbConfig.Logging`/`Database.scala` and pair with OXY-74 — are inferred from the title and sibling structure.

## Estimates & Autonomy

- **Story points:** 2 (Fibonacci) — lean is 1 if only enriching `ZioHttpClient` with duration + error logging; 2 if `Client.Config.Logging` sub-config + `JsonSchema` derivation + middleware extraction + doc/test updates; 5 only if full `RequestMiddleware`/`ResponseMiddleware` logging framework + redaction + `@envConfig` integration + OXY-35 parity negotiation expand scope
  - Justification: Touches small, well-isolated code (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala`, `ZioHttpClient.scala`, optional `RequestMiddleware.scala`/`ResponseMiddleware.scala`, plus `ViaHttpSpec`/`MultiClientSpec` and 1 doc file). No new module, no schema/migration, no runtime performance branch — purely additive logging around the existing `send` path. Smaller than OXY-35/OXY-34 paradigm refactors.

- **Autonomy:** 4 / 6 — needs minor product choice, then fully autonomous
  - Justification: Mechanics are mechanical once the logging shape is fixed (an agent can implement `Logging(level, logBody)` + `ZioHttpClient` duration/error logging + `TestLogger` coverage autonomously, following `DbConfig.Logging`/`Database.scala` precedent). The core decision — `logLevel` vs `Logging` sub-config vs middleware, and whether to log body/headers — is not encoded in the title but is a bounded choice (15-minute human decision or agent picks `DbConfig`-style precedent). No cross-module coordination beyond OXY-74 symmetry.

- **Ambiguity-to-resolve:** 3 / 6 — moderate, one decision blocks start
  - Justification: Title is 4 words ("Add logging to http-client") with no body and no `TODO` pinning the intended log shape beyond the two existing `logAtLevel` lines. One blocking choice — `Client.Config` logging shape (`LogLevel` vs `Logging` ADT vs middleware) and redaction policy — must be resolved or assumed; otherwise implementation can be reviewed against the `DbConfig` precedent. Lightweight clarification (one paragraph confirming `Logging(level, logBody)` and duration+error scope) would drop this to 1.

## Open Questions

1. **Logging shape:** Should `Client.Config` keep `logLevel: LogLevel` (just improve `ZioHttpClient` to log duration/errors) or gain a `Logging` sub-config (`level: LogLevel, logDuration: Boolean, logBody: Boolean, logHeaders: Boolean`) mirroring `DbConfig.Logging(queryLogLevel, logSql)`? Should it derive `JsonSchema`/`JsonCodec` for `@envConfig`?
2. **Middleware vs inline:** Should logging be a dedicated `RequestMiddleware`/`ResponseMiddleware` (filling the `TODO (KR)` stubs, composable via the `TODO : accept middlewares when creating a client` at `Client.scala:15`) or inline in `ZioHttpClient.send`? Which is the primary extension point for OXY-77 metrics as well?
3. **What to log:** Beyond method+url+status (already done), should we log duration, request headers/body, response headers/body, error cause (`Cause` vs `Cause.Empty`), and `apiName`/`endpointName` in the message body (not just annotations)? Should query params be included?
4. **Level strategy:** Should success log at `logging.level` and failures at `LogLevel.Error` (or `Cause`-aware), or should a single level apply to all? Should there be a separate `errorLogLevel`?
5. **Redaction:** Must body/header logging be gated (default off) to avoid leaking auth tokens/secrets? Should `logBody` truncate at N bytes? This mirrors `DbConfig.Logging.logSql` gating.
6. **Sibling OXY-74 coordination:** Should client and server logging share a convention (e.g., both use `Logging(level, logBody)` + `ZIOAspect.annotated("api-name")`, same doc section), or are they independent? Should they be introduced together?
7. **Assumption to confirm:** That "http-client" refers to `oxygen-http-client` (`modules/http/zio/src/main/scala/oxygen/http/client/Client.scala` — Epic OXY-3 In Progress) and not to `oxygen-pulsar`/`oxygen-sql` clients or the `oxygen-ui` web client (Epic OXY-83). Confirmed by module proximity and checklist epic grouping, but worth explicit sign-off.
