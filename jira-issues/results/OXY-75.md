# OXY-75 — Add metrics to http-server

## Original
- **Key:** OXY-75
- **Checklist line:** `- [ ] [OXY-75](https://kr-oxygen.atlassian.net/browse/OXY-75) — **Task** · Lower — Add metrics to http-server`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add metrics to http-server
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-75
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Add per-endpoint observability instrumentation to `oxygen-http-server` (`modules/http/zio/.jvm/src/main/scala/oxygen/http/server/`) so every HTTP request handled by `CompiledEndpoints` emits ZIO metrics (histogram/counter) tagged with the endpoint's identity and outcome. It is the **server-side instrumentation** leg of the broader metrics initiative under Epic **OXY-93 — oxygen-metrics**, alongside **OXY-5** (Explore non-in-memory metrics options — research spike), **OXY-56** (Add zio-metrics integration — exporter plumbing via `zio-metrics-connectors`/Prometheus), and **OXY-77** (Add metrics to http-client — client-side mirror). Sibling **OXY-74** (Add logging to http-server) is the logging counterpart — OXY-75 is the metrics counterpart.

Current state (verified by reading `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/EndpointSchema.scala:5-22`, `AppliedEndpoint.scala:27-28`, `CompiledEndpoints.scala:11-80`, `ZioHttpServer.scala:19-29`, `Server.scala:15-30`, `modules/sql/core/src/main/scala/oxygen/sql/SqlMetrics.scala`, `modules/sql/core/src/main/scala/oxygen/sql/query/QueryContext.scala:29-56`, and `modules/general/zio/src/main/scala/oxygen/zio/metrics/`):

1. **Scaffolding already exists but is unused.** `EndpointSchema.metricLabels: Set[MetricLabel]` (`EndpointSchema.scala:17-21`) derives `oxygen.api-name` (optional) + `oxygen.endpoint-name` per endpoint, and `AppliedEndpoint.metricLabels: Set[MetricLabel]` (`AppliedEndpoint.scala:27-28`) exposes it with an explicit `// TODO (KR) : use these` — verified. No call site consumes these labels; grep for `Metric` in `modules/http` returns only those two definitions.
2. **SQL already has the pattern to follow.** `SqlMetrics.queryDuration: Metric.Histogram[Duration]` (`SqlMetrics.scala:9-14`) is a `MetricBuilders.microTimer("oxygen.sql.query.duration", 10.micros, 1.hour)` histogram, and `QueryContext.metrics.track(exeType)` (`QueryContext.scala:29-56`) wraps it with `MetricLabel("query.name"/"query.type"/"query.main-table")` + `MetricLabel("query.execution-type"/"query.batch-size")` and `MetricBuilders.effectResult` (`oxygen.effect-result` tag via `extensions.toAspect`) applied as `effect @@ ctx.metrics.track(...)` in `QueryResult.scala:80-208`. HTTP should mirror this: a `HttpMetrics`/`ServerMetrics` object with a histogram + optional counter, tagged per-endpoint, with `effect-result`/`http.status_code`/`http.method` outcome labels.
3. **No exporter or server metric code exists yet.** `project/Dependencies.scala` and `build.sbt` contain no `zio-metrics-connectors`, `prometheus`, `otel`, or `statsd` dependency (verified by grep). `CompiledEndpoints.toRoutes` (`CompiledEndpoints.scala:31-50`) does structured logging (`ZIO.logInfoAnnotated` for request/response with `method`/`path`/`code`) but no metric update. `ZioHttpServer.serveInternal` (`ZioHttpServer.scala:19-29`) logs `starting web-server...` / `web-server started on port` but does not count connections or expose metrics. The `EndpointMiddleware` path (`ApiSpecEndpointMiddleware`, `McpEndpointMiddleware`) would inherit metrics automatically if instrumentation is at `CompiledEndpoints.handle`.
4. **Scope is distinct from OXY-56.** OXY-56 is *exporter plumbing* (publisher bridge so in-memory ZIO metrics become scrape-able); OXY-75 is *instrumentation* (defining and emitting the HTTP-server metrics that the exporter would later publish). OXY-75 can be implemented with pure `zio.metrics` (histograms/counters) and verified in-memory without waiting for OXY-56's Prometheus endpoint — but the two should share metric naming, label discipline, and bucket tuning. The natural dependency is OXY-56's decision on backend (Prometheus vs OTel) influences label cardinality guidance, but does not block the histogram definition.

Possible alternative reading — that this means adding a `/metrics` Prometheus scrape endpoint to the server — is possible from the title alone, but is disfavored because (a) the `/metrics` endpoint is the canonical OXY-56 deliverable (and OXY-31/32 already established the middleware pattern for server endpoints), (b) the `TODO: use these` on endpoint metric labels points specifically to per-request instrumentation, and (c) OXY-77 is the client mirror, which would not need a scrape endpoint — so "Add metrics to http-server" as instrumentation is the coherent reading.

**Who it affects:** Service operators and on-call (latency/error-rate dashboards, SLOs, alerting), plus performance work in OXY-55 that currently has no way to retain HTTP latency history. No direct end-user impact; purely operational. Developers wiring `oxygen-http-server` will get metrics automatically once they use the instrumented `CompiledEndpoints` — no code change required at call sites if implemented as a wrapper/aspect.

**Inferred acceptance criteria:**
1. A `HttpMetrics` / `ServerMetrics` object exists (e.g., `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/HttpMetrics.scala` or `modules/general/zio/src/main/scala/oxygen/zio/metrics/HttpMetrics.scala`) exposing at least one instrumented metric — frontrunner is `Metric.Histogram[Duration]` named `oxygen.http.server.request.duration` (or `oxygen.http.server.request_duration`) via `MetricBuilders.microTimer` (reuse `microDurations` buckets from `MetricBuilders.scala:12-32`: micros to hours), plus optionally a `Metric.Counter[Long]` for `oxygen.http.server.request.count`.
2. `CompiledEndpoints` (and/or a new `MetricsMiddleware` / `CompiledEndpoints.WithMetrics` wrapper, or a `ZIOAspect` applied per-endpoint) records the duration of every `handle(input)` call, tagging with `AppliedEndpoint.metricLabels` (`oxygen.api-name`/`oxygen.endpoint-name`) plus outcome labels: at minimum `oxygen.effect-result` (success/failure/defect/interrupt via `MetricBuilders.effectResult`) and `http.status_code` (or `oxygen.http.status`) for the final `Response.status.code`. The already-structured logging tags in `CompiledEndpoints.toRoutes` show the intended dimensionality (`method`, `path`, `code`) — metrics should be consistent with those.
3. Metrics are emitted for both success and failure paths — including `NotFound`/`MethodNotAllowed` and defects — with distinct labels so 4xx vs 5xx vs network errors are distinguishable. The existing `tapBoth`/`tapDefect` in `CompiledEndpoints.toRoutes` is a hint for where to hook, but instrumentation should be in `CompiledEndpoints.handle` itself (so it covers both `SeqScan` and `TreeScan` paths, `CompiledEndpoints.scala:85-140`) rather than only the log wrapper.
4. The API-spec endpoint (`ApiSpecEndpointMiddleware` at `GET /oxygen/api-spec`) and MCP `tools/call` endpoints are not special-cased out — they are `AppliedEndpoint`s like any other and get the same `endpointName` labels (`apiSpec`, `list_tools`, etc.), unless an explicit `exclude` config exists.
5. Metrics are in-memory `zio.metrics` only — no exporter dependency added here. OXY-56's Prometheus/OTel publisher will later expose them; verify by polling the ZIO metric registry in tests.
6. Existing integration tests (`modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala`, `MultiClientSpec`, `CompiledApiSpecSpec`) still pass; a new test asserts that after a request to a known endpoint the histogram contains samples with the expected `oxygen.endpoint-name` label (can run without DB or network, using in-memory handler).
7. No schema/migration or breaking API changes — additive, opt-in by default (or always on with negligible overhead — ZIO metrics are cheap — but with a config flag to disable if needed, following OXY-34's config paradigm). Should be described in docs (`docs/docs/http/server/index.md` currently `TODO`), but docs are secondary to code.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title is only 5 words ("Add metrics to http-server") with no Jira body fetched — that caps the ceiling below 5/6.
  - Code signal is strong: `EndpointSchema.scala:17-21` + `AppliedEndpoint.scala:27-28` (`// TODO (KR) : use these`) explicitly prepare `MetricLabel` sets per endpoint that nothing consumes (verified by grep — zero consumers in `modules/http`), which is the exact seam OXY-75 would consume. The SQL side (`SqlMetrics` + `QueryContext.metrics.track` + `extensions.toAspect`) provides a concrete, repo-grounded pattern to mirror (histogram + `MetricLabel` + `oxygen.effect-result` + `microTimer` buckets).
  - Structural signal is strong: the checklist positions OXY-75 (http-server) alongside OXY-77 (http-client), OXY-74/76 (logging), OXY-56 (exporter), OXY-5 (research), and Epic OXY-93 (oxygen-metrics). That sequence — explore (OXY-5) → exporter (OXY-56) → instrument http-server (OXY-75) + http-client (OXY-77) — is the standard observability rollout; it also disambiguates "metrics" as instrumentation (vs exporter) since OXY-56 already covers the `/metrics` endpoint.
  - Downgraded from 5/6 because metric *choice* (histogram-only vs histogram+counter+gauge, bucket boundaries, label set cardinality, status-code vs effect-result emphasis), *placement* (wrapper on `CompiledEndpoints.handle` vs dedicated `MetricsMiddleware` vs `ZioHttpServer` vs `EndpointSchema`-level aspect), and *dependency on OXY-56* (whether to add `zio-metrics-connectors` here or stay pure `zio.metrics`) are not stated anywhere — all are inferred from the SQL precedent and the TODO placement. The Lower priority (not Lowest/Low) also suggests this is opportunistic polish, not a P0 spec with an agreed design.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code. Since confidence is 4 ≥ 3, deeper analysis was performed.

- [ ] **Define server metric(s) — new file `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/HttpMetrics.scala` (or `.../ServerMetrics.scala`) + reuse `modules/general/zio/src/main/scala/oxygen/zio/metrics/MetricBuilders.scala:12-32,35-60` and `extensions.scala:7-15` (Verified — buckets + toAspect exist, Inferred — metric name + file placement)**
  - Create an object exposing at least one instrumented metric following the `SqlMetrics` precedent:
    ```scala
    object HttpMetrics {
      val requestDuration: Metric.Histogram[Duration] =
        MetricBuilders.microTimer("oxygen.http.server.request.duration", 100.micros, 30.seconds)
        // alternative names: "oxygen.http.server.request_duration", "oxygen.http.server.duration"
        // reuse microDurations buckets; min/max TBD — frontrunner is 100µs..30s for HTTP (tighter than SQL's 10µs..1h)
      val requestCount: Metric.Counter[Long] = Metric.counter("oxygen.http.server.request.count")
        // optional; counter is cheap and complements histogram for rate dashboards
    }
    ```
  - Placement decision: `modules/http/zio/.jvm/src/.../server/HttpMetrics.scala` keeps http-server self-contained (preferred — avoids `oxygen-zio` owning http-specific `oxygen.http.server.*` metric names); alternative is `modules/general/zio/src/.../metrics/HttpMetrics.scala` if OXY-93 intends a unified metrics module. Follow the Epic's module decision — verify vs. inferred: `MetricBuilders.microDurations` and `microTimer` reuse is verified; the exact metric *names* (`oxygen.http.server.request.duration` vs `oxygen.http.server.duration`) and counter vs histogram-only choice are inferred.
  - Define the tagging helper (mirror `QueryContext.metrics.track`):
    ```scala
    def track(endpoint: AppliedEndpoint, status: Option[Status] = None, method: Option[Method] = None): ZIOAspectPoly =
      HttpMetrics.requestDuration.tagged(endpoint.metricLabels ++ Set(
        method.map(m => MetricLabel("oxygen.http.method", m.name)),
        status.map(s => MetricLabel("http.status_code", s.code.toString)),
      ).flatten).tagged("oxygen.effect-result", effectResult(exit)).toAspect
    ```
    The label set must include the endpoint identity (`AppliedEndpoint.metricLabels` = `oxygen.api-name`/`oxygen.endpoint-name`) and outcome labels. Frontrunner: `oxygen.effect-result` (success/failure/defect via `MetricBuilders.effectResult`) + `http.status_code` (200/404/500) + `oxygen.http.method`. Which labels are included affects dashboard cardinality — decide port/path normalization (do NOT use raw `fullPath` with IDs — use endpoint name instead). The `path` vs `endpoint-name` choice is inferred.

- [ ] **Instrument request handling — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/CompiledEndpoints.scala:11-180` (Verified — handle is the seam, Inferred — wrapper shape)**
  - Wrap `CompiledEndpoints.handle` so every request is timed. Recommended design (verify compiles with ZIO 2.1.21 + zio-http 3.7.4 from `project/Dependencies.scala`):
    - Add a decorator `CompiledEndpoints.WithMetrics(underlying: CompiledEndpoints)` (parallel to existing `WithRequestMiddleware`/`WithResponseMiddleware` at `CompiledEndpoints.scala:145-162`) that does:
      ```scala
      override def handle(input: EndpointInput): ZIO[Scope, Response, Response] =
        ZIO.timed(underlying.handle(input)).flatMap { case (duration, result) =>
          // update histogram with metricLabels + status
        } // plus .tapError / .tapDefect to capture failure status
      ```
      or leverage `MetricBuilders`/`extensions.toAspect` as `effect @@ track(endpoint)` if the endpoint is resolved before handling. The TreeScan path (`CompiledEndpoints.TreeScan:114-140`) resolves candidates via `compatibleEndpoints`; the wrapper must handle the three branches (single match, multi-match via `SeqScan`, no-match/405).
    - Alternatively, introduce a dedicated `MetricsMiddleware` implementing `RequestMiddleware` or `EndpointMiddleware` (follow `ApiSpecEndpointMiddleware`/`McpEndpointMiddleware` pattern in the same package) and register it as a `CompiledMiddlewares` entry. The trade-off: middleware is reusable but runs earlier (before routing) and lacks the endpoint identity until after `TreeScan` narrows candidates — so a `CompiledEndpoints` wrapper that has access to the matched `AppliedEndpoint` is cleaner.
    - A simpler third option: instrument inside each `SeqScan.loop`/`TreeScan.handle` branch directly (minimal wrapper, no new file) — works but duplicates timing logic across three branches.
  - Ensure timing covers both success (`ZIO.succeed(response)`) and error (`ZIO.fail(response)`) paths with `exit.timed` / `tapBoth` / `ensuring` so 4xx/5xx latencies are recorded. Use `ZIO.exit` + `MetricBuilders.effectResult` pattern from `extensions.scala:7-15`.
  - Resolve endpoint for labeling: when `matchingMethod.length == 1`, the endpoint is known before `handle` returns; when multiple match, either label with the eventual winning endpoint (requires post-resolution tagging, e.g., tag inside `SeqScan.loop` after a `Some(response)`) or label as `endpointName = "unknown"` for 404/405. Document the 404/405 labeling policy — verified that `CompiledEndpoints.scala:85-140` has explicit 404/405 branches where no `AppliedEndpoint` is known.
  - Interact with existing logging (`CompiledEndpoints.toRoutes:28-50` does `ZIO.logInfoAnnotated` before/after `handle`) — metrics should not duplicate log logic; place timing *around* the inner `handle` so `toRoutes` logs still fire within the timed span.

- [ ] **Optionally: configuration — `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/Server.scala:15-30` (Verified — Server.Config today is `errorConfig` only; Inferred — whether metrics need a flag)**
  - Consider adding a `Server.MetricsConfig(enabled: Boolean = true, trackNotFound: Boolean = false)` under `Server.Config` (or a standalone `HttpMetricsConfig`) following OXY-34's config-paradigm cleanup. If OXY-34 lands before OXY-75, place it there; if OXY-75 lands first, keep metrics always-on (ZIO metrics overhead is negligible) or add a local `MetricsMiddleware.Config(enabled)` with a layer helper. The config requirement is inferred — no existing `Config` field mentions metrics.
  - If exporter is added (OXY-56), this config may also control the scrape endpoint path vs instrumentation toggle — confirm split of concerns (this task: toggle for *emission*, OXY-56: toggle for *export*).

- [ ] **Dependencies — `project/Dependencies.scala` + `build.sbt` (Verified — no new dep needed for instrumentation alone)**
  - No new dependency required if staying pure `zio.metrics` (already in `dev.zio:zio:2.1.21` core, `Modules/general/zio`). Reuse `zio.metrics.Metric`, `MetricLabel`, `MetricBuilders`, `extensions.toAspect`.
  - If the instrumentor is placed under `oxygen-http` (JVM-only module `modules/http/zio/.jvm`), ensure it depends on `oxygen-zio` for `MetricBuilders` — verify `build.sbt` `oxygen-http` → `oxygen-zio` dependency; add if missing (verify at impl time). Alternative: duplicate the micro-bucket table locally to avoid cross-module dep — not recommended.

- [ ] **Tests — `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala:20-27`, `CompiledApiSpecSpec.scala`, plus new `HttpMetricsSpec` (Verified — integration test patterns exist; Inferred — new coverage shape)**
  - Add a new spec (e.g., `modules/http/zio/src/test/scala/oxygen/http/server/HttpMetricsSpec.scala` or extend `ViaHttpSpec`) that:
    1. Creates a minimal `AppliedEndpoints` with one or two endpoints (follow `UserApiContract`/`DirectSpec` precedent in `modules/http/it-test/src/test/scala/oxygen/http/`),
    2. Wraps/commits via `CompiledEndpoints.TreeScan.fromEndpoints(...).withMetrics` (or the chosen API) and sends a real `Request` through `ReceivedRequest.fromRequest` → `handle` (or via `Server` + `ViaHttpSpec`'s `GET /oxygen/api-spec` flow),
    3. Polls the ZIO metric registry (e.g., `ZIO.succeed(MetricRegistry.snapshot())` or `Metric.poll` / `Metric.value` APIs — verify exact polling API at impl time; `zio-metrics` polling is done via `Runtime.default.unsafe` + `Metric` keys) and asserts `oxygen.http.server.request.duration` (or chosen name) has observations with the expected `oxygen.endpoint-name` label, and that `http.status_code` reflects 200 vs 404 vs 500.
  - Also assert counts on a second request to the same endpoint show the counter/histogram incrementing (idempotent second sample). Test should run as a normal `zio-test` without DB or network (use in-memory handler like `ViaHttpSpec`).
  - Existing tests (`ViaHttpSpec` currently does `Server.Config.defaultLayer` + `Server.layer.serving`) must continue to pass — verify no regression if metrics wrapper is default-included (it should be no-op observable except in metric snapshots).

- [ ] **Docs — `docs/docs/http/server/index.md` (Verified — currently `TODO : Oxygen HTTP Server`) + `docs/docs/http/index.md` (Inferred scope)**
  - Add a short subsection under HTTP Server docs listing the metric name(s), label set (`oxygen.api-name`, `oxygen.endpoint-name`, `http.status_code`, `oxygen.effect-result`, `oxygen.http.method`), bucket boundaries (`microDurations` 100µs..30s or the final choice), and how to consume them (ZIO `Metric` polling in tests; later via OXY-56's `GET /metrics` Prometheus endpoint). Cross-reference OXY-56 for export and OXY-74 for log correlation (logs carry `method`/`path`/`code` already). Keep docs minimal — point to `HttpMetrics.scala` for authoritative names.

- [ ] **Coordination with OXY-56 / OXY-93 — no code in this task, but verify at impl time**
  - Before merging, confirm label naming and metric name conventions with OXY-56 (exporter) and Epic OXY-93 (module placement) to avoid renaming after publication. Check whether OXY-56 already created `modules/metrics` or `modules/general/metrics` — if so, place `HttpMetrics` there instead of `modules/http`. If the exporter uses OTel, the resource/metric namespace (`oxygen.http.*` vs `http.server.*`) may shift.

- **Verified vs. inferred:** The unused `metricLabels` TODO, the SQL `SqlMetrics`/`QueryContext`/`MetricBuilders`/`toAspect` patterns, the absence of any exporter dep, and the pure-logging `CompiledEndpoints.toRoutes` were verified by reading the files and grepping `modules/http` + `modules/general` + `modules/sql`. That OXY-75 means instrumenting `CompiledEndpoints.handle` with a `Metric.Histogram[Duration]` (micro-buckets) + `Counter` tagged by endpoint identity + outcome (status/effect-result), rather than adding a `/metrics` Prometheus endpoint (which belongs to OXY-56), and the wrapper/middleware placement, metric names, label set, bucket range, and config-toggle decisions are inferred from checklist positioning and idiomatic ZIO `Metric` usage.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — small feature; 5 if counter+histogram+config+docs+polish and cross-check with OXY-56.
  - Justification: A single histogram + wrapper + registry-polling test is half a day to a day of focused work; the higher end covers the counter, `Server.Config` flag, 404/405 edge cases, label cardinality review, and doc pass. Lower priority (Lower) aligns with this being operational polish, not a correctness bug.
- **Autonomy:** 4 / 6 — mostly autonomous with light review.
  - Justification: Scope is bounded and the precedent (`SqlMetrics` + `QueryContext.track` + `extensions.toAspect`) plus the already-allocated `metricLabels` give a clear implementation path an agent can execute without constant pairing. Needs human confirmation on (a) metric naming/label set (`oxygen.http.server.request.duration` vs alternatives, `http.status_code` vs `oxygen.http.status`, path normalization), (b) where the instrumentation lives (`CompiledEndpoints.WithMetrics` vs `MetricsMiddleware`), and (c) bucket range — those are one-time decisions. Also confirm placement if Epic OXY-93 has already decided on a `modules/metrics` home.
- **Ambiguity-to-resolve:** 3 / 6 — moderate; doable but benefits from 2–3 clarifications before coding.
  - Justification: Core intent ("wrap http-server request handling with `zio.metrics` histogram/counter tagged per endpoint") is clear from the TODO + SQL precedent, but four blocking details (metric name/namespace, exact label set and 404/405 handling, wrapper vs middleware placement, bucket boundaries / whether a counter/gauge also) are unstated and materially affect dashboards and backwards compat. A short product note — e.g., "histogram `oxygen.http.server.request.duration` 100µs..30s, labels `oxygen.api-name`/`oxygen.endpoint-name` + `http.status_code` + `oxygen.effect-result`, put it in `CompiledEndpoints.WithMetrics`, 404 labeled `endpoint=__not_found__`, no config toggle needed" — would drop this to 1.

## Open Questions

1. **Metric name/namespace:** Should the primary histogram be `oxygen.http.server.request.duration`, `oxygen.http.server.request_duration`, or `http.server.request.duration` (OTel convention)? Should metric names follow Prometheus underscore or dot naming? Affects exporter mapping.
2. **Label set and 404/405:** Exactly which labels attach to every observation — just `oxygen.api-name`/`oxygen.endpoint-name` (verified existing) vs also `http.status_code`/`oxygen.http.method`/`oxygen.effect-result`? How are `NotFound`/`MethodNotAllowed` labeled when no `AppliedEndpoint` matches (e.g., `endpoint=__not_found__` vs no endpoint label vs low-cardinality sentinel)?
3. **Placement:** Wrapper at `CompiledEndpoints` (`WithMetrics`) vs dedicated `MetricsMiddleware` (`RequestMiddleware`/`EndpointMiddleware`) vs inside `ZioHttpServer` vs aspect on `EndpointSchema` derivation? Wrapper has endpoint identity; middleware runs before routing and would need to resolve endpoint late. Which location is canonical affects reuse by `ApiSpecEndpointMiddleware` and MCP endpoints.
4. **Metric cardinality — counter and gauge:** Is a single histogram sufficient, or should this also add `oxygen.http.server.request.count` (`Metric.counter`) for rate and `oxygen.http.server.request.active` (`Metric.gauge` for in-flight) to mirror typical HTTP instrumentation? Histogram+counter is common; gauge is optional but useful for concurrency.
5. **Bucket boundaries:** Reuse `MetricBuilders.microDurations` verbatim (10µs..1h from SQL) or narrower for HTTP (e.g., 100µs..30s, with sub-ms + 1s..10s buckets emphasized)? SQL's hour-long tail is excessive for HTTP and adds bucket cardinality at the exporter.
6. **Bucket/path normalization:** Must path-parameter values (e.g., `/users/123`) never appear as metric labels — only endpoint names (`endpoint=user_get`). Confirm that `AppliedEndpoint.metricLabels` is sufficient and that raw `Request.url.path` is NOT used as a label (high cardinality risk).
7. **Dependency on OXY-56 / export:** Should OXY-75 depend on OXY-56's choice (Prometheus via `zio-metrics-connectors` vs OTel) before landing? Instrumentation is decoupled (pure `zio.metrics`), but OTel's resource/scope naming may change the desired metric name prefix — confirm sequencing with OXY-93.
8. **Config toggle:** Should instrumentation be always-on or gated by a `Server.MetricsConfig(enabled)` / `EndpointMiddleware.Config` flag? ZIO metrics are cheap, but some teams want to disable per-env; OXY-34's config cleanup may be the right home — confirm whether this task owns the toggle or defers it.
9. **Assumption to confirm:** That "Add metrics to http-server" means per-request duration/count tagging (instrumentation), not (a) adding a `GET /metrics` Prometheus scrape endpoint (assumed OXY-56), nor (b) integrating with an external APM vendor (New Relic/Datadog) directly, nor (c) just documenting the existing `MetricLabel` sets. If (a), scope shifts to the OXY-31/32 endpoint-middleware precedent.

