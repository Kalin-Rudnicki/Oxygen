# OXY-77 — Add metrics to http-client

## Original
- **Key:** OXY-77
- **Checklist line:** `- [ ] [OXY-77](https://kr-oxygen.atlassian.net/browse/OXY-77) — **Task** · Lower — Add metrics to http-client`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add metrics to http-client
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-77
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Instrument the `oxygen-http-client` (`modules/http/zio/src/main/scala/oxygen/http/client/`) with ZIO metrics (`zio.metrics.Metric`) so outbound HTTP calls emit observable timing/counter data, mirroring how `oxygen-sql` already instruments queries via `SqlMetrics.queryDuration` + `QueryContext.metrics.track` and how the server side is planned to be instrumented in sibling **OXY-75 — Add metrics to http-server**.

Today the client has **zero metric instrumentation** — verified by grep: no `Metric`, `MetricLabel`, `MetricBuilders`, or `toAspect` import exists under `modules/http/zio/src/main/scala/oxygen/http/client/`. The only observability in the client is text logging: `ZioHttpClient.send` (`modules/http/zio/src/main/scala/oxygen/http/client/ZioHttpClient.scala:14-33`) logs `Sending request [METHOD] url` and `Response status` at `config.logLevel`, and annotates the effect with `ZIOAspect.annotated("api-name" -> extras.apiName, "endpoint-name" -> extras.endpointName)` — but records no `Metric.Histogram[Duration]`, counter, or tagged histogram. `Client.RequestExtras` already carries the ideal low-cardinality labels (`apiName`, `endpointName`) and `SendRequest` carries `method`/`path`, so the label set is ready; nothing publishes it.

"Add metrics to http-client" therefore means adding one or more `Metric` definitions (most likely a `Metric.Histogram[Duration]` request-duration timer via `oxygen.zio.metrics.MetricBuilders.microTimer`, plus optionally counters for total/error responses) and wiring them into the client send path so every outbound call records duration and outcome with appropriate `MetricLabel`s. This is the *instrumentation* half; the *export* half (scraping/pushing those metrics externally) is sibling **OXY-56 — Add zio-metrics integration** (Lower) and the research spike **OXY-5 — Explore non-in-memory metrics options**, both under the In Progress Epic **OXY-93 — oxygen-metrics**. The client metrics produced here would only become externally visible once OXY-56's exporter (e.g., `zio-metrics-connectors-prometheus` `/metrics` endpoint) is wired — until then they live in ZIO's in-memory registry (useful in tests, invisible to ops).

This task is the client-side counterpart to **OXY-75** (server) and complements **OXY-76 — Add logging to http-client** (Low) and **OXY-74 — Add logging to http-server** (Low). Together OXY-74/75/76/77 form the HTTP observability quartet. Parent epic is almost certainly **OXY-3 — oxygen-http-client** (In Progress) for ownership, with cross-cutting ownership by **OXY-93 — oxygen-metrics** for metric conventions.

**Who it affects:** Service authors making outbound calls via `Client` / `DerivedClientEndpointImpl` (every `oxygen-http` client consumer) and operators/SREs who need latency/error visibility on downstream dependencies. No direct end-user impact; purely operational. Performance work in OXY-55 also benefits from having client timings available.

**Why it matters (Lower priority):** Lower (not High) indicates enabling / observability tech-debt, not a production bug. As `oxygen-http-client` matures under Epic OXY-3, lacking client-side timings makes it hard to diagnose slow downstream calls, set SLOs, or correlate with server-side metrics from OXY-75. Instrumenting now prevents each new client consumer from rolling ad-hoc timers and establishes label cardinality conventions before cardinality explosion becomes a risk.

**Inferred acceptance criteria:**
1. A new metrics definition exists for the client (e.g., `modules/http/zio/src/main/scala/oxygen/http/client/ClientMetrics.scala` or `HttpClientMetrics.scala`) exposing at least a `requestDuration: Metric.Histogram[Duration]` built via `MetricBuilders.microTimer("oxygen.http.client.request.duration", ...)` (reusing `microDurations` buckets spanning micros to minutes) and optionally `requestCounter` / `errorCounter`. Labels include at minimum `api.name`/`endpoint.name` (from `RequestExtras`), `http.method`, and `oxygen.effect-result` (via `MetricBuilders.effectResult`), with optional `http.status_code` or `response.status` — following `QueryContext.metrics` tagging precedent.
2. `ZioHttpClient.send` (and/or a reusable `ZIOAspect` / `RequestMiddleware`+`ResponseMiddleware` wrapper) is instrumented so every `client.send(request, extras)` records the histogram, tagged with the request's labels, and correctly classifies success/failure/defect/interrupt (as `MetricBuilders.effectResult` does for SQL). Instrumentation is unconditional or opt-in via a config flag — consistent with how `SqlMetrics` is always on — and does not change request/response semantics.
3. Metrics are also visible for the higher-level `DerivedClientEndpointImpl` path (`modules/http/zio/src/main/scala/oxygen/http/client/generic/DerivedClientEndpointImpl.scala`) which delegates to `Client.send` — i.e., instrumenting `ZioHttpClient.send` is sufficient; no double-counting. If a separate aspect is provided, it is applied exactly once per logical call.
4. No breaking API change to `Client`, `RawClient`, or `Client.Config`; additive only. If a new `ClientMetrics` object or `ClientAspect` is introduced, existing call sites (`ViaHttpSpec`, `MultiClientSpec`, `UIMain`, example apps) continue to compile without modification.
5. Tests updated/added: a `zio-test` that exercises a client call (e.g., via `ViaHttpSpec` pattern or an in-memory `zio.http.Client` stub), asserts the histogram was updated with expected labels — either by polling `Metric` state directly or via ZIO's test metric utilities. Existing `oxygen-http` tests still pass.
6. Docs updated: brief note in `docs/docs/http/client/index.md` (currently minimal) or `agent-docs/` explaining what client metrics are emitted, what labels they carry, bucket configuration, and how to scrape them once OXY-56's exporter is enabled. Cross-link to OXY-75 for server-side parity.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title is only 4 words ("Add metrics to http-client") with no Jira body fetched, so meaning must be inferred from code context — that caps the ceiling below 5/6.
  - Code signal is strong: `modules/http/zio/src/main/scala/oxygen/http/client/ZioHttpClient.scala` is the single send path with logging + `ZIOAspect.annotated("api-name","endpoint-name")` but zero `Metric` usage (verified by grep), while the established precedent in `modules/sql/core/src/main/scala/oxygen/sql/SqlMetrics.scala` + `QueryContext.scala:29-56` + `modules/general/zio/src/main/scala/oxygen/zio/metrics/MetricBuilders.scala`/`extensions.scala` shows exactly how Oxygen instruments durations (`microTimer` → `tagged` → `toAspect`). Sibling tasks OXY-75 (server metrics), OXY-56 (exporter), and OXY-5 (spike) under Epic OXY-93 confirm this is instrumentation, not exporter plumbing.
  - Structural signal is strong: Epic **OXY-3 — oxygen-http-client** is In Progress, and the checklist groups OXY-74/75/76/77 as the HTTP observability quartet (logging + metrics × server/client). The task being Type **Task** · **Lower** aligns with small, additive instrumentation rather than a research doc or breaking refactor.
  - Remaining ambiguity keeps it at 4 not 5/6: whether the deliverable is just a duration histogram or also counters/gauges, which exact label keys to use (`api.name` vs `api-name` vs `http.route`), whether instrumentation should be always-on or opt-in via `Client.Config` flag/middleware, and dependency ordering vs. OXY-56 exporter are not stated anywhere.

## Required Changes

This is an **additive, non-breaking instrumentation task** — no schema/migration or API break expected. Concrete, repo-grounded list:

- [ ] **Create client metrics definition** (`Verified: no Metric exists in modules/http/zio/src/main/scala/oxygen/http/client/`)
  - New file `modules/http/zio/src/main/scala/oxygen/http/client/ClientMetrics.scala` (or `HttpClientMetrics.scala` — follow `SqlMetrics` naming precedent) exposing `val requestDuration: Metric.Histogram[Duration] = MetricBuilders.microTimer("oxygen.http.client.request.duration", 1.micros, 1.minute)` (tune min/max for HTTP — micros to minutes range; reuse `MetricBuilders.microDurations` buckets). Optionally add `requestCounter: Metric.Counter[Long]` and `errorCounter` if error-rate alerting is desired — to be confirmed.
  - Define canonical label keys: `http.method` (from `SendRequest.method.name`), `api.name` + `endpoint.name` (from `Client.RequestExtras`), and `oxygen.effect-result` (via `MetricBuilders.effectResult(exit)` as in `extensions.scala:10`). Optionally `http.status_code` after response is received — decide whether to tag duration with outcome status or emit a separate counter. Document low-cardinality discipline (do not use raw `path` or `queryParams` as label values).
  - Follow `modules/general/zio/src/main/scala/oxygen/zio/metrics/` conventions; no new dependencies beyond existing `zio` + `zio-metrics` already available via `oxygen-zio`.

- [ ] **Instrument the send path** (`Verified: ZioHttpClient.send is the single bottleneck for all client calls`)
  - In `modules/http/zio/src/main/scala/oxygen/http/client/ZioHttpClient.scala:14-33`, wrap `baseEffect` with the histogram aspect: `baseEffect @@ ClientMetrics.track(extras, request)` pattern mirroring `QueryContext.metrics.track(exeType)` (`QueryContext.scala:40-58`). E.g., `ClientMetrics.requestDuration.tagged(...).toAspect` or a helper `ClientMetrics.track(extras, method): ZIOAspectPoly` that tags `api.name`, `endpoint.name`, `http.method` and then delegates to `toAspect` which adds `oxygen.effect-result`.
  - Ensure duration includes only the `rawClient.client.request` call + logging, not codec encoding that happens in `DerivedClientEndpointImpl` (which calls `requestCodec.encode` before `client.send`). If codec time should be included, instrument at `DerivedClientEndpointImpl.makeOut` level instead — pick one site to avoid double counting; recommended to instrument at `ZioHttpClient.send` since it is the narrowest common path for all client variants (`ZIO`, `SSE`, `LineStream`).
  - Preserve existing `ZIOAspect.annotated("api-name", "endpoint-name")` — either merge labels with metric tags or keep both (annotation for log correlation, metric tags for aggregation). Verify annotation keys vs metric label keys are consistent or intentionally distinct.
  - Consider whether to expose instrumentation as `RequestMiddleware`/`ResponseMiddleware` composition for consumers who want to customize — but the default path should not require middleware assembly; direct `toAspect` on `baseEffect` is simplest.

- [ ] **Handle outcome classification** (`Verified: MetricBuilders.effectResult classifies Exit into success/failure/defect/interrupt`)
  - Reuse `MetricBuilders.effectResult` (or inline similar logic) so histogram is tagged with outcome, and optionally increment an error counter on non-success. Handle `Scope` resource lifecycle correctly — `ZioHttpClient.send` returns `RIO[Scope, Response]` where `Scope` manages response body finalization; ensure `timed` aspect measures until response headers received, not until body consumed (body streaming after `send` is out of scope for this timer — document this boundary).

- [ ] **Configuration / opt-in policy** (`Inferred: no config flag for metrics exists today`)
  - Decide: always-on (like `SqlMetrics`) vs. opt-in via `Client.Config` flag or `Client.layer` variant. Recommended: always-on with negligible overhead (ZIO metrics are cheap histograms), consistent with SQL. If opt-in is preferred, add a `metricsEnabled: Boolean = true` field to `Client.Config` following the `logLevel` pattern and thread it through `ZioHttpClient` — but this adds churn that Lower priority may not warrant. Confirm before implementing.

- [ ] **Tests** (`Verified: ViaHttpSpec, MultiClientSpec exist but assert no metrics`)
  - New or extended test: start a local dummy server, make a client call via the standard layer (`Client.layer.localPort` or a test `Client` built from an in-memory `zio.http.Client`), then poll the metric registry (`Metric` state via ZIO Test `TestClock`/`TestMetrics` or by reading histogram buckets) and assert `oxygen.http.client.request.duration` was recorded with expected `api.name`/`endpoint.name`/`http.method`/`oxygen.effect-result` labels. Can run without DB, as a plain `zio-test` in `modules/http/zio/src/test/scala/oxygen/http/client/`.
  - Ensure no double-counting: one request → one histogram sample.

- [ ] **Docs** (`Verified: docs/docs/http/client/index.md is minimal; no metrics section exists`)
  - Add short section to `docs/docs/http/client/index.md` or `docs/docs/metrics.md` (if OXY-56 creates it) listing emitted metrics, label keys, histogram buckets (`MetricBuilders.microDurations`), and how to enable scraping via OXY-56's `/metrics` endpoint. Cross-link to `OXY-75` server metrics for operational parity.

- [ ] **No data-model / schema / migration changes** — purely additive runtime instrumentation; backwards compatible. No `oxygen-sql` schema changes, no `oxygen-http` API break. Disabled-by-default export (OXY-56) means metrics are inert until scraped.

- **Verified vs. inferred:** The absence of any `Metric` in the client module, the exact shape of `ZioHttpClient.send` + `Client.RequestExtras` + `SendRequest`, the `SqlMetrics`/`QueryContext`/`MetricBuilders` precedent, and the checklist grouping of OXY-74/75/76/77 were verified by file reads and grep. That the metric name is `oxygen.http.client.request.duration` with `api.name`/`endpoint.name`/`http.method`/`oxygen.effect-result` labels, the histogram range, the always-on policy, and the precise instrumentation site (`ZioHttpClient.send` vs. `DerivedClientEndpointImpl`) are inferred from conventions and sibling issues — to be confirmed before implementation.

## Estimates & Autonomy

- **Story points:** 2 (Fibonacci) — small instrumentation task. If counters + comprehensive label/status handling + dedicated integration test + docs are included, 3.
  - Justification: Single new metrics object + one-site aspect wrap + test is half-day to a day of focused work. Lower priority and the narrow, additive scope support the small estimate; not an Epic and not a multi-module refactor like OXY-34/35.
- **Autonomy:** 4 / 6 — mostly autonomous with light review.
  - Justification: Scope is bounded and the `SqlMetrics`/`QueryContext` precedent gives a clear template an agent can follow without constant pairing. Needs human confirmation on label keys, histogram bounds, and always-on vs. opt-in before coding — those are one-time decisions, not ongoing pairing.
- **Ambiguity-to-resolve:** 3 / 6 — moderate; doable but benefits from 2–3 clarifications before coding.
  - Justification: Core intent ("instrument http-client with ZIO metrics") is clear, but metric name, label set (status code inclusion), bucket range, opt-in policy, and ordering vs. OXY-56 exporter are unstated and materially affect the implementation. A short note — "add `oxygen.http.client.request.duration` histogram with `api.name`/`endpoint.name`/`http.method`/`oxygen.effect-result`, always-on, at `ZioHttpClient.send`, buckets from MetricBuilders.microTimer micros→1m" — would drop this to 1.

## Open Questions

1. **Metric name & scope:** Should the histogram be named `oxygen.http.client.request.duration` (consistent with `oxygen.sql.query.duration`) or a different prefix? Is a single duration histogram sufficient, or should counters (`oxygen.http.client.requests.total`, `oxygen.http.client.errors.total`) also be added for error-rate alerting?
2. **Label cardinality:** Which labels are required? Assumed `api.name`, `endpoint.name` (from `RequestExtras`), `http.method`, `oxygen.effect-result`; should `http.status_code` (200/400/500 buckets) also be a tag on the duration histogram, or is status only for a separate counter? Must avoid high-cardinality labels like raw `path` or `queryParams`.
3. **Histogram range:** What duration range to cover? Assumed `1.micro` to `1.minute` via `MetricBuilders.microTimer` (HTTP calls are typically ms–seconds, not hours); confirm upper bound and whether `microDurations` buckets are appropriate or need a http-specific bucket set.
4. **Always-on vs. opt-in:** Should client metrics be always recorded (like SQL) or opt-in via `Client.Config(metricsEnabled: Boolean)` / `Client.layer` variant? Affects `Client.Config` shape and backwards compat.
5. **Instrumentation site:** Is `ZioHttpClient.send` the correct single site, or should instrumentation also wrap `DerivedClientEndpointImpl.makeOut` to include codec encode/decode time? Must avoid double counting if both are instrumented.
6. **Dependency ordering:** Is **OXY-56 — Add zio-metrics integration** (exporter) a prerequisite for validating client metrics externally, or can OXY-77 be implemented and verified purely via in-memory `Metric` polling in tests? Assumed the latter — to be confirmed — but local `/metrics` validation requires OXY-56.
7. **Parity with OXY-75:** Should client and server metrics share the same metric name prefix/histogram buckets/label conventions for unified dashboards, or are they intentionally distinct? Confirm with OXY-75 owner to keep `docs/docs/http/` consistent.
