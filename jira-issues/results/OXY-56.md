# OXY-56 — Add zio-metrics integration

## Original
- **Key:** OXY-56
- **Checklist line:** `- [ ] [OXY-56](https://kr-oxygen.atlassian.net/browse/OXY-56) — **Task** · Lower — Add zio-metrics integration`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Add zio-metrics integration
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-56
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Wire up an external metrics exporter for the ZIO metrics already present in the codebase, so the in-memory `Metric.Histogram[Duration]` data can be scraped or pushed to an observability backend. This is the *implementation* counterpart to the research spike **OXY-5 — Explore non-in-memory metrics options** and a child of the In Progress Epic **OXY-93 — oxygen-metrics**.

Today the repo uses ZIO's built-in `zio.metrics` API exclusively — `MetricBuilders.microTimer` / `extensions.toAspect` in `modules/general/zio/src/main/scala/oxygen/zio/metrics/`, consumed by `modules/sql/core/src/main/scala/oxygen/sql/SqlMetrics.scala` and `modules/sql/core/src/main/scala/oxygen/sql/query/QueryContext.scala` (with `QueryResult.scala` applying `@@ ctx.metrics.track(...)` on every query/update). `EndpointSchema` / `AppliedEndpoint` in `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/` already allocate `MetricLabel` sets but route through no publisher. There is **no** `zio-metrics-connectors`, Prometheus, OpenTelemetry, or StatsD dependency in `project/Dependencies.scala` or `build.sbt` — verified by grep — so all metrics live only in ZIO's in-memory registry (observable in tests, invisible to ops tooling).

"Add zio-metrics integration" therefore means adding the standard ZIO ecosystem bridge — [`zio-metrics-connectors`](https://github.com/zio/zio-metrics-connectors) — that publishes the ZIO metric registry to an external system. The most common first backend is **Prometheus** (pull via a `GET /metrics` text exposition endpoint on `oxygen-http-server`); the library also supports StatsD, Datadog, and New Relic via the same publisher abstraction. An alternative that would also satisfy the title is bridging to **OpenTelemetry** via `zio-telemetry` / OTel SDK, but `zio-metrics-connectors-prometheus` is the minimal, idiomatic ZIO 2.x path and the one an agent would default to unless the OXY-5 decision doc says otherwise.

Sibling tasks clarify scope: **OXY-75** and **OXY-77** ("Add metrics to http-server/client") are about *adding metric instrumentation* to HTTP layers, while **OXY-56** is about *exporting* the metrics that already exist (and those future HTTP metrics) — i.e., the plumbing, not the per-endpoint `Metric` definitions. The deliverable lives under the planned `oxygen-metrics` module (or a shared `modules/general/metrics` shim) and is Opt-in per service.

**Who it affects:** Service operators and on-call (observability), plus performance work in OXY-55 that currently has no way to retain metric history. No direct end-user impact; purely operational.

**Inferred acceptance criteria:**
1. A new or extended module/layer (e.g., `modules/general/metrics` or `modules/general/zio` extension) that depends on `dev.zio %% zio-metrics-connectors` (and `zio-metrics-connectors-prometheus` for the default backend) and exposes a `ZLayer` / `ZIOAspect` or `PrometheusPublisher` that wires ZIO's `Metric` registry to an exporter.
2. An opt-in HTTP endpoint (e.g., `GET /metrics` on `oxygen-http-server`, or a reusable `MetricsMiddleware` / `MetricsRoutes`) serving Prometheus exposition format, disabled by default and enabled via config flag / layer composition so existing services are unaffected.
3. Wiring validated: the existing `SqlMetrics.queryDuration` histogram is visible in the scrape output with its `MetricLabel`s (`query.name`, `query.type`, `query.main-table`, `query.execution-type`, `oxygen.effect-result`) intact.
4. Docs updated — short section in `docs/docs/metrics/` or `agent-docs/` explaining how to enable the exporter, what labels/histogram buckets are emitted (`MetricBuilders.microDurations`), and how to add new metrics.
5. Existing tests still pass; a new integration test (or manual smoke) asserts `/metrics` returns Prometheus text format and contains a known metric name.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Title is only 4 words ("Add zio-metrics integration") with no Jira body fetched, so all meaning must be inferred from phrase + code context — that caps the ceiling below 5.
  - Code signal is strong: the repo already *uses* `zio.metrics` (`Metric.Histogram[Duration]`, `MetricLabel`, `MetricBuilders`, `toAspect`) in SQL/HTTP, but has zero exporter code and no `zio-metrics-connectors`/`prometheus`/`otel`/`statsd` dependency in `project/Dependencies.scala` or `build.sbt` (verified by grep). This makes "integration" almost certainly mean "add the external publisher bridge," not "add basic Metric usage" (which already exists).
  - Structural signal is strong: Epic **OXY-93 — oxygen-metrics** exists as In Progress, and the To Do list pairs this task with **OXY-5** ("Explore non-in-memory metrics options," a research spike, Lower priority) and **OXY-75/77** ("Add metrics to http-server/client"). That sequence — explore options (OXY-5) -> wire exporter (OXY-56) -> instrument HTTP (OXY-75/77) — is the standard observability rollout and matches the module dependency order.
  - Remaining ambiguity keeps it at 4 not 5/6: whether the chosen backend is Prometheus vs. OTel vs. StatsD, whether this task also includes creating the `oxygen-metrics` sbt subproject itself, and whether OXY-5's decision doc is a hard prerequisite are not stated anywhere.

## Required Changes

This is an **additive, opt-in integration** — no schema/migration or breaking API changes expected. Concrete, repo-grounded list:

- [ ] **Decide placement & dependency on OXY-5** — confirm whether OXY-5's decision doc already picks the backend. If Prometheus via `zio-metrics-connectors` is chosen (frontrunner), proceed; if OTel is preferred, the `zio-telemetry` / OTel SDK path changes the dependency and endpoint shape. Verify vs. inferred: the *absence* of any exporter dep was verified; the Prometheus frontrunner and the OXY-5 -> OXY-56 sequencing are inferred.
- [ ] **Add exporter dependency** (`Verified: no exporter dep exists today`)
  - In `project/Dependencies.scala` add a `zioMetricsConnectors` object (org `dev.zio`, artifacts `zio-metrics-connectors`, `zio-metrics-connectors-prometheus` or `-opentelemetry` if OTel), version aligned with `zio.coreVersion` `2.1.21` (e.g., `2.3.x` line — check Maven Central at implementation time).
  - In `build.sbt` wire the dependency into the owning module (either `modules/general/zio` or a new `modules/general/metrics` / `modules/metrics` subproject — see next item).
- [ ] **Create or extend metrics module** (`Verified: no dedicated metrics module exists; metrics helpers live in modules/general/zio/src/main/scala/oxygen/zio/metrics/`)
  - Option A (minimal): extend `modules/general/zio` with a `PrometheusPublisher` wrapper / `MetricsLayer` that re-exports `zio.metrics.connectors.prometheus.PrometheusPublisher` as a `ZLayer`.
  - Option B (if Epic OXY-93 intends a dedicated module): create `modules/general/metrics` (or `modules/metrics`) sbt subproject, add to `build.sbt` aggregates (`oxygen-modules-jvm`), publish settings mirroring `modules/general/zio`, and move or re-export `MetricBuilders` / `extensions` there. Follow `modules/general/test-utils` precedent for shared general modules.
  - Decision affects `build.sbt` aggregates and inter-module deps — confirm with maintainer before scaffolding a new module.
- [ ] **Publisher wiring + HTTP endpoint** (`Verified: EndpointSchema/AppliedEndpoint already expose MetricLabel sets but no publishing`)
  - Provide a `ZLayer` that registers the `PrometheusPublisher` (or OTel `MetricProvider`) against ZIO's runtime — typically `ZLayer.scoped` that starts the publisher and hooks `Metric` polling.
  - Provide a reusable HTTP route: `GET /metrics` returning Prometheus text exposition (or OTel push endpoint if that backend is chosen). For `oxygen-http-server` this is a `Route` / `Middleware` in `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/` (follow `OXY-31`/`OXY-32` docs-endpoint middleware precedent). Must be opt-in (config flag or explicit layer inclusion) so existing services don't expose metrics unintentionally.
  - Ensure histogram buckets reuse `MetricBuilders.microDurations` (already tuned for microsecond -> hour range) and that existing labels (`query.name`, `query.type`, `query.main-table`, `query.execution-type`, `oxygen.effect-result`) are preserved end-to-end.
- [ ] **Configuration** — add a small config case class (e.g., `MetricsConfig(enabled: Boolean, path: String = "/metrics", port: Option[Int])`) following `OXY-34`/`OXY-35` server/client config cleanup patterns. Wire via `zio-config` or existing config mechanism; default to disabled.
- [ ] **Tests** (`Verified: no exporter tests exist`)
  - New test: start an in-memory server with the metrics layer enabled, emit a `SqlMetrics.queryDuration` sample (or synthetic `Metric.counter`), `GET /metrics`, assert Prometheus text contains the metric name and at least one `query.*` label. Can run as a normal `zio-test` without DB.
  - Optionally extend `SqlMetrics` / `QueryContext` tests to assert metric labels are correctly tagged — low risk, but validates cardinality.
- [ ] **Docs** (`Verified: docs/docs/` has no metrics section; `docs/docs/future-plans.md` is empty`)
  - Add `docs/docs/metrics.md` or `docs/docs/metrics/integration.md` (follow `docs/docs/http/` structure) covering: what `zio-metrics-connectors` does, which backend is wired, how to enable `GET /metrics`, label cardinality guidance, histogram bucket reference, and how to add a new `Metric` (example using `MetricBuilders.microTimer` + `toAspect`).
  - Cross-link from `OXY-5` decision doc if it exists.
- [ ] **No data-model / schema / migration changes** — purely additive runtime + HTTP layer; backwards compatible (disabled by default). No `oxygen-sql` schema changes.
- **Verified vs. inferred:** The existing `Metric` call sites, the absence of any exporter dependency/metrics module, and the HTTP `metricLabels` placeholders were verified by grep and file listing. That "integration" means `zio-metrics-connectors` + Prometheus `/metrics` (vs. OTel, vs. just more instrumentation) and the new-module vs. extend-existing-module choice are inferred from ZIO ecosystem conventions and the OXY-5/56/75/77/93 checklist structure.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — small feature. If a new `modules/general/metrics` subproject is scaffolded with docs + integration test, 5.
  - Justification: Dependency + layer + route is ~half-day to a day of focused work; the larger estimate covers module scaffolding, config, and cross-checking against OXY-5's recommendation. Lower priority (Lower) aligns with this being non-urgent enabling work, not a production bug.
- **Autonomy:** 4 / 6 — mostly autonomous with light review.
  - Justification: Scope is bounded and the ZIO connectors integration is well-documented externally; an agent with the repo + network can implement the dependency, layer, and `/metrics` route without constant pairing. Needs human confirmation on (a) backend choice (Prometheus vs. OTel) if OXY-5 hasn't decided, (b) whether to create a new sbt subproject, and (c) the config/enablement policy — those are one-time decisions, not ongoing pairing.
- **Ambiguity-to-resolve:** 3 / 6 — moderate; doable but benefits from 2–3 clarifications before coding.
  - Justification: Core intent ("wire exporter for existing ZIO metrics") is clear, but backend preference, module placement, and opt-in vs. always-on policy are unstated and materially affect the implementation. A short product note — "use zio-metrics-connectors-prometheus, extend modules/general/zio, opt-in via MetricsConfig, depend on OXY-5 doc if available" — would drop this to 1.

## Open Questions

1. **Backend choice:** Should this wire **Prometheus** (`zio-metrics-connectors-prometheus`, pull `/metrics`) or **OpenTelemetry** (OTel SDK via `zio-telemetry`, push to collector)? If OXY-5 already chose, this task should follow that decision — otherwise Prometheus is the assumed default.
2. **Prerequisite on OXY-5:** Is OXY-5 ("Explore non-in-memory metrics options") a hard prerequisite, or can OXY-56 proceed with the frontrunner (Prometheus) and let OXY-5 run in parallel? Affects sequencing under Epic OXY-93.
3. **Module placement:** Extend `modules/general/zio` (minimal, no new sbt project) or create a dedicated `modules/general/metrics` / `modules/metrics` subproject under OXY-93? The latter is cleaner for Epic ownership but adds `build.sbt` aggregate churn — pick one and be consistent with future `oxygen-metrics` placement.
4. **Enablement policy:** Opt-in per service via explicit `MetricsLayer` / `MetricsConfig(enabled = false)` default, or always on with a flag to disable? Affects default security posture of `GET /metrics`.
5. **Scope vs. OXY-75/77:** Does OXY-56 also add HTTP metric instrumentation (request duration histograms in `oxygen-http-server`/`oxygen-http-client`), or is HTTP instrumentation strictly OXY-75/77 and OXY-56 is only the exporter plumbing? Assumed the latter — to be confirmed.
6. **Cardinality / label discipline:** High-cardinality labels like `query.name` and `query.main-table` are already in use (see `QueryContext.defaultMetricLabels`). Should the exporter add cardinality guardrails (allowlist, sanitization) at publish time, or is that deferred?
7. **Assumption to confirm:** That "zio-metrics integration" refers to exporting ZIO's `zio.metrics` registry via `zio-metrics-connectors` — not to migrating from another metrics library (e.g., Micrometer/Dropwizard) to `zio.metrics`, and not to adding more `Metric` definitions (which is OXY-75/77's job). If the latter, scope shifts to per-module instrumentation.
