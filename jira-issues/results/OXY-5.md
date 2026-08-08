# OXY-5 — Explore non-in-memory metrics options

## Original
- **Key:** OXY-5
- **Checklist line:** `- [ ] [OXY-5](https://kr-oxygen.atlassian.net/browse/OXY-5) — **Task** · Lower — Explore non-in-memory metrics options`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Explore non-in-memory metrics options
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-5
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** A research/spike task to evaluate how to export and persist metrics *outside* process memory. The current codebase uses ZIO's built-in `zio.metrics` API exclusively — `Metric.Histogram[Duration]` timers stored in ZIO's in-memory metric registry (see `modules/general/zio/src/main/scala/oxygen/zio/metrics/MetricBuilders.scala`, `modules/sql/core/src/main/scala/oxygen/sql/SqlMetrics.scala`, `modules/sql/core/src/main/scala/oxygen/sql/query/QueryContext.scala:29-56`). These metrics are only observable inside the running JVM (via ZIO's `Metric` polling / test reporters) — they have no exporter, no Prometheus scrape endpoint, no push to an external TSDB, and no OTel pipeline.

"Non-in-memory metrics options" means surveying backends, libraries, and architectures that move metrics from that in-memory registry to an external system so they survive process restarts and are queryable by ops tooling. The parent context is almost certainly the In Progress Epic **OXY-93 — oxygen-metrics** (a planned dedicated metrics module), with sibling tasks **OXY-56** ("Add zio-metrics integration"), **OXY-75** ("Add metrics to http-server"), and **OXY-77** ("Add metrics to http-client") indicating the broader initiative to make the whole stack observable.

The expected deliverable is **not code** — it is a short design doc / decision record that (a) enumerates viable backends, (b) compares them on operational and code-integration cost, and (c) recommends a path (and optionally a follow-up implementation epic breakdown). Candidate backends to evaluate based on the stack (ZIO 2.x, zio-http, no existing Otel/Prometheus dependency):

- **zio-metrics-connectors** (ZIO incubator project): drop-in backends that bridge ZIO metrics to **Prometheus** (pull via `/metrics` HTTP endpoint), **StatsD**, **Datadog**, **New Relic**, etc. This is likely the frontrunner — minimal code change (register a publisher layer), widely used in ZIO ecosystem.
- **OpenTelemetry (Otel) / zio-telemetry**: bridge ZIO metrics (and tracing, if OXY-107/108) through the Otel SDK to any Otel collector (Prometheus, Grafana Mimir, Honeycomb, etc.). Heavier but future-proof if tracing is also planned.
- **Micrometer / Dropwizard Metrics registry**: less idiomatic for ZIO, would require adapting ZIO metrics to a JVM-global registry.
- **Custom polling + push**: periodically snapshot `Metric` state and push to a time-series DB or log sink.

The doc should also address cross-cutting concerns: pull vs. push model, cardinality/label discipline (the codebase already uses `MetricLabel` heavily — `QueryContext`, `EndpointSchema.metricLabels`), histogram bucket configuration (already tuned in `MetricBuilders.microDurations`), cost of enabling metrics in production, and whether metrics should be opt-in per-service.

**Who it affects:** Platform/observability consumers of Oxygen (service operators, performance testing in OXY-55). No end-user-facing change; purely operational.

**Inferred acceptance criteria:**
1. Document exists (e.g., `agent-docs/metrics-options.md` or `docs/docs/metrics/options.md`) listing at least ~3 backends with pros/cons, integration sketch, and operational requirements (infra to run).
2. Recommendation for which backend(s) to adopt first and rough effort estimate for each.
3. Follow-up issues filed or outlined for the chosen path (so OXY-56 / OXY-75 / OXY-77 / OXY-93 can be scoped).

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title is only 5 words ("Explore non-in-memory metrics options") with no Jira body fetched — so interpretation leans heavily on the title phrase plus code context.
  - Code signal is strong: the only metrics in the repo are ZIO's in-memory `Metric` histogram timers (`SqlMetrics.queryDuration`, `QueryContext.metrics.track`), with zero exporter code and no `prometheus`/`otel`/`statsd` dependencies in `build.sbt` or `Dependencies.scala`. This makes "non-in-memory" clearly mean "export ZIO metrics externally."
  - Structural signal: Epic OXY-93 `oxygen-metrics` already exists as In Progress, and sibling To Do tasks OXY-56 (zio-metrics integration), OXY-75/77 (http metrics) show this is part of a larger observability initiative — consistent with a spike preceding implementation.
  - Remaining ambiguity keeps it at 3 not higher: the exact desired output format (short doc vs. prototype), which backends must be covered, and whether tracing/logs scope is included are not stated anywhere.

## Required Changes

This is a **research/doc task — no production code changes expected**. Concrete deliverables inferred from repo conventions:

- [ ] **Survey current state** — document existing metric instrumentation: `modules/general/zio/src/main/scala/oxygen/zio/metrics/MetricBuilders.scala` (histogram buckets), `modules/general/zio/src/main/scala/oxygen/zio/metrics/extensions.scala:7-15` (`toAspect` bridge), `modules/sql/core/src/main/scala/oxygen/sql/SqlMetrics.scala`, `modules/sql/core/src/main/scala/oxygen/sql/query/QueryContext.scala:29-56`, `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/EndpointSchema.scala` / `AppliedEndpoint.scala` (`metricLabels` TODO).
- [ ] **Add decision document** — new file, e.g. `agent-docs/metrics-options.md` or `docs/docs/metrics/non-in-memory-options.md` (follow `agent-docs/http-docs/` and `agent-docs/sql-migration/` precedent). Should cover:
  - Comparison matrix: backend, ZIO integration library, pull vs. push, infra required, license/ops cost.
  - Minimal integration sketch for top 1–2 candidates (e.g., add `"dev.zio" %% "zio-metrics-connectors-prometheus" % ...` + a `PrometheusPublisher` ZLayer + `/metrics` route).
  - Label cardinality guidance and histogram bucket reuse (`MetricBuilders.microDurations`).
  - Recommendation + next steps (which follow-up issues to file under OXY-93).
- [ ] **Optionally: tiny proof-of-concept** — if doc alone is deemed too thin, a branch that wires a Prometheus text-format endpoint behind a flag to validate the integration compiles. Not required for the "Explore" scope but could be proposed.
- [ ] **No data-model / schema / migration changes** — purely additive documentation; no backwards-compat risk.
- [ ] **Tests/docs:** No production tests needed; doc should be reviewed. If a PoC is done, a single integration test asserting `/metrics` returns Prometheus exposition format.
- **Verified vs. inferred:** Current metric call sites and the absence of any exporter dependency were verified by grep. That this task maps to OXY-93 and that the deliverable is a doc are inferred from checklist structure and the word "Explore."

## Estimates & Autonomy

- **Story points:** 2 (Fibonacci) — half-day to a day of focused reading + writing; comparable to other small research spikes. If a PoC is included, 3.
- **Autonomy:** 5 / 6 — highly autonomous. An agent with the repo and network access can enumerate options from public ZIO-metrics-connectors / OTel docs, read the existing metric code, and draft a decision doc with little human input. Review would be light.
  - Justification: Scope is bounded, no production code risk, and integration options are well-documented externally.
- **Ambiguity-to-resolve:** 3 / 6 — moderate. Core intent ("survey exporters for ZIO metrics") is clear, but format, required depth, and whether a PoC is expected are unspecified. A 2–3 sentence product clarification ("doc-only spike, cover at least Prometheus + OTel, no PoC required") would drop this to 1.
  - Justification: Title is terse; without a Jira body, the evaluator must assume deliverable shape. The recommendation target audience (who decides?) is also unstated.

## Open Questions

1. **Deliverable shape:** Is a markdown decision doc sufficient, or is a small working PoC (e.g., Prometheus `/metrics` endpoint on the example app) expected to prove the recommendation?
2. **Scope of backends:** Must the survey cover only ZIO-native options (`zio-metrics-connectors`) or also generic JVM options (Micrometer, Otel Java agent) and hosted vendors (Datadog, New Relic)?
3. **Tracing overlap:** OXY-107/OXY-108 plan `oxygen-tracing` (likely OTel-based). Should this task align the metrics recommendation with the tracing strategy (i.e., prefer OTel for both) or evaluate metrics in isolation?
4. **Pull vs. push preference:** Does the target deployment environment favor pull (Prometheus scraping) or push (StatsD/OTel collector)? This affects the recommendation.
5. **Parent Epic ownership:** Should the resulting doc live under `agent-docs/` (spike notes) or `docs/docs/` (published docs), and should follow-up implementation issues be filed as children of OXY-93?
6. **Assumption to confirm:** That "non-in-memory" refers specifically to ZIO's in-memory `zio.metrics` registry as observed in `SqlMetrics`/`QueryContext` — not to some other in-memory store (e.g., the event in-memory pipe `modules/events/in-memory`). If the latter, scope changes materially.
