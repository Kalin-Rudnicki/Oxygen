# OXY-55 — Create a framework for performance testing

## Original
- **Key:** OXY-55
- **Checklist line:** `- [ ] [OXY-55](https://kr-oxygen.atlassian.net/browse/OXY-55) — **Task** · High — Create a framework for performance testing`
- **Type:** Task
- **Priority:** High
- **Title (verbatim):** Create a framework for performance testing
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-55
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Create a reusable, cross-cutting harness for measuring and reporting performance (throughput, latency, duration) across Oxygen modules, replacing the current one-off ad-hoc approach. Today the only performance testing in the repo is `modules/sql/it-test/src/test/scala/oxygen/sql/PerformanceQuerySpec.scala` — a single `OxygenSpec[Database]` that benchmarks SQL insert strategies (sequential vs. parallel, batched vs. non-batched, `batchOptimized`) by generating `PersonCache` data, timing with `ZIO.timed`/`Clock`, and printing a markdown table via `ZIO.logImportant`. It is gated by env vars (`QPERF_PARS`, `QPERF_BATCH_SIZE`, `QPERF_NUM_BATCHES`, `QPERF_MIN_EFFICIENCY`), tagged `TestAspect.tag("performance")`, and requires `TestAspect.nondeterministic` + `TestAspect.withLiveClock` — i.e., it is explicitly opt-in, non-deterministic, and not part of normal CI. No shared abstraction, no JMH/Gatling/k6 dependency, no histogram/metric integration, and no baseline/comparison logic exist.

"Framework" implies extracting the patterns from `PerformanceQuerySpec` into a reusable facility that any module (especially `oxygen-sql` for query/batch performance and `oxygen-http` for request throughput/latency, but potentially `oxygen-json`, `oxygen-schema`, etc.) can use to define, run, and report perf workloads consistently. It likely should provide:

- A common data model for perf cases/results (name, duration total/avg, rows/sec or req/sec, histogram buckets) — similar to `PerfCase`/`PerfCase.Executed` but generalized beyond SQL inserts.
- Standard harness concerns: warmup, iteration count, parallelism control, `timed`/`MetricBuilders.microTimer` integration, env-var or config-driven enablement, `TestAspect.tag("performance")` convention, and markdown/table or JSON reporting (as `PerformanceQuerySpec` does).
- Opt-in execution that does not pollute normal `sbt test` runs (tag exclusion, separate sbt config or CI job).
- Optional integration with the metrics/metrics-options work (OXY-5, OXY-56, OXY-75/77) so perf runs can emit `Metric.Histogram[Duration]` data via `MetricBuilders`/`extensions.toAspect` and/or external export (OXY-93 `oxygen-metrics` epic).

The title has no module prefix (unlike `oxygen-sql`-specific tasks), so the framework is intended to be cross-cutting — likely living in `modules/general/test-utils` or a new `modules/general/perf` / `modules/tests/perf` module, with SQL and HTTP as first consumers. The High priority suggests it is seen as enabling work for other tasks and for validating batch/parallel optimizations before they are considered done.

**Who it affects:** Contributors and service operators needing to validate that SQL/HTTP changes do not regress throughput/latency. No end-user API change; purely developer/CI-facing. Enables data-driven decisions for batch sizing, parallelism, and future optimizations.

**Inferred acceptance criteria:**
1. A shared perf harness exists (new module or `test-utils` extension) that a spec can extend or compose to define perf cases — with standard timing, warmup, parallelism, and result rendering — so that `PerformanceQuerySpec` could be rewritten as a thin workload definition on top of it.
2. At least one example workload demonstrates the framework (e.g., SQL inserts ported to the new harness, and/or a minimal HTTP throughput example).
3. Perf specs are excluded from default `sbt test` (tagged `performance`, runnable via `sbt "testOnly -- --tags=performance"` or a dedicated sbt task/CI job) and documented (how to run, which env vars, how to read the report).
4. Documentation added (e.g., `docs/docs/perf.md` or `agent-docs/perf-framework.md` plus README note) explaining when to use the framework vs. ad-hoc timing.

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Title is only 5 words ("Create a framework for performance testing") with no Jira body fetched and no `TODO`/`FIXME`/design doc mentioning a perf framework — so intent must be inferred from title plus code context.
  - Code signal is moderate: `PerformanceQuerySpec.scala` is the sole perf test in the repo, and its ad-hoc structure (custom `PerfCase`/`Executed`, manual `timed` + `ZIO.logImportant` table, env-var gating, `performance` tag) is exactly the kind of duplication a "framework" would generalize. The *absence* of any shared harness, JMH, Gatling, or `oxygen-metrics` perf integration (verified by grep across `build.sbt`, `Dependencies.scala`, and `modules/`) confirms the gap is real and cross-cutting (no module prefix in title, CI `workflows/ci.yml` has no perf job).
  - Structural signal: sibling observability tasks (OXY-5 "Explore non-in-memory metrics options", OXY-56 `zio-metrics` integration, OXY-75/77 http metrics, epic OXY-93 `oxygen-metrics`) indicate perf/metrics is a broader initiative, but none defines what "performance testing" should cover — leaving open whether the framework is ZIO-Test-based, JMH-based, or HTTP load-testing based.
  - Remaining ambiguity keeps rating at 3 not 4+: could be interpreted as (a) generalizing the ZIO-Test `PerformanceQuerySpec` pattern (most likely), (b) adding JMH microbenchmarks for `oxygen-json`/`oxygen-schema`, or (c) adding HTTP load testing (Gatling/k6/wrk) for `oxygen-http-server`. Without a Jira body, the tool choice and scope (SQL-only vs. cross-cutting) cannot be confirmed, and alternative tooling interpretations are materially different in implementation.

## Required Changes

This is a cross-cutting test-infrastructure task — no production runtime or schema/migration changes expected. Concrete, repo-grounded list:

- [ ] **Decide location & scope** — new `modules/general/perf` or `modules/tests/perf` sbt subproject, or extension inside `modules/general/test-utils` (follow `OxygenSpec`/`OxygenAspects` precedent at `modules/general/test-utils/src/main/scala/oxygen/test/`). If a new module, add to `build.sbt` aggregates (`oxygen-modules-jvm`) and `Dependencies.scala` if external libs are needed (e.g., `zio-metrics`, JMH). Verify vs. inferred: `test-utils` location and `PerformanceQuerySpec` pattern were verified; JMH vs. ZIO-Test choice is inferred.
- [ ] **Core harness — workload/case/result model** (`Verified gap: no shared model exists`)
  - Generalize `PerformanceQuerySpec.PerfCase`/`Executed` into `PerfCase[A]` / `PerfResult` with fields: `name`, `eff`/`weight`, `totalDuration`, `avgDuration`, `throughput` (rows/sec or ops/sec), plus optional `Metric.Histogram[Duration]` integration via `oxygen.zio.metrics.MetricBuilders.microTimer` / `extensions.toAspect`.
  - Provide runner that handles: warmup iterations, measured iterations, `ZIO.timed`/`Clock`, parallelism via `mapZIOParUnordered` or `ZStream`, env-var/config gating (generalize `QPERF_*` pattern), and `TestAspect.tag("performance")` + `nondeterministic` + `withLiveClock` defaults.
  - Provide reporter that renders results as markdown table (as `PerformanceQuerySpec` does via `alignLeft`/`alignRight`/`toStringCommas`) and optionally JSON for CI artifact upload.
- [ ] **Port or demonstrate with existing workload** (`Verified: PerformanceQuerySpec` is the example to port)
  - Refactor `modules/sql/it-test/src/test/scala/oxygen/sql/PerformanceQuerySpec.scala` to use the new harness (or keep it as reference and add a new `PerfFrameworkDemoSpec`). Keep env-var compatibility (`QPERF_*`) or document replacement.
  - Add at least one additional example outside SQL (e.g., `oxygen-http` request round-trip or `oxygen-json` encode/decode) to prove cross-cutting reuse — *inferred scope for "framework"*.
- [ ] **CI / execution integration** (`Verified: .github/workflows/ci.yml` has no perf job`)
  - Ensure perf specs are excluded from default `sbt test` (tag exclusion) and add documentation/CI sketch for running them (e.g., `sbt "testOnly *Performance* -- --tags=performance"` or a dedicated `perfTest` sbt task, plus optional GitHub Actions `perf` job with artifact upload). No change to main CI required for MVP.
  - Consider optional baseline/comparison support (store previous results, fail on regression threshold) — *inferred, deferrable to follow-up*.
- [ ] **Docs** (`Verified: docs/docs/future-plans.md` is empty, `docs/docs/index.md` has no perf section)
  - Add `docs/docs/perf.md` or `agent-docs/perf-framework.md` (follow `agent-docs/http-docs/` and `agent-docs/sql-migration/` precedent) covering: what the framework is, how to define a workload, how to run (`--tags=performance` + env vars), how to read the report, and relationship to `oxygen-metrics` (OXY-5/56/93).
- [ ] **Tests/docs for the framework itself**
  - Unit test for the harness's result math/rendering (e.g., `avgDuration`, `rowsPerMinute` calculations) — does not require DB/HTTP, can run as normal test.
  - No production data-model/migration changes; purely additive test-support code. No backwards-compat risk.
- **Verified vs. inferred:** The ad-hoc nature of `PerformanceQuerySpec`, the absence of any shared perf harness/JMH/Gatling dependency, and the `performance` tag/nondeterministic/withLiveClock convention were verified by reading code and grepping `build.sbt`/`Dependencies.scala`. That "framework" means generalizing this pattern into a `test-utils`/`perf` module with warmup/parallelism/reporting and cross-module examples — and that CI/docs are in scope — is inferred from title and repo conventions.

## Estimates & Autonomy

- **Story points:** 5 (Fibonacci) — roughly 2–4 days of focused work for MVP (harness + SQL port + one cross-cutting example + docs); 8 if CI baseline-comparison/regression gating is included. High priority but not an 8+ epic-scale task unless HTTP load-testing tooling (Gatling/k6) is in scope — then 8–13.
  - Justification: Comparable to other test-infrastructure tasks; more than a single DSL feature (OXY-17 is 3) because it touches module structure, sbt config, and multiple example workloads, but less than a full new runtime if scoped to ZIO-Test generalization.
- **Autonomy:** 3 / 6 — moderately autonomous with guidance needed.
  - Justification: Agent can independently generalize `PerformanceQuerySpec`, design the harness, and write docs from repo precedent alone, but the High-priority cross-cutting scope means tooling choice (ZIO-Test vs. JMH vs. HTTP load tool) and module location affect many future consumers — so a brief human confirmation on those two decisions before coding would materially reduce rework. Without that, agent would need to assume and document the assumption.
- **Ambiguity-to-resolve:** 4 / 6 — notable open questions block start.
  - Justification: Title is 5 words with no Jira body and no codebase TODO pinning the desired tooling or scope. The four blocking design choices below must be resolved or explicitly assumed; implementation cannot be reviewed without agreeing on them. A one-paragraph clarification ("generalize PerformanceQuerySpec in test-utils, ZIO-Test-based, SQL first, HTTP later") would drop this to 1–2.

## Open Questions

1. **Tooling choice:** Should the framework be (a) ZIO-Test-based (generalizing `PerformanceQuerySpec` — lowest friction), (b) JMH/ScalaMeter microbenchmarks for hot paths (json/schema/sql codegen), or (c) HTTP load testing (Gatling/k6/wrk) for `oxygen-http-server`? These are different dependencies and have different CI needs — needs PO confirmation.
2. **Scope — SQL-only vs. cross-cutting:** Title has no module prefix, suggesting cross-cutting, but the only existing perf code is SQL. Is the MVP SQL-only (refactor `PerformanceQuerySpec`) or must it demonstrate cross-module reuse (e.g., HTTP + JSON workloads) to qualify as a "framework"?
3. **Module location:** New `modules/general/perf` (or `modules/tests/perf`) subproject vs. extension inside `modules/general/test-utils` (`oxygen.test.perf`)? Affects `build.sbt` aggregates and dependency graph — pick one and be consistent with future `oxygen-metrics` placement (OXY-93).
4. **CI integration:** Should perf tests run in CI (nightly perf job with artifact upload and regression threshold) or remain local-only (`performance` tag excluded from PR CI, run manually)? This determines whether baseline storage/comparison is in scope.
5. **Metrics integration:** Should the framework emit `Metric.Histogram[Duration]` via `MetricBuilders.microTimer`/`toAspect` and/or integrate with the planned `oxygen-metrics` exporter (OXY-5/56/93), or is `ZIO.timed` + log table sufficient for MVP?
6. **Reporting/retention:** Is markdown log output (as today) sufficient, or is structured JSON + artifact upload + historical trending required? Affects reporter design.
7. **Assumption to confirm:** That "framework for performance testing" refers to application-level throughput/latency benchmarking (as `PerformanceQuerySpec` does), not to profiling/observability infrastructure (continuous profiling, tracing) which is covered by OXY-107/108 `oxygen-tracing`.
