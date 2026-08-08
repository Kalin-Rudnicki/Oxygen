# OXY-96 — Implement prepared statement caching

## Original
- **Key:** OXY-96
- **Checklist line:** `- [ ] [OXY-96](https://kr-oxygen.atlassian.net/browse/OXY-96) — **Task** · Lower — Implement prepared statement caching`
- **Type:** Task
- **Priority:** Lower
- **Title (verbatim):** Implement prepared statement caching
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-96
- **Checklist section:** To Do
- **Epic filter:** Likely child of **OXY-1 — Epic · oxygen-sql** (the only sql epic in progress; all query/connection work in `modules/sql` falls under it). No explicit epic link in `checklist.md` title — association inferred from module. Closest siblings: OXY-94 (`on conflict` DSL), OXY-97 (explicit FK/IDX naming), OXY-98 (`lateral join`/`union all`), OXY-100 (`group by`).

## Expanded Description

**What this likely means:** Add a cache so repeated executions of the same SQL do not pay the full `Connection.prepareStatement(sql)` / PostgreSQL `Parse` cost every time. The oxygen-sql stack currently has **zero** prepared-statement caching anywhere.

Evidence from the codebase (verified):

- `modules/sql/core/src/main/scala/oxygen/sql/query/PreparedStatement.scala:195-204` — `PreparedStatement.prepare(ctx, fetchSize)` does for **every** execution: `database.getConnectionAndType` → `con.connection.prepareStatement(ctx.sql)` → `withFinalizer { rawPS.close() }`. The `rawPS` is closed at `Scope` exit. No lookup/store.
- `modules/sql/core/src/main/scala/oxygen/sql/query/query.scala:40,75,91,107,120,219,354` — every `Query` / `QueryI` / `QueryO` / `QueryIO` path calls `PreparedStatement.prepare` anew; no caller caches.
- `modules/sql/core/src/main/scala/oxygen/sql/Driver.scala:13-33` — `JdbcDriver` builds `java.util.Properties` with only `user`/`password`; no PostgreSQL performance properties (`prepareThreshold`, `preparedStatementCacheQueries`, `preparedStatementCacheSizeMiB`, `preferQueryMode`) are set.
- `modules/sql/core/src/main/scala/oxygen/sql/ConnectionPool.scala` — `ZPool[ConnectionError, Connection]` pools raw `java.sql.Connection`s; `Connection.scala:7-28` wraps `java.sql.Connection` without a statement cache.
- `modules/sql/core/src/main/scala/oxygen/sql/DbConfig.scala` — `DbConfig.Pool/Execution/Logging` exist but there is no `PreparedStatementCache` or `JdbcOptions` config.

**Why it matters:** For OLTP workloads where the same 10–50 query shapes are executed thousands of times (typical for Oxygen services), the current path pays per-execution JDBC and PG server parse overhead plus extra GC from allocating `PreparedStatement`/`ResultSet`. Caching keeps the parsed/planned server statement alive and reuses the client handle. This complements the batch optimizations (`BatchOptimizedInsert`) and streaming reads (`bufferChunkSize`/`bufferNumChunks`) already tuned in `DbConfig.Execution`.

The intended cache is almost certainly **PostgreSQL JDBC prepared-statement caching**, with two plausible layers that are not mutually exclusive:

1. **Driver-level (primary, idiomatic for PG):** Configure `org.postgresql.Driver` via connection properties — `preparedStatementCacheQueries` (default 256), `preparedStatementCacheSizeMiB` (default 5), `prepareThreshold` (default 5, after N executions the driver promotes to a server-prepared `PREPARE`/`EXECUTE`), and optionally `preferQueryMode=extendedCacheEverything`. This is per-`java.sql.Connection` and is handled entirely by `pgjdbc` (see `org.postgresql:postgresql:42.7.5` in `project/Dependencies.scala:46-53`).
2. **Application-level (possible stretch goal):** A small Oxygen-owned LRU keyed by `ctx.sql` (or `QueryContext` hash) that holds `java.sql.PreparedStatement` references per pooled `Connection` (wrapping `Connection` with a `Ref[Map[String, PreparedStatement]]` or using Caffeine/ZIO Cache). This gives explicit control over eviction and ties lifetime to `ZPool` Scope finalizers, but must handle invalidation on connection close, transaction/savepoint boundaries, and schema changes.

Priority `Lower` suggests this is a performance optimization, not a correctness blocker — desirable once the query DSL and migration compat work (OXY-8, OXY-123) stabilize.

**Who it affects:** All oxygen-sql users via `Database`/`Query*` APIs; most visible in high-QPS services and in `PerformanceQuerySpec`-style workloads. Invisible to call-site code if done via driver properties; small API surface if an explicit config is added.

**Inferred acceptance criteria:**

1. Repeated executions of the same `ctx.sql` against the same pooled connection reuse the prepared statement (observable via `pg_prepared_statements` or pgjdbc debug, or a benchmark showing reduced p50 prepare latency).
2. Cache is **configurable** (enabled by default, size/threshold tunable via `DbConfig`) and **bounded** (LRU + `SizeMiB` cap) to avoid unbounded memory.
3. Cache is correctly scoped to the underlying `java.sql.Connection` — a statement is never reused across connections, and is cleared when a pooled connection is closed/invalidated or when the `Scope` exits.
4. Works correctly under `Atomically` transactions/savepoints (`Database.ConnectionState.InTransaction`/`InSavepoint`) — does not leak a prepared statement across a rollback that would invalidate server state unexpectedly. At minimum, driver-level cache handles this transparently; if app-level, document the guarantee.
5. Existing `ZIO[Database, QueryError, _]` / `ZStream` query APIs are unchanged (backwards compatible); only config grows.
6. A before/after benchmark or at least a unit test proving cache hit/miss behavior is added; no regression in `modules/sql/it-test` ( `CustomQuerySpec`, `TableCompanionQuerySpec`, `IsolationAspectSpec`).

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - **Strong code signal:** Grep for `prepareStatement`/`PreparedStatement` hits only the uncached creation path (`PreparedStatement.scala:202`, `query.scala:*`); grep for `cache`/`Hikari`/`pool` shows no statement-caching code or TODO mentioning it. The absence is itself strong evidence that the title describes exactly what is missing.
  - **Title is unambiguous:** "Implement prepared statement caching" has a single conventional meaning in a JDBC/PostgreSQL context — reuse `PreparedStatement`s for the same SQL — unlike vague titles (e.g., "Explore options"). The module is inferable as `oxygen-sql` from the SQL/JDBC context even though the title omits the module prefix.
  - **Dependency context fits:** `org.postgresql:postgresql:42.7.5` is the driver; its well-documented `preparedStatementCacheQueries` / `prepareThreshold` properties are the textbook solution, so the intended design is predictable.
  - **Remaining ambiguity lowers from 5/6:** No Jira body was fetched and no skipped test/TODO names the desired layer (driver-only vs. app-level LRU vs. both) or the exact config shape (new `DbConfig.PreparedStatementCache` vs. generic `jdbcProperties: Map`). That one design choice is still inferred, keeping it at 4 not 5/6.

## Required Changes

> Concrete, repo-grounded list. Checked = verified to need change; unchecked = inferred/nice-to-have. All paths assume deeper analysis gate (confidence 4 ≥ 3) was passed.

- [ ] **Decide caching layer** — Driver-level pgjdbc cache is the minimal correct fix; application-level LRU is optional/additive. Recommend implementing driver-level first, with an app-level wrapper only if benchmarks show the driver cache is insufficient (e.g., we want cross-connection logical caching or explicit eviction control). Document the decision in the PR description.
- [ ] **`modules/sql/core/src/main/scala/oxygen/sql/DbConfig.scala`** — add configuration for the cache. Minimal shape:
  ```scala
  final case class PreparedStatementCache(
    enabled: Boolean = true,
    cacheQueries: Int = 256,
    cacheSizeMiB: Int = 5,
    prepareThreshold: Int = 5,        // pgjdbc default; 1 = prepare on first execution
    preferQueryMode: Option[String] = None // e.g. Some("extendedCacheEverything")
  )
  ```
  Wire into `DbConfig` (e.g., `jdbc: DbConfig.Jdbc = ...` or directly `preparedStatementCache: PreparedStatementCache`). Derives `JsonSchema` for config JSON support, with defaults matching pgjdbc so existing deploys are unaffected. Verified: `DbConfig` already `derives JsonSchema` and is the canonical place for pool/execution tuning.
- [ ] **`modules/sql/core/src/main/scala/oxygen/sql/Driver.scala`** — propagate the new config into `java.util.Properties` before `driver.connect(...)`. E.g.:
  ```
  props.put("preparedStatementCacheQueries", cache.cacheQueries.toString)
  props.put("preparedStatementCacheSizeMiB", cache.cacheSizeMiB.toString)
  props.put("prepareThreshold", cache.prepareThreshold.toString)
  ```
  Guard with `enabled` (set `cacheQueries=0` to disable). Alternatively, disable via `prepareThreshold=0` per pgjdbc docs. Verify pgjdbc property names against `42.7.5` javadoc — names above are current.
- [ ] **`modules/sql/core/src/main/scala/oxygen/sql/ConnectionPool.scala` / `Connection.scala`** — no functional change if driver-level only. If app-level cache is chosen, extend `Connection` to carry a per-connection `Ref[Map[String, java.sql.PreparedStatement]]` (or a dedicated `PreparedStatementCache` service scoped to the connection's `Scope`), and ensure `pool.invalidate(con)` clears that map. The `getConnectionLoop` retry already handles closed connections; cached statements for an invalidated connection must be dropped.
- [ ] **`modules/sql/core/src/main/scala/oxygen/sql/query/PreparedStatement.scala`** — if app-level cache is chosen, change `prepare` to:
  1. Lookup `ctx.sql` in the per-connection cache (keyed by `ctx.sql`; consider also including `fetchSize` since `setFetchSize` is per-statement).
  2. On hit, return cached `PreparedStatement` after clearing previous bindings (`clearParameters`, `clearBatch`) and resetting `fetchSize`.
  3. On miss, create via `con.connection.prepareStatement(ctx.sql)` and cache with a finalizer that removes from map + closes on eviction/connection close.
  If driver-level only, this file needs **no** code change — the driver handles caching transparently — but add a comment referencing `DbConfig.PreparedStatementCache` so future readers know why it looks uncached.
- [ ] **`modules/sql/core/src/main/scala/oxygen/sql/Database.scala`** — thread the new config through `Database.make` / `Database.layer` / `ConnectionPool.makeZPool` so `Driver.GetConnection` sees it. Current wiring is `DbConfig → Target + Credentials → Driver.GetConnection → ConnectionPool → Database`; the cache config must be added as a layer input alongside `DbConfig.Pool`.
- [ ] **(Optional, if app-level)** New file `modules/sql/core/src/main/scala/oxygen/sql/query/PreparedStatementCache.scala` — isolated LRU (e.g., `zio.Cache` or `com.github.ben-manes.caffeine` if already on classpath; otherwise a simple `Ref` + `LinkedHashMap` bounded by `cacheQueries` / `cacheSizeMiB`). Keep it `private[sql]` and scoped to `Connection`.
- [ ] **Tests — `modules/sql/it-test/src/test/scala/oxygen/sql/`** — add `PreparedStatementCacheSpec` (or extend `CustomQuerySpec`):
  - Prove cache hit: execute same `Query` N times and assert second execution reuses (via a test-only counter or by inspecting `pg_prepared_statements`, or by mocking `Connection.prepareStatement` call count with a wrapper).
  - Prove eviction/boundedness: fill cache beyond `cacheQueries` and assert oldest is evicted.
  - Prove transaction safety: prepare inside `atomically`, rollback, and verify next execution still succeeds.
  - Prove `Scope` cleanup: statement is closed when connection is returned to pool / `Scope` closes.
  - Existing it-tests must still pass (`sbt "oxygen-sql-it-test/test"` or `testOnly *TableCompanionQuerySpec` etc.).
- [ ] **Docs — `docs/docs/sql/database.md` (+ optionally `docs/docs/sql/queries.md`)** — document the new `DbConfig.PreparedStatementCache` JSON shape, defaults, tuning guidance (when to raise `cacheQueries` vs. `prepareThreshold=1` for OLTP), and that it is per-connection. Follow existing `DbConfig` JSON example style.
- [ ] **Benchmark (optional, Lower priority)** — extend `PerformanceQuerySpec` or add a JMH micro-benchmark comparing p50/p99 for 10k executions of the same query with cache disabled vs. enabled. Not strictly required for acceptance but valuable for a perf task.
- [ ] **Backwards compatibility** — defaults must preserve current behavior for callers who supply no config (i.e., `enabled=true` with pgjdbc defaults is safe; if we want strict BC, default to disabled and require opt-in — call out the choice). JSON schema derivation must handle missing `preparedStatementCache` field via default.

- **Verified vs. inferred:** The absence of any caching code and the exact `prepareStatement` call sites were verified by grep/read. That the cache should be driver-level pgjdbc config is inferred from PostgreSQL/JDBC conventions and the `postgresql:42.7.5` dependency; an alternative app-level interpretation is plausible but less likely for a first implementation. The per-connection scoping requirement is inferred from JDBC spec (`PreparedStatement` is bound to its `Connection`).

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — small-to-medium task.
  - Justification: Driver-level fix is ~20–40 lines of config plumbing + properties wiring + 1 test file; well-bounded. Rises to **5** if a full app-level LRU with `Ref`/eviction and mocked `Connection` tests is required, or if JMH benchmarking is included.
- **Autonomy:** 4 / 6 — mostly autonomous, needs one design confirmation.
  - Justification: An agent with the repo can implement the driver-level path end-to-end (add `DbConfig` case class, wire through `Driver`, add tests, update docs) without human pairing. The only blocking decision is "driver-only vs. also app-level" — if the reviewer expects app-level, the agent would need to extend scope. With that one question answered, it is 5–6.
- **Ambiguity-to-resolve:** 3 / 6 — moderate; a few questions must be answered before coding to avoid rework.
  - Justification: Cache layer (driver vs. app), default enabled/disabled, exact config placement (`DbConfig.PreparedStatementCache` vs. generic `jdbcProperties`), and eviction semantics are not in the title. All are answerable with 2–3 sentences from the owner. Once clarified, ambiguity drops to 1.

## Open Questions

1. **Desired layer — driver-only or also app-level?** Is configuring pgjdbc's `preparedStatementCacheQueries` / `prepareThreshold` sufficient, or is an Oxygen-owned LRU of `java.sql.PreparedStatement` per pooled `Connection` expected? The former is ~1 day, the latter is ~2–3 days and touches `PreparedStatement.scala` directly.
2. **Defaults — opt-in or opt-out?** Should the cache be enabled by default (matching pgjdbc defaults: 256 queries / 5 MiB / threshold 5) or disabled by default for strict BC? For Lower-priority perf tasks, enabled-by-default is usually preferred — but confirm.
3. **Config shape — typed case class or open map?** Is a typed `DbConfig.PreparedStatementCache(cacheQueries, cacheSizeMiB, prepareThreshold)` desired, or a generic `jdbcProperties: Map[String, String]` passthrough to `Driver.JdbcDriver`? Typed is more discoverable; map is more future-proof for other PG properties (`preferQueryMode`, `autosave`, etc.).
4. **Cache key granularity:** Is `ctx.sql` (the rendered SQL string) the sole key, or should `fetchSize` / `QueryType` / `constParams` also partition the cache? Different `fetchSize` values use the same `PreparedStatement` but require `setFetchSize` per execution.
5. **Invalidation semantics:** On `Atomically` rollback/savepoint, should cached statements created inside the transaction be invalidated, or is driver-level handling trusted? If app-level, we need to decide whether to clear the per-connection cache on `InTransaction`/`InSavepoint` exit.
6. **Observability:** Should cache hit/miss be exposed via `SqlMetrics` / ZIO metrics (counter `sql.preparedStatement.cache.hit` / `.miss` + gauge for size), or is silent operation acceptable for this iteration? Related to OXY-55 (perf testing) and OXY-93 (oxygen-metrics).
7. **Assumption to confirm:** That "prepared statement caching" refers to JDBC-level `java.sql.PreparedStatement` reuse for repeated `ctx.sql` on PostgreSQL (as implemented in `PreparedStatement.scala`), not to caching of the Oxygen query compilation/macro output (`CompileMacros` / `GeneratedSql`). If the latter (macro plan cache) was intended, scope shifts to `modules/sql/core/src/main/scala/oxygen/sql/generic/generation/`.
