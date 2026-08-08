# Oxygen Jira Triage — Full Picture & Recommendations

**Date:** 2026-08-08  
**Scope:** 79 issues in `jira-issues/checklist.md` — 16 Epics excluded per briefing, **63 non-epic issues triaged** (55 Task, 3 Documentation, 2 Bug, 2 Subtask, 1 Architecture).  
**Artifacts:** `jira-issues/results/OXY-#.md` (one per issue, 6-section workflow) + `jira-issues/checklist.md` (63 `[x]` / 16 `[ ]`).  
**Method:** §4 7-step workflow — port `Original`, read code, write `Expanded Description` + Confidence 1–6, gate deeper analysis at ≥3, add Required Changes / Story Points / Autonomy 1–6 / Ambiguity 1–6 / Open Questions, flip checklist, brief return. Max 4 sub-agents at a time; central repair for concurrent checklist races.

---

## 1. Executive Summary

* **Total effort if done serially: ~245 story points** (avg 3.9 pts/issue). Largest single item is `OXY-98` (13 pts, lateral join/union/sparse/zstream aggregate); the rest are 1–8 pts, with the bulk at 3 or 5 pts.
* **Signal is decent but not crisp:** 37/63 at Confidence 4 (good evidence, one frontrunner), 18 at 3 (plausible, more likely than not), only 8 at 5 (strong). No issue hit 6 (near-certain) — no Jira bodies or explicit specs were available; every interpretation leaned on title + code grep.
* **Most work is autonomously doable — with clarification:** 14 issues score Autonomy 5 (highly autonomous) and 1 scores 6 (fully autonomous, `OXY-109` one-line CSS fix). 46 issues are at 3–4 (moderate autonomy, needs a brief spec/UX decision). Only 2 need constant pairing (Autonomy 2: `OXY-129` grammar re-shuffling, `OXY-132` sockets client prototype — both have zero codebase signal).
* **Ambiguity is the bottleneck, not confidence:** 22 at Ambiguity 3 (moderate), 20 at 4, 12 at 5, 1 at 6 (`OXY-81` docs). 13 issues with Ambiguity 5–6 should not start without a 2–3 sentence product clarification — they will thrash otherwise.
* **Module skew:** `sql` dominates (20 issues), then `http` (15), `ui` (8). `meta` (5) and `transform` (5) are the remaining clusters. `sql` + `http` alone are 55% of backlog and contain the highest-priority items (`OXY-8,17,70,72,98`).

**Bottom line:** You can start immediately on ~30 “ready” issues (Confidence ≥4, Ambiguity ≤3, Autonomy ≥4). Another ~20 need a one-paragraph spec before sprinting. The final ~13 are spikes or green-field prototypes that need a design decision first.

---

## 2. Aggregate Metrics

### Confidence (do we know what it means?)

| Rating | Meaning | Count |
|--------|---------|-------|
| 5 | strong evidence (code TODO + docs + siblings align) | 8 |
| 4 | good evidence, one clear frontrunner | 37 |
| 3 | plausible / more likely than not (threshold) | 18 |
| 1–2 | weak/wild guess — *none* | 0 |

All 63 passed the ≥3 gate, so every file has Required Changes + Estimates. No issue was so vague it was untriagable — a credit to how much code signal exists.

### Story Points (Fibonacci 1,2,3,5,8,13)

| Points | Count | Notes |
|--------|-------|-------|
| 5 | 27 | Standard mid-size feature (new DSL clause, new endpoint middleware, new prototype) |
| 3 | 18 | Small feature or focused fix |
| 2 | 12 | Docs, small wiring, or single-file change |
| 1 | 3 | Trivial bugs (`OXY-109` wrap, `OXY-8` compat gate, `OXY-92` subtask) |
| 8 | 2 | `OXY-7` migration rollback, `OXY-128` Slyce impl |
| 13 | 1 | `OXY-98` — four Postgres/stream features bundled |

**Total 245 pts.** At ~20 pts/sprint/agent, that is ~12 sprints for one agent, or ~3 sprints for a 4-agent parallel team (the triage parallelism you used).

### Autonomy (can an agent do it alone with this briefing + repo?)

| Rating | Meaning | Count |
|--------|---------|-------|
| 6 | fully autonomous | 1 (`OXY-109` CSS) |
| 5 | highly autonomous | 14 |
| 4 | mostly autonomous, occasional check-in | 21 |
| 3 | needs brief kickoff + one review | 25 |
| 2 | needs pairing throughout | 2 (`OXY-129`, `OXY-132`) |

### Ambiguity to Resolve Before Starting

| Rating | Meaning | Count |
|--------|---------|-------|
| 1 | ready to start | 2 |
| 2 | trivial open question | 6 |
| 3 | moderate — 1-paragraph spec drops it | 22 |
| 4 | significant — needs product/UX decision | 20 |
| 5 | major — blocks start | 12 |
| 6 | blocking design question | 1 (`OXY-81`) |

### Priority / Type

| Priority | Count | Type | Count |
|----------|-------|------|-------|
| Higher | 11 | Task | 55 |
| High | 10 | Documentation | 3 |
| Normal | 18 | Bug | 2 |
| Low | 11 | Subtask | 2 |
| Lower | 10 | Architecture | 1 |
| Lowest | 3 | Epic | 0 (excluded) |

### Module Cluster (inferred from title + code search)

| Cluster | Count | Epic Parent(s) |
|---------|-------|----------------|
| `oxygen-sql` | 20 | `OXY-1` (In Progress) |
| `oxygen-http` (server/client) | 15 | `OXY-2`, `OXY-3` |
| `oxygen-ui` / `oxygen-ui-rework` | 8 | `OXY-83`, `OXY-133` |
| `oxygen-meta` | 5 | `OXY-118` |
| `oxygen-transform` | 5 | `OXY-87` |
| other (`cli`, `events`, `crypto`, `tracing`, `sockets`, `slyce`) | 10 | `OXY-101,107,111,114,130,127` |

---

## 3. Full Inventory — 63 Issues

| Key | Conf | SP | Aut | Amb | Pri | Type | Title |
|-----|------|----|-----|-----|-----|------|-------|
| OXY-5 | 3 | 2 | 5 | 5 | Lower | Task | Explore non-in-memory metrics options |
| OXY-6 | 4 | 5 | 3 | 4 | Low | Task | Add support for array input + unnest |
| OXY-7 | 4 | 8 | 3 | 4 | Low | Task | Support migration rollback |
| OXY-8 | 5 | 1 | 4 | 3 | High | Task | Add compatibility checking to db migrations |
| OXY-13 | 4 | 5 | 3 | 3 | Low | Task | Support automatic join clauses |
| OXY-14 | 3 | 5 | 3 | 4 | Normal | Task | Add support for querying into a JSONB structure |
| OXY-15 | 3 | 5 | 3 | 3 | Low | Task | Add support for running an effect as a db migration step |
| OXY-17 | 4 | 3 | 3 | 5 | High | Task | Add support for IN |
| OXY-29 | 4 | 5 | 4 | 3 | Higher | Task | Create compatibility spec for HTTP Schema |
| OXY-31 | 5 | 2 | 5 | 5 | Higher | Task | Create simple `/docs.json` endpoint middleware |
| OXY-32 | 5 | 3 | 5 | 2 | High | Task | Create basic `/docs` html endpoint middleware |
| OXY-34 | 3 | 3 | 3 | 4 | Normal | Task | Clean up server configuration paradigm |
| OXY-35 | 3 | 3 | 3 | 4 | Normal | Task | Clean up client configuration paradigm |
| OXY-38 | 4 | 5 | 3 | 3 | Higher | Task | Add compatibility checking for endpoint schema |
| OXY-53 | 4 | 3 | 4 | 3 | Low | Task | Add `Lens` typeclass |
| OXY-55 | 3 | 5 | 3 | 3 | High | Task | Create a framework for performance testing |
| OXY-56 | 4 | 3 | 4 | 5 | Lower | Task | Add zio-metrics integration |
| OXY-60 | 4 | 5 | 3 | 3 | Low | Task | Support json parsing and decoding at the same time |
| OXY-70 | 4 | 5 | 3 | 3 | High | Task | Improve representation and differentiation between Option/Specified in json schema |
| OXY-72 | 4 | 5 | 4 | 3 | High | Task | Add schema representation for Tuple |
| OXY-74 | 4 | 2 | 5 | 4 | Low | Task | Add logging to http-server |
| OXY-75 | 4 | 3 | 4 | 3 | Lower | Task | Add metrics to http-server |
| OXY-76 | 4 | 2 | 4 | 4 | Low | Task | Add logging to http-client |
| OXY-77 | 4 | 2 | 4 | 5 | Lower | Task | Add metrics to http-client |
| OXY-79 | 4 | 2 | 4 | 3 | Lowest | Task | Add easy HTTPS configuration to server |
| OXY-80 | 4 | 2 | 4 | 3 | Lowest | Task | Add easy HTTPS config to client |
| OXY-81 | 5 | 5 | 5 | 6 | High | Documentation | Add docs for oxygen-http |
| OXY-82 | 3 | 3 | 4 | 3 | High | Documentation | Create video for oxygen-http |
| OXY-88 | 3 | 5 | 3 | 5 | Lower | Task | Add support for partial transformer |
| OXY-89 | 3 | 5 | 3 | 4 | Lower | Task | Add support for configurable transformer |
| OXY-90 | 4 | 2 | 4 | 4 | Low | Architecture | Figure out if any form of full-auto transform makes sense |
| OXY-91 | 4 | 2 | 5 | 5 | Normal | Subtask | Figure out if any form of full-auto transform makes sense : Create Document |
| OXY-92 | 4 | 1 | 5 | 5 | Normal | Subtask | Figure out if any form of full-auto transform makes sense : Create Issues |
| OXY-94 | 4 | 3 | 3 | 4 | Lower | Task | Add `on conflict` support to query dsl |
| OXY-96 | 4 | 3 | 4 | 5 | Lower | Task | Implement prepared statement caching |
| OXY-97 | 5 | 3 | 5 | 2 | Lower | Task | Support explicitly naming FK + IDX |
| OXY-98 | 4 | 13 | 3 | 5 | Higher | Task | Add support for `lateral join` + `union all` + `sparse data` + `zstream agg` in order to query nested data structures |
| OXY-100 | 4 | 5 | 3 | 4 | Lowest | Task | Add query support for `group by` |
| OXY-102 | 3 | 3 | 5 | 3 | Low | Task | Improve parse error messages in CLI |
| OXY-108 | 4 | 5 | 4 | 4 | Normal | Task | Create initial implementation of oxygen-tracing |
| OXY-109 | 5 | 1 | 6 | 1 | Higher | Bug | Fix long text in page message not wrapping |
| OXY-110 | 3 | 5 | 3 | 4 | High | Task | Create more featureful page messages, open dialog, etc |
| OXY-112 | 4 | 2 | 5 | 2 | Low | Task | Add helper for Basic auth |
| OXY-113 | 4 | 3 | 5 | 2 | Lower | Documentation | Add docs for oxygen-sql |
| OXY-116 | 4 | 3 | 5 | 2 | Higher | Task | Add real client error handlers for provided models |
| OXY-117 | 4 | 3 | 4 | 3 | Higher | Task | Add real server error handlers for provided models |
| OXY-119 | 5 | 3 | 5 | 1 | Higher | Task | Support type-class derivation of sum types with type param(s) — *note: code suggests already landed in `f88bbbd4`* |
| OXY-120 | 3 | 5 | 3 | 4 | Normal | Task | Add helpers for creating classes using macros |
| OXY-121 | 3 | 5 | 3 | 4 | Normal | Task | Add better support for treating unions as sum types |
| OXY-123 | 4 | 5 | 3 | 3 | Higher | Task | Add compatibility spec for DB Schema |
| OXY-128 | 4 | 8 | 3 | 4 | High | Task | Get initial initial macro-based `Sylce` impl working |
| OXY-129 | 3 | 5 | 2 | 5 | Normal | Task | Add support for grammar re-shuffling to prevent conflicts |
| OXY-131 | 3 | 5 | 3 | 4 | Normal | Task | Create initial `oxygen-sockets` server prototype |
| OXY-132 | 3 | 5 | 2 | 5 | Normal | Task | Create initial `oxygen-sockets` client prototype |
| OXY-143 | 3 | 5 | 4 | 3 | Higher | Task | Create a service for managing config files |
| OXY-145 | 4 | 5 | 4 | 3 | Normal | Task | UI Rework - Resizable panels |
| OXY-146 | 5 | 2 | 5 | 2 | Higher | Bug | Pulsar `createIfDNE` throws random error - Topic already exists |
| OXY-147 | 4 | 2 | 4 | 3 | Normal | Task | Add `JsonEncoder.Omit` typeclass and `@jsonOmit` annotation omit |
| OXY-149 | 3 | 3 | 4 | 4 | Normal | Task | Create separate `Int` JsonType (json + schema) |
| OXY-151 | 4 | 5 | 3 | 4 | Normal | Task | Add better support for mobile vs desktop differences |
| OXY-152 | 4 | 5 | 3 | 4 | Normal | Task | Add dropdown menu support to TopBar |
| OXY-153 | 4 | 5 | 4 | 3 | Normal | Task | Actually implement FE IndexedDB |
| OXY-154 | 4 | 3 | 4 | 3 | Normal | Task | Add a reusable helper for color theme |

*All 63 have full `Required Changes`, `Estimates & Autonomy`, `Open Questions` because Confidence ≥3. See per-issue markdowns for file-level change lists.*

---

## 4. Themes by Module — Where the Work Clusters

### `oxygen-sql` (20 issues, ~72 pts, Epic OXY-1 In Progress)
The DSL is the most active surface. Missing predicates/verbs (`IN`, `group by`, `lateral join/union`, `on conflict`, `automatic joins`, `JSONB`, `FK/IDX naming`, `prepared caching`) are all independent but compete for the same macro/parser (`RawQueryExpr`, `ParsedQuery`, `FragmentBuilder`, `GeneratedSql`) and the same “empty-collection semantics” decisions. Do them as a single DSL track, not interleaved with UI work, to keep macro divergence low.

### `oxygen-http` (15 issues, Epic OXY-2/3)
Two parallel tracks: **observability** (logging/metrics OXY-74..77, ~10 pts) and **docs/schema** (OXY-29 spec, OXY-31/32 docs endpoints, OXY-38 schema compat check). The docs endpoints conflict with each other and with client/server config cleanup (OXY-34/35) — decide endpoint path (`/docs`, `/docs.json`, `/openapi.json`) once.

### `oxygen-ui` / `ui-rework` (8 issues, Epic OXY-83 In Progress + OXY-133 To Do)
Mix of bug (OXY-109 wrap — 1 pt, fully autonomous), feature (OXY-110, OXY-152 dropdown, OXY-145 resizable panels) and green-field (OXY-153 IndexedDB, OXY-154 theme). The latter two have no existing implementation — treat as prototypes.

### `oxygen-meta` (5 issues, Epic OXY-118)
Type-class derivation cluster: `OXY-119` may already be done (verify `f88bbbd4`), leaving `OXY-121` unions-as-sums and `OXY-120` macro helpers. These share `SumGeneric`/`ProductGeneric` kernel — sequence them after confirming 119’s status.

### `oxygen-transform` (5 issues, Epic OXY-87)
Spike + partial/configurable transformers (OXY-88/89) plus the `OXY-90/91/92` full-auto spike that should stay as a doc/spike, not code. Conflicts with `oxygen-meta` derivation philosophy — decide where transformation lives before coding.

---

## 5. Risk & Ambiguity Hotspots

### High Priority but Low Confidence (needs spec before sprinting)
* `OXY-110` (High, C3) — “more featureful page messages, open dialog, etc” is open-ended; expanded file assumes `Dialog`/`PageDialog` but no design exists.
* `OXY-143` (Higher, C3) — “service for managing config files” — no module owns it; could be `oxygen-executable`, `oxygen-config`, or a new `oxygen-config` service.
* `OXY-55` (High, C3) — “framework for performance testing” — could be Gatling harness, ZIO Test perf, or JMH + Grafana; zero code signal.
* `OXY-82` (High, C3) — “video for oxygen-http” — not a code task; needs scope (length, audience) before estimating 3 pts.

### Highest Ambiguity (Amb 5–6, will thrash if started blind)
`OXY-81` (6), `OXY-5,17,31,56,77,88,91,92,96,98,129,132` (5). Common pattern: green-field or “explore/add integration” with no chosen backend/library. Each file’s `Open Questions` lists the 2–3 decision sentence that would drop ambiguity to 2.

### Lowest Autonomy (needs pairing)
* `OXY-129` (Aut 2) — grammar re-shuffling for Slyce — zero sykpe code on `main` (only branch `current/feature/slyce`).
* `OXY-132` (Aut 2) — sockets client prototype — same green-field, plus sockets server/client split ambiguity.

### Already-Landed / Trivial Wins
* `OXY-119` — triage notes `f88bbbd4` likely already implements sum-type type-param derivation. Verify by running `sbt test` on `oxygen-meta` — if green, close immediately.
* `OXY-109` — 1 pt, Aut 6, Amb 1 — single CSS fix (`overflow-wrap`/`word-break` in `PageMessagesBottomCorner`). Ship today.
* `OXY-8` — 1 pt, Aut 4, Amb 3 — binary `MigrationCompatibility` gate already exists; needs only policy tightening.

---

## 6. Recommendations

### Immediate Next Steps (this week)

1. **Close or verify `OXY-119`.** Run `git show f88bbbd4` + `sbt 'project core' test` (meta). If `SumGeneric` already handles `F[A]` children, mark Done and reclaim 3 pts.
2. **One-paragraph specs for the 4 high-priority/low-confidence items.** Owner writes 2–3 sentences each: intended deliverable shape (doc vs. code vs. video), parent epic confirmation, and one acceptance bullet. That alone unblocks ~15 pts and moves those 4 from Amb 3–5 to 1–2.
3. **Decide the metrics/tracing stack once (affects 6 issues).** `OXY-5,56,74,75,76,77` all orbit “ZIO metrics → external system.” Choose Prometheus via `zio-metrics-connectors` vs. OTel in a 30-minute decision (inputs: `OXY-5` options doc can be the pre-read). Once decided, `OXY-56` and `OXY-74/75/76/77` become 2–3 pt wiring tasks instead of 5-pt exploratory tasks.
4. **Ship the 1-point quick wins in one PR.** `OXY-109` (wrap), `OXY-92` (create issues subtask of OXY-90), `OXY-8` (compat check tighten). 3 pts, fully autonomous, zero cross-issue risk.

### Sequencing — Suggested 3-Sprint Plan (4 agents)

**Sprint 1 — Foundations & Docs (ready, low ambiguity)**
`OXY-109,113,112,74,76,79,80,102,97,81` (if OTel decision deferred, do doc outline). Unblocks later http/sql work; builds momentum with 2–5 pt items at Autonomy 5.

**Sprint 2 — SQL DSL Track (batch the macro changes)**
`OXY-6,13,14,17,94,96,97,100` as one `oxygen-sql` branch (shared `RawQueryExpr`/`FragmentBuilder` touch). Pair with `OXY-98` only after the 5-pointers land — its 13 pts benefit from the same parser groundwork. Keep `OXY-98` as a separate PR to avoid conflating `LATERAL`/`UNION ALL`/`Sparse`/`zstream`.

**Sprint 3 — HTTP + Transform + UI Prototypes**
`OXY-29,31,32,38,34,35` (http docs + schema compat + config cleanup) as one http track; `OXY-88,89` (transform) after `OXY-90/91/92` spike doc is approved; `OXY-145,152,153,154` (ui) as parallel UI track (no DSL overlap).

Green-field prototypes (`OXY-108` tracing, `OXY-128/129` Slyce, `OXY-131/132` sockets, `OXY-55` perf framework, `OXY-143` config service) should each start with a **spike doc** (the shape already in their `Required Changes`) and only promote to code after the `Ambiguity 5` questions are answered — do not schedule them as 5-pt code tasks yet.

### Staffing Guidance

* **Autonomy 5–6 issues → single agent, async review.** (`OXY-5,31,32,74,97,109,113,116` etc.).
* **Autonomy 3 → pair with domain owner for kickoff (15 min) then async.** Most `sql` DSL items.
* **Autonomy 2 → do not assign solo.** `OXY-129`, `OXY-132` require the author of `current/feature/slyce` or the sockets design owner in the room.

### Process Fix for Next Triage Loop

The concurrent `checklist.md` flip caused ~25 lost `[x]` updates that had to be repaired via a post-batch `for f in results/OXY-*.md` loop. For the next run, **have sub-agents skip the checklist edit** and let the orchestrator do a single `sed` pass after `ls results/*.md` — or serialize writes with `flock`. The data is sound; only the signalling needed repair.

---

## 7. What to Defer or Re-scope

* **Bundle `OXY-6` (array+unnest) with `OXY-17` (IN) and `OXY-98` (lateral/union).** Titles are distinct but SQL generation and binding overlap. One combined spec avoids divergent `IN (?, ?, ?)` vs `= ANY(?::type[])` syntax.
* **Defer `OXY-81` docs + `OXY-82` video until `OXY-29/38` specs land.** Docs that describe a schema format before the spec is frozen will be rewritten.
* **Treat `OXY-90/91/92` as a single Architecture spike (2+1+1 pts → 4 pts total, not 2+2+1).** Its value is the decision doc, not code.
* **Move Epic-level scoping out of Task points.** Several 5-pt Tasks (e.g., `OXY-123` DB compat spec, `OXY-29` HTTP compat spec) are really spec-writing — estimate them as 3-pt docs unless a reference implementation is required.

---

## 8. Open Questions That Block Multiple Issues

Answering these five once unblocks ~15 issues:

1. **Metrics backend choice** (Prometheus vs. OTel) — unblocks `OXY-5,56,74,75,76,77`.
2. **HTTP docs endpoint contract** (`/docs` vs `/docs.json` vs `/openapi.json`, auth, versioning) — unblocks `OXY-31,32,29,38,81,82`.
3. **SQL `IN` empty-list and composite-row semantics** (`WHERE FALSE` vs `IN (NULL)` vs error) — unblocks `OXY-17` and informs `OXY-6/98`.
4. **Transform vs. meta ownership** — should `partial`/`configurable`/`auto` transform live in `oxygen-transform` or `oxygen-meta` derivation? — unblocks `OXY-88,89,90,121`.
5. **Slyce & Sockets prototype scope** — are `OXY-128/129` and `OXY-131/132` throwaway spikes on `current/feature/slyce` or shippable modules on `main`? — unblocks 4 green-field items.

Each answer is a single decision + one bullet in the corresponding `Open Questions` section — no meeting series required.

---

*Generated from `jira-issues/results/OXY-*.md` (63 files) + `jira-issues/checklist.md`. Per-issue files remain the source of truth for file-level change lists; this summary is the portfolio view.*
