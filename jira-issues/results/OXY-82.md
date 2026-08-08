# OXY-82 — Create video for oxygen-http

## Original
- **Key:** OXY-82
- **Checklist line:** `- [ ] [OXY-82](https://kr-oxygen.atlassian.net/browse/OXY-82) — **Documentation** · High — Create video for oxygen-http`
- **Type:** Documentation
- **Priority:** High
- **Title (verbatim):** Create video for oxygen-http
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-82
- **Checklist section:** To Do
- **Epic:** OXY-2 — oxygen-http-server (In Progress) + OXY-3 — oxygen-http-client (In Progress) — Epic filter: PASS (generic "oxygen-http" maps to `modules/http` + `docs/docs/http/*`; belongs to both In Progress http epics; companion to OXY-81 Add docs for oxygen-http)

## Expanded Description

**What this likely is:** Create a short demo/tutorial video for `oxygen-http` (`modules/http` — server `modules/http/zio/.jvm/src/main/scala/oxygen/http/server/` Epic OXY-2 + client `modules/http/zio/src/main/scala/oxygen/http/client/` Epic OXY-3) to be embedded/linked from `docs/docs/http/*`. The video is the companion to **OXY-81 — Add docs for oxygen-http** (also **Documentation · High**); both are High priority because `oxygen-http` is the primary user-facing flagship alongside `oxygen-sql`/`oxygen-ui`. Today all three http doc pages are placeholders (`docs/docs/http/index.md: "TODO : Oxygen HTTP"`, `client/index.md: "TODO : Oxygen HTTP Client"`, `server/index.md: "TODO : Oxygen HTTP Server"`), `mkdocs.yml` nav already lists `HTTP Overview / Client / Server` under `docs_dir: docs`, and `docs/Justfile` + `Dockerfile` build the material site — but there are zero video assets, zero embeds, and zero scripts.

**Current state (verified by reading code/docs):**

1. **Doc placeholders confirm scope gap.** `docs/docs/http/index.md`, `client/index.md`, `server/index.md` are each a single `TODO` line (verified). `docs/docs/sql/index.md` is the exemplar for what a finished overview looks like — quick-start with `@tableName`/`TableCompanion` + `ZIO[Database,...]` + layer wiring + section nav. `oxygen-http` has no equivalent written narrative yet (OXY-81), so the video has no written companion to reference.
2. **Code/demo material exists to record against.** `modules/http` has a fully worked API surface: `ResourceApi`/`UIApi` traits (`api/ResourceApi.scala`, `UIApi.scala`), generic derivation `RouteRepr`/`ApiRepr`, server `Server`/`CompiledEndpoints`/`ZioHttpServer`, client `Client`/`DeriveClient`/`DerivedClientEndpointImpl`, plus integration tests `ViaHttpSpec`/`DirectSpec`/`UserApiContract` and example consumers `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala` + `example/apps/ui` + `example/apps/example-app`. The `cfe88137 Added auto-docs to oxygen-http` and `248dcc57 Added auto-MCP support` commits show `GET /docs.json` and `/docs` html middleware already exist — natural video beats.
3. **No video infra exists.** `grep -rn video docs/ modules/` found only `Icon.video` and `Node.video` (unrelated). `docs/docs/assets/` contains only `images/logo.svg` + `js/jquery.js`. No `videos/`, no `*.mp4`, no YouTube embed in any `*.md`, no `mkdocs.yml` video plugin. `docs/docs/index.md` and `future-plans.md` do not mention video.
4. **Sibling structure confirms intentional docs push.** Checklist groups OXY-81 (Add docs for oxygen-http · High) immediately before OXY-82 (Create video · High) — the only two `Documentation · High` entries in the 79-issue list. `OXY-113 Add docs for oxygen-sql` is `Lower` priority by contrast, confirming http docs/video are the current docs priority. Epics OXY-2/OXY-3 being `In Progress` matches timing: video follows code stabilization, before `oxygen-http` is considered done.

**Most plausible interpretation (frontrunner):** Record a 5–10 minute narrated screencast (screen capture + voiceover) walking through the `oxygen-http` happy path: (a) define a typed API trait with endpoint annotations (`@route` / method/path), (b) derive & mount the server (`Server`, `CompiledEndpoints.toRoutes`) and show `GET /docs` / `GET /docs.json`, (c) derive a typed client (`DeriveClient`) and call it via `ViaHttpSpec`-style test or a small `Main`, (d) show error handling / `ClientErrorHandler` / `ServerErrorConfig` briefly. Publish to YouTube (or Loom) as unlisted/public, embed in `docs/docs/http/index.md` (and optionally `client/index.md`+`server/index.md`) via `iframe` or `![type: video]` with a fallback link, commit any thumbnail/poster to `docs/docs/assets/images/` or `videos/`, and add a one-paragraph transcript/outline beneath the embed for accessibility/search.

**Alternative plausible scopes (why ambiguity remains):** (a) marketing sizzle vs. hands-on tutorial vs. deep-dive on derivation/codegen; (b) client-only vs. server-only vs. end-to-end; (c) self-hosted `mp4` committed to repo vs. externally hosted YouTube/Vimeo (affects repo size and `mkdocs` offline build); (d) whether the video must include live coding from `example/apps/web-server` or is a slide-talk; (e) length/style (60s teaser vs. 20m workshop) and narration language. Any satisfies "Create video" but changes effort, tooling, and review.

**Who it affects:** Every current and prospective `oxygen-http` consumer (service authors deriving APIs, FE engineers consuming typed clients, new contributors evaluating the stack via `scala-oxygen.readthedocs.io`). Today they have only `TODO` pages and must read source/tests; after, they have a visual on-ramp that pairs with the written docs (OXY-81).

**Why it matters (High priority):** High (not Low) signals this is not a polish item but a launch/adoption blocker for Epics OXY-2/OXY-3. As `oxygen-http` matures toward `Released`, written docs alone (OXY-81) leave a discoverability gap — a video drives GitHub/YouTube discovery, conference demos, and onboarding, and is explicitly called out separately from docs (two High issues, not one). High also suggests sequencing: OXY-81 written docs likely land first or in parallel, then the video embeds/links there.

**Inferred acceptance criteria:**

1. A video file or hosted URL exists (YouTube/Vimeo/Loom link is acceptable; if self-hosted, an `mp4`/`webm` under `docs/docs/assets/videos/` or `docs/docs/assets/images/` not bloats the repo > ~50 MB — prefer external host).
2. `docs/docs/http/index.md` (and optionally `client/index.md`/`server/index.md`) replaces its `TODO` with an embedded player (`<iframe>` for YouTube or `<video>` tag) + a title, 2–3 sentence description, transcript/outline bullets, and a direct link fallback. `mkdocs.yml` nav remains unchanged; `docs` still builds via `Justfile`/`Dockerfile`.
3. Video covers at minimum: API trait definition → server derivation/mount → client derivation/call → showing `GET /docs` auto-docs — the core `oxygen-http` loop. Duration 4–15 min, narrated, with readable font/zoom, builds and runs without errors on screen.
4. No code behavior change — purely `docs/` (+ optional `docs/docs/assets/`) edits; no `modules/http` source change, no schema/migration, no new sbt module.
5. Brief PR description/ `docs` note credits OXY-81 relationship and confirms hosting/permissions (unlisted vs. public, channel ownership).

## Confidence
- **Rating:** 3 / 6 — plausible / more likely than not (threshold)
- **Justification:**
  - Module + type give a clear frontrunner: "oxygen-http" unambiguously maps to `modules/http` + `docs/docs/http/*` (the only http module/docs in the repo) and `Documentation` type confirms this is a docs/marketing artifact, not a code feature. Sibling **OXY-81 — Add docs for oxygen-http · High** immediately preceding it establishes the docs push as intentional, with all three `docs/docs/http/*.md` still `TODO` (verified).
  - Downgraded from 4/6 because the title is 4 words with no Jira body, no spec, no skipped test, no `TODO`/`FIXME` mentioning video, and no existing video infra/pattern to copy — so *what* the video should contain, how long, where it is hosted, where it is embedded, and whether it is client-only vs. end-to-end are entirely inferred. Multiple equally valid interpretations (60s teaser vs. 10m tutorial vs. 30m workshop; YouTube vs. committed `mp4`; `example/apps/web-server` demo vs. greenfield minimal app) satisfy the same title with different scope/effort.
  - No Jira description fetched, no design doc, no prior video to clone — ceiling is 3/6 until content/hosting/placement is confirmed.

## Required Changes

Concrete, repo-grounded list. `Verified` = confirmed by reading the file; `Inferred` = required by design but not explicitly hinted in code.

- [ ] **Outline/script — new `docs/docs/http/video-outline.md` or section in `index.md` (Inferred — no current outline; Verified — placeholders exist)**
  - Draft a 1-page outline/script (≈200–400 words) covering: hook (why `oxygen-http`), API trait snippet (derive via `RouteRepr`/`ApiRepr`), server mount (`Server.Config` + `CompiledEndpoints.toRoutes` + `Server.layer`), client call (`DeriveClient` + `Client.Config.layer`), showing `GET /docs.json` + `GET /docs` html middleware, and error-handler note (`ServerErrorConfig`/`ClientErrorHandler`). Keep it tight for a 5–10 min video; match the narrative density of `docs/docs/sql/index.md` quick-start. Verify against `example/apps/web-server/src/main/scala/oxygen/example/webServer/WebServerMain.scala` and `modules/http/it-test/src/test/scala/oxygen/http/ViaHttpSpec.scala` that the snippets compile/run — those are the canonical runnable demos.

- [ ] **Recording — external capture (Inferred — no repo change; Verified — no video assets)**
  - Record with screen capture (OBS/CleanShot) at 1080p, readable editor font (e.g., 16–18pt), terminal + browser side-by-side. Narrated voiceover or captioned text; export `mp4`/`webm` H.264. Decision: host externally (YouTube/Loom/Vimeo) — preferred to avoid bloating the repo — or commit a <50 MB `mp4` to `docs/docs/assets/videos/oxygen-http-overview.mp4`. If externally hosted, ensure channel ownership/permissions and unlisted-vs-public choice are agreed. No `modules/` code change.

- [ ] **Docs embedding — `docs/docs/http/index.md:1-3` (Verified — currently `TODO : Oxygen HTTP`) + optionally `client/index.md` + `server/index.md` (Verified — both `TODO`)**
  - Replace the `TODO` in `index.md` with: `# Oxygen HTTP` heading (keep), 2–3 sentence intro (what `oxygen-http` is, link to `oxygen-sql`/`oxygen-ui` in `docs/docs/index.md:71`), embedded player — e.g., `<iframe width="560" height="315" src="https://www.youtube.com/embed/<id>" …>` for YouTube or `<video controls poster="assets/images/oxygen-http-poster.jpg"><source src="assets/videos/oxygen-http-overview.mp4" type="video/mp4"></video>` for self-hosted — plus a direct link fallback and a transcript/outline bullets beneath for a11y/search. Optionally add a one-line cross-link in `client/index.md`/`server/index.md` ("See the overview video on the HTTP Overview page") rather than duplicating the embed, to keep a single source of truth. Ensure `mkdocs.yml` nav (`HTTP Overview / Client / Server`) needs no change; verify `mkdocs build` still succeeds (video is static asset, no plugin required; `docs/Justfile` build path unchanged).

- [ ] **Assets — `docs/docs/assets/images/` + optionally `docs/docs/assets/videos/` (Verified — only `logo.svg` + `js/jquery.js` exist)**
  - Add a poster/thumbnail `oxygen-http-poster.jpg` (1280×720) to `docs/docs/assets/images/` for the `<video poster>` or markdown preview. If self-hosting, add the `mp4` to `docs/docs/assets/videos/` and ensure `mkdocs.yml` `docs_dir: docs` copies it (material does by default for `docs/docs/assets`). Keep total added binary < ~50 MB; otherwise prefer external host and commit only the poster.

- [ ] **No code change — `modules/http/**` (Verified — no video-related code)**
  - No `modules/http/zio/.../Server.scala` or `Client.scala` change, no `build.sbt` aggregate, no new sbt subproject, no schema/migration. If the video demos `example/apps/web-server`, ensure that app still runs (`sbt example-app/run` or `web-server` run) but do not modify its source unless fixing a trivial compile for the demo — that is out-of-scope for this docs issue.

- [ ] **Verification — `docs` build (Verified — `docs/Dockerfile` + `mkdocs.yml` theme material)**
  - Run `mkdocs build` (or `just docs-build` if `docs/Justfile` defines it) and confirm `site/http/index.html` contains the embed without 404. Check that external `iframe` has `allowfullscreen` and a fallback link for offline builds. No `sbt` build/test required beyond confirming the demo snippet compiles (use `ViaHttpSpec` as smoke if touching example code).

- **Verified vs. inferred:** That all three `docs/docs/http/*.md` are `TODO` placeholders, that `mkdocs.yml` nav already exists, that `docs/docs/assets/` has no video, and that `example/apps/web-server` + `ViaHttpSpec` are runnable demos were verified by reading the files. That the video should be a 5–10 min end-to-end API→server→client→`/docs` walkthrough hosted on YouTube and embedded via `iframe` with a transcript/outline — and that length/hosting/format choices are 5–10 min/YouTube/`iframe` specifically — are inferred from the title and OXY-81 pairing; a shorter teaser, Loom, or committed `mp4` would also satisfy the title.

## Estimates & Autonomy

- **Story points:** 3 (Fibonacci) — lean is 2 if reusing `example/apps/web-server` verbatim, single-take recording, YouTube unlisted, minimal `index.md` embed + 5-bullet outline; 3 if script/outline + retake/edit + poster/thumbnail + transcript + `client`/`server` cross-links; 5 only if greenfield demo app is built, professional editing/captions, or self-hosted `mp4` with compression/accessibility pass
  - Justification: Pure `docs/` (+ `assets/`) change, no `modules/` code, no migration, no cross-team coordination beyond choosing host/channel. Touches 2–4 small markdown files + 1–2 binary assets. Smaller than OXY-34/OXY-35 config refactors (2–3 pts) and far smaller than any Epic; High priority reflects adoption importance, not size.

- **Autonomy:** 4 / 6 — needs one product choice, then fully autonomous
  - Justification: Mechanics are mechanical once hosting/outline are fixed (an agent can draft the outline from `docs/docs/sql/index.md` + `ViaHttpSpec`/`WebServerMain` and record/edit/embed autonomously). The blocking decision is video scope/hosting (YouTube vs. committed `mp4`, 5 min vs. 15 min, end-to-end vs. server-only, narration vs. captions). That is a 15-minute human decision; after, no pairing needed. No engineering dependencies.

- **Ambiguity-to-resolve:** 4 / 6 — moderate-high, several decisions block start
  - Justification: Title is 4 words with no body and no prior video spec — content, length, audience (new-user on-ramp vs. contributor deep-dive), hosting (YouTube channel ownership, public vs. unlisted), placement (`index.md` only vs. all three http pages), and audio (voiceover vs. captions vs. text-only) are all open. A short brief (one paragraph confirming "5–10 min end-to-end tutorial, YouTube, embed in `http/index.md` with outline, use `example/apps/web-server` as demo") would drop this to 1–2. Without it, the agent must assume and get review churn.

## Open Questions

1. **Scope/content:** Should the video be a 5–10 min end-to-end tutorial (API trait → server mount → client call → `GET /docs`), a 60s marketing teaser, or a 20–30 min deep-dive on derivation/codegen? Should it cover both client and server (generic "oxygen-http") or just one? Which narrative should be mirrored — `docs/docs/sql/index.md` quick-start or a new http-specific story?
2. **Hosting:** YouTube (public vs. unlisted, which channel/owner) vs. Loom/Vimeo vs. committed `mp4` under `docs/docs/assets/videos/`? Self-hosted `mp4` bloats the repo and may exceed GitHub limits; external host requires channel permissions and link stability for `readthedocs` builds.
3. **Placement:** Embed only in `docs/docs/http/index.md` (single source) or also in `client/index.md` + `server/index.md`? Should the video also be linked from `docs/docs/index.md` or `future-plans.md`?
4. **Demo source:** Reuse `example/apps/web-server` + `UserApi`/`UserApiContract` (verified runnable) or build a new minimal greenfield example? Should the video show live `sbt run` + `curl`/`httpie` against `GET /docs.json`?
5. **Audio/accessibility:** Voiceover (which voice/language) vs. captioned text-only vs. both? Must a transcript/captions file be committed for a11y/search? Should captions be auto-generated or manually edited?
6. **Production quality:** What is the bar — single-take screen capture is enough, or is light editing (intro card, zooms, cuts) and a poster/thumbnail required? Who reviews/approves the final cut before embedding?
7. **Sequencing with OXY-81:** Should written docs (OXY-81) land first so the video can reference them (and embed there), or should video and docs be delivered together? Does OXY-81 define the outline that OXY-82 then records?
8. **Assumption to confirm:** That "oxygen-http" means the `oxygen-http` module (`modules/http` — Epics OXY-2 In Progress `oxygen-http-server` + OXY-3 In Progress `oxygen-http-client`, `docs/docs/http/*`) and not `oxygen-ui` HTTP helpers or another transport. Confirmed by module proximity and OXY-81 pairing, but worth explicit sign-off.
