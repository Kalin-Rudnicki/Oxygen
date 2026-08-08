# OXY-53 — Add `Lens` typeclass

## Original
- **Key:** OXY-53
- **Checklist line:** `- [ ] [OXY-53](https://kr-oxygen.atlassian.net/browse/OXY-53) — **Task** · Low — Add `Lens` typeclass`
- **Type:** Task
- **Priority:** Low
- **Title (verbatim):** Add `Lens` typeclass
- **Jira URL:** https://kr-oxygen.atlassian.net/browse/OXY-53
- **Checklist section:** To Do

## Expanded Description

**What this likely means:** Introduce a first-party `Lens[S, A]` optic typeclass/abstraction into `oxygen-core` (exposed via `oxygen.predef.core` and `oxygen.core.typeclass`), mirroring the pattern of existing typeclasses (`Show`, `Zip`, `SeqOps`, `StringCodec`, etc. in `modules/general/core/src/main/scala/oxygen/core/typeclass/`). A `Lens[S, A]` is the standard functional optic: a composable getter/setter pair `S => A` / `(A => S => S)` that knows how to `get`, `replace`/`set`, and `modify` a focused field inside a larger structure, with laws (get-replace, replace-get, etc.).

**Why it exists — inferred from code:**

The codebase already depends heavily on `monocle.Lens` (`dev.optics:monocle-core:3.3.0` + `monocle-macro`, declared in `project/Dependencies.scala:11-19` and `build.sbt:547-548` for `oxygen-ui/web`). Current usage is concentrated in the UI layer:

- `modules/ui/web/src/main/scala/oxygen/ui/web/internal/LensUtil.scala:3-10` — thin wrapper `genLens[A,B](f: A => B): Lens[A,B] = GenLens[A](f)` over `monocle.macros.GenLens`. This is the only indirection over monocle today.
- `modules/ui/web/src/main/scala/oxygen/ui/web/WidgetState.scala:3,51-54,81-90` — `ZoomIn` stores `lens: Lens[OuterState, InnerState]` and delegates `get`/`replace`/`modify` to `outer`.
- `modules/ui/web/src/main/scala/oxygen/ui/web/PWidget.scala:3,41-45,412-432,453-475` — `zoomOut`/`zoomOutLens` on `PWidget.Stateful`, `ZoomedCaseBuilder.zoomIn` composes via `lens.andThen(newLens)`, and `PWidget.Ops.lenses` / `FromSizeAndLens.lens` manufacture index lenses for `ArraySeq`/`Seq`/`List` (e.g. `Lens[F[A],A](get)(update)`).
- `modules/ui/web/src/main/scala/oxygen/ui/web/PForm.scala:3,252-256`, `FormValue.scala`, `component/Tabs.scala:3,22,33,61,87-94`, `component/SortableList.scala:3,237-238,309` — all use `monocle.Lens` directly for `Tabs.State` selection lens, `SortableList.itemLens`, form `zoomOut`, etc.
- `docs/docs/ui/forms.md:24` documents `.zoomOut[PageState](_.email)` as "lens into page state" and `TabsPage.scala:30` advertises `Lens[PageState, Tabs.State]`.

There is zero `Lens` typeclass in `modules/general/core/src/main/scala/oxygen/core/typeclass/` today (verified by `ls` — only `Show`, `Zip`, `SeqOps`, etc.), and `oxygen.predef.core` does not export any `Lens`. No `TODO`/`FIXME` explicitly mentioning `Lens` extraction was found, but `LensUtil`'s existence as a single-line macro wrapper plus the `TODO inline def with lenses` comments in `HolyGrail.scala:16` / `SideBar.scala:26` suggest the authors intended to centralize and eventually own this abstraction.

So `OXY-53` almost certainly asks to **own** the optic: define `oxygen.core.typeclass.Lens[S, A]` (or `oxygen.core.optics.Lens`) with the same surface as `monocle.Lens` (and potentially `Setter`/`Optional`/`Prism` later), provide macro-based derivation via `oxygen-meta`/`oxygen-quoted` (consistent with how `Show` and `StrictEnum` derive), and migrate `LensUtil` + the ~96 call sites in `modules/ui/web` to use the internal type instead of importing `monocle` directly. This either removes or hides the `monocle` dependency behind an oxygen abstraction (lowering external coupling for cross-platform `js/jvm/native` builds).

**In-scope behaviors (inferred):**
- Core `Lens[S,A]` trait: `def get(s: S): A`, `def replace(a: A): S => S`, `def modify(f: A => A): S => S`, `def andThen[B](other: Lens[A,B]): Lens[S,B]`, `def compose`/`andThen` symmetry, plus `Setter` view if desired.
- Companion `Lens.apply` / `Lens.from` constructor: `Lens[S,A](get)(set)` mirroring `Lens[F[A],A](get)(update)` pattern in `PWidget.Ops.FromSizeAndLens:469-470`.
- Macro derivation: `inline def genLens[A,B](inline f: A => B): Lens[A,B]` — either re-exporting `monocle.macros.GenLens` under the hood initially, or reimplemented via `oxygen.quoted` + `oxygen.meta.k0.ProductGeneric` field-name extraction (like `Show` derivation uses `K0.ProductGeneric` + `Quotes`).
- Re-export via `oxygen.predef.core` so `import oxygen.predef.core.*` gives `Lens`.
- Optional law checks / tests (get-set, set-get, modify idempotence).

**Out-of-scope / later:** Full optics hierarchy (`Optional`, `Prism`, `Iso`, `Traversal`) — title says `Lens` singular, so only `Lens` (and maybe `Setter` already used in `LensUtil.setBoth`) is required for v1.

**Who it affects:** `oxygen-ui` consumers (every `zoomOut`/`zoomIn` call), and any future module wanting functional updates without `copy` boilerplate. Indirectly affects build dependencies (potential to drop or make `monocle` optional).

**Inferred acceptance criteria:**
1. New file(s) `modules/general/core/src/main/scala/oxygen/core/typeclass/Lens.scala` (or `oxygen/core/optics/Lens.scala`) exist, defining `Lens[S,A]` with get/replace/modify/andThen and a lawful implementation.
2. `Lens.genLens` / `Lens.apply` macro compiles on `js`/`jvm`/`native` (or at least `js` for the UI project) and `LensUtil.genLens` either delegates to it or is deprecated in favor of it.
3. `oxygen.predef.core` exports `Lens` and existing UI code can switch `import monocle.Lens` -> `import oxygen.core.typeclass.Lens` (or `oxygen.predef.core.Lens`) without behavioral change — verified by compiling `oxygen-ui/web`.
4. Basic tests for lens laws exist (even if minimal).
5. Docs/scaladoc explain usage vs. `monocle.Lens` and whether `monocle` remains a transitive dep or is removed.

## Confidence
- **Rating:** 4 / 6 — good evidence, one clear frontrunner
- **Justification:**
  - Code signal is strong and specific: 96 `Lens`/`monocle` hits in `modules/ui/web`, plus `LensUtil.scala` (9 lines) is an explicit thin wrapper around `monocle.macros.GenLens` — the exact seam where a first-party `Lens` typeclass would slot in. The pattern matches the existing `oxygen.core.typeclass.*` design (e.g., `Show`, `Zip`, `SeqOps`).
  - Existing `oxygen.core.typeclass` directory (`ls` verified) contains ~20 typeclasses but no `Lens`, and `oxygen.predef.core` exports `Show`/`Zip`/`SeqOps` but not `Lens` — so the "Add `Lens` typeclass" title maps 1:1 to "add a new file in that package + predef export," no other module fits.
  - `monocle` is only depended on by `oxygen-ui/web` (`build.sbt:547-548`) and the only optics used is `Lens` (plus `Setter` in `LensUtil.setBoth`), so the scope is narrowly a `Lens` abstraction rather than a full optics library — consistent with the singular title.
  - Deduction that the desired implementation is fiber-for-fiber `monocle.Lens` replacement vs. a richer typeclass-derivation is the main uncertainty keeping this at 4 not 5/6: no Jira body was fetched, no TODO says "replace monocle," and whether derivation should use `oxygen-meta` macros or keep delegating to `GenLens` is unstated — but the frontrunner (thin owned `Lens` delegating to GenLens initially) is still the most parsimonious reading.

## Required Changes (only if Confidence >= 3)

- [ ] **New typeclass — `modules/general/core/src/main/scala/oxygen/core/typeclass/Lens.scala` (or `oxygen/core/optics/Lens.scala`) (Verified missing, Inferred shape)**
  - Define `trait Lens[S, A]` with at minimum: `def get(s: S): A`, `def replace(a: A): S => S`, `def modify(f: A => A): S => S`, `def andThen[B](other: Lens[A, B]): Lens[S, B]` (or `compose`), and `def set` alias. Optionally extend `Setter[S,A]` for `LensUtil.setBoth` compatibility.
  - Implement `Lens.apply[S,A](get: S => A)(set: A => S => S): Lens[S,A]` constructor mirroring `PWidget.Ops.FromSizeAndLens.lens` pattern (`Lens[F[A],A](_(idx))(value => fValue => update(...))`).
  - Provide `object Lens` with `inline def apply[S,A](inline f: S => A): Lens[S,A]` / `genLens` macro entry point. Initial implementation can delegate to `monocle.macros.GenLens` (`GenLens[A](f).asInstanceOf[Lens[A,B]]` via adapter/wrapper) to keep behavior identical while hiding the import; later iteration can reimplement via `oxygen.meta.k0.ProductGeneric` + `scala.quoted.Quotes` (see `modules/general/meta` + `modules/general/quoted` and `Show.derived` macro pattern).
  - Add law helpers / `equals` considerations (lenses are functions — reference equality is fine).
  - **Verified:** `modules/general/core/src/main/scala/oxygen/core/typeclass/` exists and follows `trait X` + `object X` + `given` instances pattern; **Inferred:** exact method names and whether to also define `Optional`/`Prism` — v1 should be `Lens` only per title.

- [ ] **Update `LensUtil` — `modules/ui/web/src/main/scala/oxygen/ui/web/internal/LensUtil.scala:1-25` (Verified)**
  - Change `import monocle.{Lens, Setter}` to `import oxygen.core.typeclass.Lens` (or `oxygen.predef.core.Lens`) and keep `GenLens` import only if still delegating.
  - Keep `genLens`, `arraySeq`, `setBoth` signatures but return `oxygen.core.typeclass.Lens`/`Setter`. If `Setter` is not part of the new typeclass, define a minimal `Setter[S,A]` alias or keep `monocle.Setter` for that one helper — document choice.
  - **Verified:** current `LensUtil` is 25 lines and is the sole macro adapter; **Verified:** `arraySeq` already constructs `Lens` via `Lens(get)(set)` — that call site must be updated to the new constructor.

- [ ] **Migrate UI call sites — `modules/ui/web/src/main/scala/oxygen/ui/web/{WidgetState,PWidget,PForm,FormValue,component/Tabs,component/SortableList}` (Verified imports)**
  - Replace `import monocle.Lens` with `import oxygen.core.typeclass.Lens` (or `oxygen.predef.core.*` which should re-export it). Files verified: `WidgetState.scala:3`, `PWidget.scala:3`, `PForm.scala:3`, `Tabs.scala:3`, `SortableList.scala:3`, `FormValue.scala:3`.
  - No behavioral change expected — `lens.get`/`lens.replace`/`lens.modify`/`lens.andThen` call sites (`WidgetState.ZoomIn`, `PWidget.ZoomedCaseBuilder`, `Tabs` selected-tab update) should continue to compile against new type.
  - **Verified:** ~96 hits across `modules/ui`; **Inferred:** whether migration is required in this issue or only the typeclass creation is required — title says "Add `Lens` typeclass" not "Migrate to Lens typeclass," so creation alone may satisfy the issue; migration can be a follow-up. Document assumption.

- [ ] **Predef export — `modules/general/core/src/main/scala/oxygen/predef/core.scala` (Verified)**
  - Add `Lens` (and optionally `Setter`) to the `export oxygen.core.typeclass.{..., Lens}` line so `import oxygen.predef.core.*` provides it, matching `Show`/`Zip`/`SeqOps` export pattern.
  - **Verified:** current `predef/core.scala` exports `Show`, `Showable`, `StrictEnum`, etc. but not `Lens`.

- [ ] **Build — `project/Dependencies.scala:11-19` + `build.sbt:547-548` (Verified)**
  - Decision: either keep `monocle` as implementation detail of `oxygen-core` (move dep from `oxygen-ui/web` to `oxygen-core` if `genLens` still delegates to `GenLens`), or remove it if reimplemented via `oxygen-meta`. For v1, keeping `monocle` as an internal dep is lowest-risk; document the choice. No new external deps expected.

- [ ] **Tests — `modules/general/core/src/test/scala/` (or `modules/general/core/test`) (Inferred)**
  - Add `LensSpec` covering lens laws: `get(replace(a)(s)) == a`, `replace(get(s))(s) == s`, `modify(id)(s) == s`, `andThen` composition, and `arraySeq`/`itemLens` round-trip. Minimal — mirrors no existing lens tests (none found).

- [ ] **Docs — Scaladoc on `Lens` trait + optional `docs/` note (Inferred)**
  - Document that `Lens` is the owned abstraction over `monocle.Lens`, show `genLens(_.field)` and `Lens(_.get)(set)` examples, and note `zoomOut`/`zoomIn` usage in `docs/docs/ui/forms.md`.

## Estimates & Autonomy (only if Confidence >= 3)

- **Story points:** 3 (Fibonacci) — small, well-scoped new typeclass + predef export + LensUtil update; if full UI migration + macro reimplementation via `oxygen-meta` is required, 5. As a pure "add typeclass, LensUtil delegates to monocle" task: 3. As "reimplement GenLens without monocle": 5.
  - Justification: Existing typeclasses (`Show`, `Zip`) provide clear template; `Lens` itself is ~30-50 lines plus macro delegation. UI migration is mechanical (6-8 files, same API). Reimplementing field-name macro via `oxygen.quoted` would add a day.

- **Autonomy:** 4 / 6 — mostly autonomous with repo + code context, but needs one product decision (see Open Questions) before committing to "wrap monocle vs. replace monocle."
  - Justification: Trait shape and placement are obvious from `oxygen.core.typeclass` conventions; `LensUtil` is the single integration point. The only blocker to full autonomy is whether the reviewer expects a pure-Oxygen macro derivation (using `oxygen-meta`/`oxygen-quoted`) or is happy with a `monocle`-backed v1.

- **Ambiguity-to-resolve:** 3 / 6 — moderate; ready to start on a thin wrapping implementation, but a pure-Oxygen reimplementation requires clarification.
  - Justification: Title is 3 words with no Jira body; the macro strategy ("use `GenLens` internally vs. rewrite via `K0.ProductGeneric`") and whether to also introduce `Setter`/`Optional`/`Prism` in the same issue are unstated. A one-sentence confirmation ("v1 may delegate to monocle, UI migration optional") drops this to 1.

## Open Questions

1. **Wrap vs. replace `monocle`:** Should v1 `Lens` be a thin owned wrapper that still delegates `genLens` to `monocle.macros.GenLens` (lowest risk, keeps `monocle` as transitive dep), or must it be reimplemented via `oxygen.meta`/`oxygen.quoted` and remove the `monocle` dependency entirely? The latter is larger (macro over `Quotes.reflect` + `ProductGeneric` field extraction).
2. **Scope of optics:** Is only `Lens[S, A]` required, or should `Setter[S,A]`, `Optional[S,A]`, `Prism[S,A]`, `Iso[S,A]` also be introduced in this issue? `LensUtil.setBoth` currently uses `monocle.Setter`, and PWidget's collection lenses are `Lens` — title says singular `Lens`.
3. **Module placement:** `oxygen.core.typeclass.Lens` (consistent with `Show`/`Zip`) vs. `oxygen.core.optics.Lens` (new package)? Placement affects `predef` exports.
4. **UI migration in scope?** Does "Add Lens typeclass" include migrating `modules/ui/web` off `monocle.Lens` imports in this issue, or is creation of the typeclass alone sufficient with migration as a follow-up?
5. **Cross-platform:** Must `Lens` macro work on `js`/`jvm`/`native` (like `oxygen-core` cross-project), or only `js` where `oxygen-ui/web` runs? `monocle-macro` is cross-platform but `oxygen-quoted` macros may differ per platform.
6. **Naming/compatibility:** Should the new `Lens` be source-compatible with `monocle.Lens` (same `get`/`replace`/`modify`/`andThen` names) to make migration a pure import swap, or is a different API (e.g. `get`/`set`) preferred? Current UI code calls `lens.get`, `lens.replace`, `lens.modify`, `lens.andThen`.
7. **Assumption to confirm:** That the intended `Lens` is the functional optics lens (getter/setter pair) as used in `WidgetState`/`PWidget`/`Tabs`/`SortableList`, not a database/column lens or a test-lens utility.
