# OXY-154-colormode — Harden ColorModePicker

Branch: `OXY-154-colormode` (stacked on `OXY-154`, PR #296).

## Goal
Make `ColorModePicker` a first-class standalone Light/Dark/System component: decouple from ThemePicker/pack work, add variants, solid a11y + keyboard nav, react to cross-tab/programmatic ColorMode changes, unit tests, showcase, docs.

## Findings (current ColorModePicker.scala)
- Single `apply(label)` -> segmented control only. No variants.
- Uses own `ModeState` GlobalState + own `readStored` (duplicates ColorMode.readStored logic w/ localStorage).
- a11y: role=radiogroup/radio, aria-checked, tabindex roving present, BUT no keyboard handlers (arrow keys / space) wired.
- Known limitation documented: highlight does not live-update on cross-tab/programmatic changes (no Broadcast subscribe).
- Coupling to ThemePicker: shares *pattern* (GlobalState + detach + readStored) but no direct code dependency. Independent already at code level — good.

## Decisions
- ColorModePicker was already code-independent of ThemePicker (only shared a *pattern*: GlobalState + detach + readStored). Confirmed no import/type coupling. Kept its own shared `ModeState` GlobalState seeded via `ColorMode.storageKey`/`ColorMode.parse` (depends only on `ColorMode` service, never on `Theme`/`OxygenThemes`/`ThemePicker`). => fully standalone.
- Rewrote as a config case-class extending `PWidget.Deferred.Stateless` (matches ToggleThumb / HorizontalRadio / Icon conventions), so instances are droppable widgets AND fluently configurable. Backward-compat `apply()` / `apply(label = ...)` retained (existing call sites: ThemePage, StylesPage, KitchenSinkPage, ShellPage).
- Variants: **Segmented** (role=radiogroup, default) + **Compact** (single icon cycle button, top-bar friendly, role=button). Deliberately did NOT add a native `<select>`/dropdown "form" variant — form contexts are already served by the generic `HorizontalRadio.form[ColorMode.Mode]`; a third variant would be over-engineering. Documented.
- Config knobs: `.small/.medium/.large`, `.label(..)/.noLabel`, `.includeSystem(..)/.lightDarkOnly` (sensible variant: some apps only want Light/Dark), `.withIcons/.noIcons`, `.idPrefix(..)` (multi-instance focus-id disambiguation).
- Icons: Light=sun, Dark=moon, System=monitor.
- a11y hardening (segmented): roving `tabindex` (0 on selected, -1 others), `role=radiogroup`+`aria-label`, per-option `role=radio`+`aria-checked`+stable `id`, keyboard nav on the group (Left/Up = prev, Right/Down = next, Home/End = first/last with wrap, Space/Enter select) that selects + moves DOM focus to the new radio. Compact: native `<button>` via Button + descriptive `aria-label`/`title` announcing current mode and next action.
- Extracted pure decision logic into `ColorModePicker.Logic` (options list, `keyToNav`, `resolveNav` with wraparound, `cycle`) — unit-tested (no DOM).
- Cross-tab/programmatic live update: added opt-in `ColorModePicker.syncAcrossTabs: URIO[Scope, Unit]` that subscribes to `Broadcast.subscribeThemeMode` and pushes into the shared `ModeState` (via `GlobalState.set`, which re-renders the current page) so the highlight live-updates. Left OFF by default (no behavior change); consumers wire it in `prePageLoad`/`postLoad`. Kept ColorMode's own doc-application subscription untouched. Removed the old "known limitation" caveat since it's now addressable.

## What changed (files)
- `modules/ui/web/.../component/ColorModePicker.scala` — rewritten as config case-class widget (Segmented + Compact variants), a11y + keyboard nav, `Logic` (pure), `syncAcrossTabs`, `Size`/`Variant`/`Nav`. Backward-compat `apply()` kept.
- `modules/ui/web/src/test/.../style/OxygenColorSystemSpec.scala` — added `ColorModePicker.Logic` suite (6 tests, all pure/DOM-free).
- `example/apps/ui/.../showcase/pages/ThemePage.scala` — showcase now demos 4 variants (segmented, with-icons, light/dark-only, compact).
- `docs/docs/ui/builders.md` — documented variants, a11y, `syncAcrossTabs`, form guidance.

## Verification
- `oxygen-ui-web/compile` ✅  `example-ui-web/compile` ✅  `oxygen-ui-web/test` ✅ (51 tests, incl. 6 new).
- `sbt fmt` applied; JGit worktree workaround (`git.gitUncommittedChanges := false`) added temporarily to load sbt, then reverted — NOT committed.
- No changes to `ColorMode` / `Theme` / `ColorTheme` / `ThemePicker` behavior. Existing `ColorModePicker()` call sites (ThemePage, StylesPage, KitchenSinkPage, ShellPage) unchanged and still compile.

## Assumptions
- Native `<select>`/dropdown variant intentionally skipped (HorizontalRadio.form covers form contexts) — judged as avoiding over-engineering, not a gap.
- `syncAcrossTabs` left opt-in (no default wiring) to avoid changing existing app bootstrap behavior; `GlobalState.set` from a forked subscription fiber re-renders the current page (consistent with `ColorMode.subscribeCrossTab`'s scoped-fork pattern). Not exercised in a live browser here — logic is straightforward but unverified at runtime.
- Keyboard focus management (`document.getElementById(id).focus()` after `st.set`) relies on `set` completing after the re-render patch; matches the framework's synchronous render model but not browser-tested.
- Multi-instance focus ids share a default `_idPrefix`; `idPrefix(..)` provided for disambiguation. All instances intentionally share one persisted preference (single logical selection).

## CONFIDENCE: 8/10
Compiles, formatted, all tests green, backward compatible, decoupling verified. Deductions: runtime a11y/keyboard-focus behavior and `syncAcrossTabs` live cross-tab re-highlight are logically sound and unit-tested at the decision-logic level, but not exercised in a real browser this session.
