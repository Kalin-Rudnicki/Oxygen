# Builders & components

Oxygen UI widgets use **immutable HolyGrail-style builders**. The old `Decorator` / `Decorable` stacking API is gone.

## Pattern

```scala
// Start from empty / factory
Button.empty.medium.primary.solid.content("Save", onClick := …)

// Or short constructors
Button("Save").positive.subtle

// Layout shells use slots
HolyGrail.empty
  .topHeight(48.px)
  .leftWidth(220.px)
  .top(TopBar.empty.left(TopBar.item("Home")))
  .modLeft(_ => SideBar.empty.middle(navItems*)) // replace the rail, don't nest SideBars
  .center(pageBody)
```

### Rules

1. **Immutable `copy`** — every fluent method returns a new instance.
2. **Slots over soup** — content goes in named slots (`center`, `content`, `leading`, …), not free-form node modifier chains as the primary API.
3. **CSS variables for color** — use `S.color.*` / theme packs; do not hard-code product greys for chrome that should theme.
4. **`PWidget.Deferred` is the default** — builders configure a value; DOM materializes on `build`. See [Deferred components](#deferred-components) below.
5. **Forms are separate** — labeled fields live as `Component.form` (`PForm.Deferred`), not as a second Props blob. See [Forms](forms.md).
6. **`apply` adds children** — on a component builder, `apply` / `content` means “append child widgets”, not “configure me with a lambda”. Prefer named methods for configuration (`size`, `intent`, `modX`, …).
7. **Stay polymorphic** — Deferred builders should keep `Env` / `Action` / `StateGet` / `StateSet` type params. Type-locking a Deferred component to `Any`/`Nothing`/`Unit` defeats configurability (rare special cases only).

---

## Deferred components

Almost every interactive or chrome control should look like:

```scala
final case class MyControl[-Env, +Action, -StateGet, +StateSet <: StateGet](
    // config fields…
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def size(s: Size): MyControl[Env, Action, StateGet, StateSet] = copy(…)

  def content[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): MyControl[Env2, Action2, StateGet2, StateSet2] = …

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = …
}

object MyControl extends WidgetTypes[MyControl] {
  // --- public factories first ---
  val empty: MyControl.Const = …
  def apply(): MyControl.Const = empty
  def apply(text: String): MyControl.Const = empty.content(text)

  // --- nested form / State / public types ---

  // --- private helpers last ---
  private def roleVars(…): … = …
}
```

### Companion layout

| Zone | What goes there |
| ---- | --------------- |
| **Top** | Public factories: `empty`, `apply`, short constructors |
| **Middle** | Nested public types: `State`, `form`, `Item`, … |
| **Bottom** | Private helpers / pure model / CSS token tables |

`apply()` should call the `empty` val (not the reverse).

### File naming

- **One primary type per file** → `PascalCase.scala` matching the type (`Button.scala`).
- **Multiple top-level definitions in one file** → **lowercase** filename (`columns.scala` for `Row`/`Col`/`ColumnsStyle`, `loading.scala` for `Spinner`/`Progress`/`Skeleton`).

### Package map

| Package | Owns |
| ------- | ---- |
| `oxygen.ui.web.component` | Interactive + chrome controls (Button, TextField, Modal, TopBar, …) |
| `oxygen.ui.web.layout` | Full-page shells (`HolyGrail`, `CenteredCard`) — not “widget soup” in `component` |
| `oxygen.ui.web.create` | HTML/CSS primitives, sheets, media helpers (`MediaCSS`) |
| `oxygen.ui.web.style` | Color system, theme **pack definitions**, breakpoints |
| `oxygen.ui.web.service` | Runtime apply / persist / subscribe (`Theme`, `ColorMode`, `Window`, …) |
| `oxygen.ui.web.defaults` | Shared sheets + default pages (`coreOxygenStyleSheets`) |

**Packs vs apply:** theme *definitions* live in `style.OxygenThemes`; *applying* a pack to the document lives in `service.Theme`.

---

## Component families

| Family | Examples | Notes |
| ------ | -------- | ----- |
| Actions | `Button`, `Icon` | Intent + size + variant |
| Text entry | `TextField`, `TextArea` | Raise `Form.Submit` on Enter / Ctrl+Enter |
| Choice | `Dropdown`, `HorizontalRadio`, `Checkbox`, `ToggleThumb` | State types on companions |
| Date/time | `DatePicker`, `TimePicker`, `DateTimePicker` | Custom UI (not native date input) |
| Layout | `HolyGrail`, `CenteredCard`, `Row` / `Col` (+ `ColumnsStyle` sheet) | Full-page shell + body grid |
| Chrome | `TopBar`, `SideBar`, `Modal`, `Drawer` | App frame |
| Feedback | `PageMessages` (API) + `PageMessagesBottomCorner` (UI), `Spinner`, `InfoSection` | Transient UI |
| Data | `Table`, `Tabs`, `SortableList`, `Pagination`, `InfiniteScroll` | Mixed maturity — see [Known gaps](#known-gaps--wip) |

## Form companions

Every form-esque control should expose:

```scala
object TextField {
  final case class form[+Value] … extends PForm.Deferred[…] { … }
  object form {
    def apply[A: StringDecoder](label: String): TextField.form[A]
  }
}
```

With:

- typed **inner** control (`modField` / `modButton` / …)
- **label/chrome** helpers (`modLabel`, `describe`, …)
- deferred **build** into a real `PForm`

Details: [Forms tutorial](forms.md).

---

## Layout shells

### HolyGrail

```scala
HolyGrail.empty
  .topHeight(48.px)          // grid row owns top height
  .leftWidth(220.px)
  .top(TopBar.empty.brand.left(…))  // do NOT also set TopBar.barHeight under HolyGrail
  .modLeft(_ => SideBar.empty.surface.middle(nav*))
  .center(body)
```

- **Page messages are included by default.** HolyGrail mounts `PageMessagesBottomCorner` as a fixed overlay sibling of the shell. Callers must **not** also render `PageMessagesBottomCorner.default` (or another messages corner) in page body/chrome — that double-mounts toasts. Opt out only with `.noPageMessages` when you intentionally own messaging yourself.
- Center pane is scrollable by default; shell chrome stays fixed.
- Below `md`, `HolyGrail.responsiveSheet` collapses side rails (included in `coreOxygenStyleSheets`).

**Height fighting:** set height on **either** HolyGrail’s top row (`.topHeight`) **or** `TopBar.barHeight`, not both. Under HolyGrail, leave TopBar height unset so the grid row owns size.

### CenteredCard

Auth / marketing body preset in `oxygen.ui.web.layout.CenteredCard` — not a form system.

```scala
CenteredCard.empty.boxShadow("…")(h1("Login"), loginForm)
```

### SideBar slots

`HolyGrail.left(…)` / `.right(…)` append into the rail’s **middle** slot. To replace the whole rail (surface, top brand, bottom user menu), use **`modLeft` / `modRight`**:

```scala
.modLeft(_ => SideBar.empty.surface.top("Brand").middle(nav*).bottom(userMenu))
```

Do not nest a full `SideBar` inside `.left(SideBar.empty.…)` — that double-wraps.

---

## App bootstrap

### Stylesheets

Use the shared helper so reset, theme vars, core classes, columns, HolyGrail responsive, motion, tooltips, and sortable styles all load:

```scala
import oxygen.ui.web.defaults.*

override val styleSheets: ArraySeq[StyleSheet] =
  coreOxygenStyleSheets ++ ArraySeq(MyApp.sheet)
```

Do not reinvent a partial list of sheets unless you know which ones you can drop.

### Theme + color mode

**Preferred:** one combinator wires both color mode (`ColorMode`) and theme pack (`Theme`) — apply
stored preferences first-paint, then subscribe both to their cross-tab channels:

```scala
import oxygen.ui.web.service.ColorTheme

override protected def prePageLoad: RIO[Env & Scope, Unit] =
  ColorTheme.install
```

`ColorTheme.applyStored` (apply only, no cross-tab subscription) and the underlying
`ColorMode.*` / `Theme.*` methods remain available for manual wiring:

```scala
import oxygen.ui.web.service.{ColorMode, Theme}

override protected def prePageLoad: RIO[Env & Scope, Unit] =
  ColorMode.applyStoredOrSystem *>
    Theme.applyStoredOrDefault *>
    ColorMode.subscribeCrossTab *>
    Theme.subscribeCrossTab
```

**Pickers (UI):** drop-in reusable widgets — no page state to wire:

```scala
import oxygen.ui.web.component.{ColorModePicker, ThemePicker}

ColorModePicker()                              // Light / Dark / System segmented control
ColorModePicker(label = Some("Color mode"))
ThemePicker()                                  // all OxygenThemes packs, as selectable cards
ThemePicker(OxygenThemes.graphiteFamilyPacks)  // filter to a pack family
```

Both are self-contained (backed by a shared `GlobalState` seeded from the stored value): they
reflect the current selection and call `ColorMode.setAndPersist` / `Theme.applyAndPersist` on
select.

**`ColorModePicker`** is a first-class, standalone Light/Dark/System control — independent of the
theme-pack machinery (depends only on the `ColorMode` service). It is a config builder (à la
`ToggleThumb` / `HorizontalRadio`):

```scala
ColorModePicker()                    // segmented radiogroup (default)
ColorModePicker.compact              // single icon button that cycles modes (top-bar friendly)
ColorModePicker.segmentedWithIcons   // segmented + Light/Dark/System glyphs
ColorModePicker().lightDarkOnly      // drop the System option
ColorModePicker().large.label("Theme")
```

- **a11y:** segmented is a `role=radiogroup` with `role=radio` + `aria-checked` per option, roving
  `tabindex`, and keyboard nav (Left/Up = prev, Right/Down = next, Home/End = first/last with wrap,
  Space/Enter select) that moves selection **and** DOM focus together. Compact is a native
  `<button>` with a descriptive `aria-label`/`title`.
- **Live cross-tab highlight (opt-in):** by default the highlight reflects the mode at render time.
  Wire `ColorModePicker.syncAcrossTabs` once (e.g. from `prePageLoad`) to have every mounted picker
  re-highlight on cross-tab / programmatic `ColorMode` changes:

  ```scala
  override protected def prePageLoad: RIO[Env & Scope, Unit] =
    ColorTheme.install *> ColorModePicker.syncAcrossTabs
  ```

  (For labeled *form* contexts, use the generic `HorizontalRadio.form[ColorMode.Mode]` instead — a
  dedicated dropdown variant is intentionally omitted.)

`ThemePicker` does not re-highlight on cross-tab changes until the page re-renders.

### Mobile / viewport

- `PageApp` injects a viewport meta tag (`width=device-width`, `viewport-fit=cover`).
- Server-rendered HTML via `PageHtmlResponse` includes the same meta (mobile looks much better; layout on phones is still imperfect — expect further shell work).
- Prefer `maxWidth := 100.pct` + `boxSizing.borderBox` on width-taking controls.
- Breakpoints: `style.Breakpoints` + `create.MediaCSS`.

---

## Style inclusion

Prefer **shared stylesheets** (`coreOxygenStyleSheets`, companion `sheet` vals) over injecting one-off `<style>` tags from components. If a control needs CSS (drag chrome, tooltips, motion), add a `sheet` and wire it into `coreOxygenStyleSheets` (or document that apps must include it).

---

## Known gaps / WIP

These are intentional honesty notes for agents and humans (from the UI overhaul review). Prefer existing solid components; do not “finish” these unless the task says so.

| Area | Status |
| ---- | ------ |
| `InfiniteScroll` | Very much WIP / wonky API |
| `Memo` | Experimental — not production-verified |
| `Drawer` | Works; Deferred conversion still TODO |
| `Button.form` | Internally uses `Button.Const` today; full Env/Action/State params are a known follow-up |
| `LockAware` | Revisit after remaining component cleanup |
| Mobile shell | Viewport meta helps a lot; narrow layouts still rough |
| `PageHtmlResponse` | Needs OG / social meta support (TODO) |
| `service.Broadcast` / `MatchMedia` / some IDB edges | APIs may still change — prefer Theme/ColorMode patterns |

---

## Code style (UI modules)

- Prefer **imports** over long fully-qualified names at call sites.
- Default arguments that hide `Option` / presence are discouraged for public APIs — make optionality explicit when designing new APIs.
- Public factories near the top of companions; private helpers at the bottom.
)
