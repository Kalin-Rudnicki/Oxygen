# Agent guide: implementing Oxygen UI

This page is written for **automated coding agents** (and humans acting like one).  
Follow it strictly. Oxygen UI will reject “generic React-ish” freestyle.

---

## Hard rules (do not break)

1. **Use Oxygen components** (`oxygen.ui.web.component.*`) for controls users interact with.  
   Do not invent parallel Button / Input / Modal stacks with raw HTML unless the component truly does not exist **and** you are adding a proper builder in-tree.

2. **Use `PForm` for forms.**  
   - Build with `TextField.form`, `Button.form`, `Dropdown.form`, …  
   - Compose with `<*>`, `zoomOut`, `required`.  
   - Handle with `onSubmit` or `handleActionStateful`.  
   - Never rely on native `<form onsubmit>` or document-level Enter hacks.

3. **Builders are immutable fluent APIs.**  
   - No `Decorator` / `Decorable` / `GenericDecorator`.  
   - No reintroducing `FormConfig` + configure-lambda as the primary API.  
   - Forms: nested **`final case class form` extends `PForm.Deferred`** with typed child + `modX`.  
   - Widget builders: **`extends PWidget.Deferred`**, polymorphic `Env`/`Action`/`State` — do not type-lock Deferred components.

4. **`apply` means children.**  
   On component builders, `apply` / `content` appends child widgets. Configuration uses named methods (`size`, `intent`, `modField`, …). Do not invent `apply(configure: X => X)` as the primary public API.

5. **Colors come from the theme system.**  
   - Prefer `S.color.*` CSS vars.  
   - Prefer `OxygenThemes` packs over one-off hex for product chrome.  
   - Solid fills that need readable text use **on-fill** tokens (`primary.on`, status `.on`, …).  
   - Apply packs with `Theme.applyStoredOrDefault` (service), not `OxygenThemes.*` mutators.

6. **State lives in page / widget state**, not free global vars.  
   - Control state: `TextField.State`, `Dropdown.State[A]`, …  
   - Decode to domain types **on submit**, not on every keystroke (unless you have a deliberate live-parse UX).

7. **Do not discard submit Actions in production paths.**  
   - `.widget.discardAction` is for demos only.  
   - Enter → `Form.Submit` only works if Action is handled.

8. **Page lock for async submit.**  
   - Prefer `Button.form` (lock-aware by default) + `PageLock.withPageLock { … }` around effects.  
   - **Do not** wrap `withPageLock` in an extra `ZIO.scoped` — the lock already scopes the effect.

9. **Layout package for full-page shells.**  
   - `HolyGrail`, `CenteredCard` live in `oxygen.ui.web.layout`.  
   - Under HolyGrail, set top height on the shell (`.topHeight`), not also on `TopBar.barHeight` (they fight).  
   - Replace side rails with `.modLeft` / `.modRight`; `.left(…)` only appends middle-slot children.  
   - **Do not** mount `PageMessagesBottomCorner` yourself when using HolyGrail — it is **on by default**. Use `PageMessages.add` / errors for content; only call `.noPageMessages` if you deliberately own the overlay.

10. **Stylesheets:** start from `coreOxygenStyleSheets` in app `styleSheets`. Do not hand-roll a partial sheet list.

11. **No drive-by refactors.**  
    - Change only what the task needs.  
    - Do not “clean up” unrelated components, themes, or layout shells.  
    - Do not “finish” WIP APIs (`InfiniteScroll`, `Memo`, …) unless asked.

12. **Do not revive deleted packages.**  
    - No `componentV2`, no Decorator dual API, no parallel docs-only fake APIs.  
    - No resurrecting removed `PageLayout` — use `HolyGrail` / `CenteredCard`.

13. **Mobile / overflow:** controls that take width must tolerate narrow viewports (`maxWidth 100%`, border-box).

14. **Imports over FQNs.** Prefer `import oxygen.ui.web.layout.*` over spelling long paths at every call site.

---

## Preferred implementation order

When adding a **new form screen**:

```text
1. Define PageState fields (control States, not only domain types)
2. Compose field forms with zoomOut + required
3. Zip with Button.form
4. handleActionStateful / onSubmit → PageLock.withPageLock → API → navigate / messages
5. Style with existing spacing / color tokens only
6. Verify Enter submits from a TextField
```

When adding a **new form-capable component**:

```text
1. Immutable case class extending PWidget.Deferred (polymorphic Env/Action/State)
2. Companion: empty + apply factories at top; private helpers at bottom
3. final case class form extends PForm.Deferred
   - typed child (or clear chrome-only form)
   - modChild / modLabel
   - object form { def apply(label: String): form }
4. Docs: mention in forms.md table
5. Optional showcase usage WITHOUT discardAction if demonstrating submit
6. Multi-type files → lowercase filename
```

When adding **app chrome**:

```text
1. HolyGrail.empty.topHeight(…).leftWidth(…).top(TopBar…).modLeft(…).center(…)
2. Include coreOxygenStyleSheets
3. Theme + ColorMode in prePageLoad
```

---

## Allowed raw HTML

Raw `div` / `span` / `input` from `create` are fine for **layout glue** and for implementing a component’s **private** render.

They are **not** fine as the public way apps collect domain input when a form component exists:

| Need | Use |
| ---- | --- |
| Text / password / email | `TextField.form` / `TextField` |
| Multiline | `TextArea.form` |
| Submit | `Button.form` |
| Select one of many | `Dropdown.form` / `HorizontalRadio.form` |
| Boolean | `Checkbox.form` / `ToggleThumb.form` |
| Date / time | `DatePicker` / `TimePicker` / `DateTimePicker` (+ `.form`) |
| Files | `FileDropZone` (+ `.form`) |
| Color seed | `ColorPicker` (+ `.form`) |
| Action / nav popup menu | `DropdownMenu` (or `TopBar.item.dropdown`) — not a hand-rolled overlay |
| Page shell | `HolyGrail` / `CenteredCard` (`layout`) |

---

## Form builder template (copy this)

```scala
object MyControl {
  final case class form private (
      private val _fieldName: String,
      private val _control: MyControl,
      private val _label: Label,
      // chrome fields…
  ) extends PForm.Deferred[Any, /* Action */, MyControl.State, MyControl.State, /* Value */] {

    override protected lazy val build: PForm[…] =
      Form.makeWith(_fieldName, /* labeled widget */)(/* extract value */)

    def modControl(f: MyControl => MyControl): form = copy(_control = f(_control))
    def modLabel(f: Label => Label): form = copy(_label = f(_label))

    // shortcuts that call modControl / modLabel
  }
  object form {
    def apply(label: String): MyControl.form =
      new MyControl.form(
        _fieldName = label,
        _control = MyControl.empty,
        _label = Label(label),
        // …
      )
  }
}
```

**Name clash tip:** if the form has `def width(…)`, do not write bare `width := …` inside `build` — qualify CSS (`oxygen.ui.web.create.width := …`).

---

## Anti-patterns (agents love these — stop)

| Anti-pattern | Do instead |
| ------------ | ---------- |
| `document.getElementById` / direct DOM mutation | Widget state + rerender |
| Global `var` form model | `PageState` + `WidgetState` |
| Custom CSS class string soup for theme | `S.color` / `O.*` style sheet objects |
| Rebuilding Decorator “for flexibility” | Immutable builder + `modX` |
| `FormConfig` lambda “like before” | Deferred `form` class |
| Type-locking Deferred components | Keep Env/Action/State params |
| `apply` used as configure-lambda | Named fluent methods; `apply` = children |
| Nesting `SideBar` inside `HolyGrail.left` | `modLeft(_ => SideBar.empty.…)` |
| `TopBar.barHeight` + `HolyGrail.topHeight` both set | Only shell row height under HolyGrail |
| Manual `PageMessagesBottomCorner` under HolyGrail | Rely on default include; use `PageMessages` API only |
| `ZIO.scoped { PageLock.withPageLock { … } }` | Just `PageLock.withPageLock { … }` |
| Hand-rolled style sheet subset | `coreOxygenStyleSheets ++ …` |
| Fully-qualified paths everywhere | Import the package |
| Swallowing errors in submit | `toUI` / page messages / form validation errors |
| Full-width time/date blowouts | Intrinsic width + `alignItems.flexStart` parents |
| “Finishing” WIP (`InfiniteScroll`, `Memo`) unprompted | Leave alone or add TODO only |

---

## Testing expectations

- Prefer pure model tests for date/time cells, decoders, color normalize.  
- UI compile is the minimum bar for example pages.  
- Do not add screenshot/E2E frameworks unless the repo already has them.

---

## Where to read next

| Doc | Why |
| --- | --- |
| [Forms tutorial](forms.md) | Correct submit wiring |
| [Migration](migration-forms.md) | Port old snippets |
| [Builders](builders.md) | Deferred rules, packages, app boot, known gaps |

If something is ambiguous, **prefer the existing example app** (`example/apps/ui` login/register/showcase) over inventing a new pattern.
)
