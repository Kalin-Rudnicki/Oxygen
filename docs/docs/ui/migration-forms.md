# Migration: forms & builders

This page covers breaking form APIs introduced with the HolyGrail / deferred-form work.

!!! warning "Breaking"

    `FormConfig` + `configure: FormConfig => FormConfig` factories are **removed**.  
    Nested `object form extends Decorable` is **long gone**.  
    Use the deferred **`Component.form` class** instead.

---

## Why this changed

| Era | Shape | Problem |
| --- | ----- | ------- |
| Decorator | `object form extends Decorable` + `dec: Decorator => Decorator` | Non-composable stacking; dead with Decorator kill |
| FormConfig | `def form(label, configure: FormConfig => FormConfig)` | Config is a parallel bag; type of inner control often lost; not a deferred form value |
| **Current** | `final case class form … extends PForm.Deferred` | Typed child + chrome, fluent after factory, still a real form |

---

## Call-site mapping

### TextField / TextArea

=== "Before (FormConfig)"

    ```scala
    TextField.form[Email]("Email", _.email.width(300.px).describe("Work email"))
    TextField.form[String]("X", _.describe("a\nb").label(_.mod(color.red)))
    TextArea.form[String]("Bio", _.width(100.pct).height(6.rem))
    ```

=== "After (form class)"

    ```scala
    TextField.form[Email]("Email").email.width(300.px).describe("Work email")
    TextField.form[String]("X").describe("a\nb").labelMod(color.red)
    TextArea.form[String]("Bio").width(100.pct).height(6.rem)
    ```

Notes:

- Second-parameter `configure` lambda is **gone**. Chain methods on the returned form.  
- Old `FormConfig.label(f: FormConfig => FormConfig)` was a no-op-ish passthrough; use **`modLabel` / `labelMod`**.  
- Old `FormConfig.mod(…)` for label extras → **`labelMod(…)`**.  
- Old `textField(f)` / `textArea(f)` → **`modField(f)`**.

### Button

=== "Before"

    ```scala
    Button.form("Login", _.button(_.medium))
    Button.form("Save", _.button(_.positive.subtle), lockAware = false)
    ```

=== "After"

    ```scala
    Button.form("Login").medium
    Button.form("Save").positive.subtle.noLockAware
    // or
    Button.form("Save").modButton(_.positive.subtle).lockAware(false)
    ```

### Dropdown

=== "Before"

    ```scala
    Dropdown.form[Status]("Status", _.describe("Pick").dropdown(_.negative))
    Dropdown.form[Status]("Status", _.toString, _.width(24.ch))
    ```

=== "After"

    ```scala
    Dropdown.form[Status]("Status").describe("Pick").modDropdown(_.negative)
    Dropdown.form[Status]("Status", _.toString).width(24.ch)
    // or
    Dropdown.form[Status]("Status").show(_.toString).width(24.ch)
    ```

### HorizontalRadio

=== "Before"

    ```scala
    HorizontalRadio.form[Mode]("Mode", _.horizontalRadio(_.primarySelected).describe("…"))
    ```

=== "After"

    ```scala
    HorizontalRadio.form[Mode]("Mode").modRadio(_.primarySelected).describe("…")
    ```

### Checkbox

=== "Before"

    ```scala
    Checkbox.form("Accept", _.primary)
    ```

=== "After"

    ```scala
    Checkbox.form("Accept").primary
    // or
    Checkbox.form("Accept").modCheckbox(_.primary)
    ```

---

## Semantics that did **not** change

- `zoomOut`, `required`, `<*>`, `onSubmit`, `handleActionStateful` — same  
- `Form.Submit` as the submit Action — same  
- Enter on `TextField` / Ctrl+Enter on `TextArea` — same  
- `PageLock` + lock-aware submit button — same (now `Button.form(…).lockAware` / `noLockAware`)  
- Validation via `StringDecoder` + field names — same  

Related layout / package moves (same overhaul):

- Full-page shells moved to **`oxygen.ui.web.layout`** (`HolyGrail`, `CenteredCard`).  
- Old `PageLayout` is gone — use HolyGrail.  
- Theme packs stay in `style.OxygenThemes`; **apply** via `service.Theme.applyStoredOrDefault`.  
- Prefer `coreOxygenStyleSheets` for the required sheet bundle.

---

## New forms (no prior FormConfig)

These gained deferred `form` companions for labeled use:

- `ToggleThumb.form`  
- `DatePicker.form` / `TimePicker.form` / `DateTimePicker.form`  
- `ColorPicker.form`  
- `FileDropZone.form`  

If you previously wired only `.widget.zoomOut`, you can keep doing that; `form` is optional labeled chrome + `PForm` value.

---

## Showcase / demo pitfall

```scala
// Demo-only: discards Form.Submit — Enter will appear broken
TextField.form[String]("Name").widget.discardAction.zoomOut[S](_.name)
```

Production paths must **keep** the action and handle submit.

---

## Decorator-era reference (historical)

If you still see old snippets:

```scala
// DEAD — do not revive
object form extends Decorable {
  def apply[A](label: String, decorator: Decorator => Decorator = identity): SubmitFormS[…]
}
TextField.form[A]("Email", _.textField(_.email).label(_.describe("…")))
```

There is no compatibility shim. Port to deferred `form` classes.

---

## Checklist

- [ ] Replace every `form(label, configure)` with `form(label).…`  
- [ ] Replace `.button(f)` / `.textField(f)` / `.dropdown(f)` / `.horizontalRadio(f)` with `modButton` / `modField` / `modDropdown` / `modRadio`  
- [ ] Replace FormConfig `mod` / nested `label(_.mod)` with `labelMod` / `modLabel`  
- [ ] Confirm submit handlers still receive Actions (no accidental `discardAction`)  
- [ ] Recompile; fix any leftover `FormConfig` references  
)
