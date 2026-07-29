# Forms in Oxygen UI

Forms are a **first-class algebra** (`PForm`), not HTML `<form>` tags and not layout builders.

A form is three things:

1. **Widget tree** — what the user sees and interacts with  
2. **Value extraction** — how page/widget state becomes a typed `Value`  
3. **Actions** — especially `Form.Submit`, raised by fields and submit buttons  

On submit, Oxygen runs validation, surfaces field errors, and only then runs your effect.

---

## The form builder pattern

Every form-capable component exposes a nested **`form` class** on its companion:

```scala
TextField.form[Email]("Email")   // → TextField.form[Email]
  .email                         // chrome / field helpers
  .width(300.px)
  .required                      // PForm algebra (after Option value)
  .zoomOut[PageState](_.email)   // lens into page state
```

### Why a class (not `configure: FormConfig => FormConfig`)

| Goal | How |
| ---- | --- |
| Keep a **typed child** of the outer control | `modField(f: TextField => TextField)` |
| Compose chrome without losing types | `modLabel`, `describe`, `width`, … |
| Stay a real form until used | extends **`PForm.Deferred`** |
| Zip / submit after fluent config | `<*>`, `onSubmit`, `handleActionStateful` |

`PForm.Deferred` works like `PWidget.Deferred`: configuration is a value; **build happens when the form is rendered or handled**.

---

## Anatomy of a field form

```scala
final case class form[+Value] private (
    // field name for validation messages
    // typed child control (TextField / Dropdown / …)
    // Label chrome
    // decode / extract
) extends PForm.Deferred[Env, Action, State, State, Value]
```

### Drill-down helpers (always prefer these)

| Helper | Meaning |
| ------ | ------- |
| `modField` / `modButton` / `modDropdown` / `modRadio` / `modCheckbox` / `modToggle` | Transform the **inner control** builder |
| `modLabel` | Transform the **Label** chrome |
| `describe` / `labelMod` / `width` / `surroundingPadding` | Common chrome shortcuts |
| shortcuts like `.email`, `.medium`, `.h12` | Delegates into the child |

You can always fall through to `modX` when a shortcut is missing:

```scala
TextField.form[String]("Notes").modField(_.extraLarge.noTrimInput)
Dropdown.form[Status]("Status").modDropdown(_.negative.closeOnMouseLeave)
```

---

## Components with `form`

| Component | Value type | Raises `Form.Submit`? | Drill helper |
| --------- | ---------- | --------------------- | ------------ |
| `TextField.form[A]` | `Option[A]` (empty → None) | **Enter** | `modField` |
| `TextArea.form[A]` | `Option[A]` | **Ctrl+Enter** | `modField` |
| `Button.form("…")` | `Unit` | **Click** | `modButton` |
| `Dropdown.form[A]` | `Option[A]` selected | — | `modDropdown` |
| `HorizontalRadio.form[A]` | `A` selected | — | `modRadio` |
| `Checkbox.form` | `Boolean` | — | `modCheckbox` |
| `ToggleThumb.form` | `Boolean` | — | `modToggle` |
| `DatePicker.form` | `Option[LocalDate]` | — | `picker` / `modLabel` |
| `TimePicker.form` | `LocalTime` | — | `mode` / `h12` / `h24` / `modLabel` |
| `DateTimePicker.form` | `Option[LocalDateTime]` | — | `h12` / `h24` / `date` / `time` / `modLabel` |
| `ColorPicker.form` | `ColorPicker.State` | — | `modLabel` |
| `FileDropZone.form` | `List[String]` names | — | `prompt` / `modZone` / `modLabel` |

`TextField.rawForm` / bare `.widget` still exist for advanced composition without label chrome.

---

## Building a login form

```scala
(
  TextField
    .form[Email]("Email")
    .email
    .width(300.px)
    .required
    .zoomOut[PageState](_.email) <*>
  TextField
    .form[String]("Password")
    .password
    .width(300.px)
    .required
    .zoomOut[PageState](_.password) <*>
  Button.form("Login").medium
).handleActionStateful { case (_, (email, password)) =>
  // Effect runs only if validation succeeds
  PageLock.withPageLock {
    UserApi.login(LoginRequest(email, Password.PlainText.wrap(password))).toUILogged(_.toUI)
  }
}
```

### Alternative: `onSubmit` when Action is only `Form.Submit`

```scala
registerForm.onSubmit { (_, req: RegisterRequest) =>
  UserApi.register(req).toUILogged(_.toUI) *> HomePage.navigate.push(())
}
```

### For-comprehension assembly

`flatMap` / `map` on stateful forms let you assemble widgets + values explicitly (see example register page):

```scala
for {
  (emailW, emailV) <- TextField.form[Email]("Email").email.required.zoomOut[S](_.email)
  (submitW, _)     <- Button.form("Sign Up").medium
} yield (
  fragment(emailW, submitW),
  emailV.mapValue(RegisterRequest.emailOnly), // illustrative
)
```

---

## Core algebra cheat sheet

| Op | Role |
| -- | ---- |
| `<*>` / `<*` / `*>` | Zip form values (and combine widgets). `Unit` from `Button.form` zips away via `Zip`, so field values stay a clean tuple |
| `zoomOut[Outer](_.field)` | Lens form state into page state |
| `required` | `Option[A]` → `A`, missing → validation error |
| `mapValue` / `mapOrFail` / `validateValue` | Transform / validate the extracted value |
| `.widget` | Drop to widget only (still carries Action) |
| `discardAction` | Drop Action (demo-only; **kills Enter submit**) |
| `handleActionStateful` / `onSubmit` | Run effects on Action (usually `Form.Submit`) |
| `PageLock.withPageLock` | Disable double-submit (pairs with `Button.form` lock-aware). **Self-scoping** — do not wrap in extra `ZIO.scoped` |

---

## Enter key behaviour

| Control | Enter |
| ------- | ----- |
| TextField | raises `Form.Submit` |
| TextArea | **Ctrl+Enter** raises `Form.Submit` (Enter = newline) |
| Button.form | click (or keyboard activate when focused) |
| Other pickers | no automatic Submit |

There is **no** browser-native form submit. If Enter “does nothing”:

1. You used `.widget.discardAction` (showcase demos often do), or  
2. No `onSubmit` / `handleActionStateful` is wired, or  
3. Focus is on a control that does not raise Submit  

---

## Validation

- Field names on forms become keys in `UIError.ClientSide.FormValidationErrors`.  
- Empty optional text fields decode as `None`; use `.required` when the field is mandatory.  
- `StringDecoder` powers `TextField.form[A]` / `TextArea.form[A]` (e.g. `Email`, `String`, custom).

```scala
TextField.form[Email]("Email").email.required // missing or invalid → form error, no effect
```

---

## State placement

Prefer **page state** fields with `zoomOut`:

```scala
final case class PageState(
  email: TextField.State,
  password: TextField.State,
)

TextField.form[Email]("Email").zoomOut[PageState](_.email)
```

Do not store decoded domain values as the only live input state while typing — keep control state (`TextField.State`, `Dropdown.State[A]`, …) and decode on submit.

---

## Page lock notes

```scala
// Correct — lock scopes the effect
PageLock.withPageLock {
  apiCall.toUILogged(_.toUI)
}

// Wrong — redundant outer scope
ZIO.scoped {
  PageLock.withPageLock { … }
}
```

`Button.form` is lock-aware by default (disables + shows progress while the page is locked). Use `.noLockAware` only when you intentionally want a free submit button.

!!! note "Button.form type params"

    Today `Button.form` wraps a `Button.Const` internally. A fully polymorphic submit button (Env/Action/State on the form) is a known follow-up — fine for normal forms; leave a TODO if you hit the edge.

---

## What not to do

- Do **not** wrap Oxygen controls in a raw HTML `<form>` and expect native submit.  
- Do **not** reimplement validation with ad-hoc `Option` checks outside `PForm` when form algebra already covers it.  
- Do **not** `discardAction` on production submit paths.  
- Do **not** rebuild FormConfig-lambda APIs; use the deferred `form` class.  
- Do **not** wrap `PageLock.withPageLock` in an extra `ZIO.scoped`.  

See also: [Migration guide](migration-forms.md), [Agent guide](agent-guide.md), [Builders](builders.md).
)
