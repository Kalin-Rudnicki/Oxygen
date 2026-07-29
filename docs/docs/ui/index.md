# Oxygen UI

Oxygen UI (`oxygen-ui-web`) is a Scala.js UI framework built on ZIO. Pages, widgets, and forms are typed values — not string templates or untyped virtual DOM soup.

## What you get

| Layer | Purpose |
| ----- | ------- |
| **Pages** | Routable / non-routable screens with typed params and state |
| **Widgets (`PWidget`)** | Deferred, composable UI trees with `Env` / `Action` / `State` |
| **Forms (`PForm`)** | Value extraction + validation + submit actions on top of widgets |
| **Builders** | HolyGrail-style immutable config for chrome (Button, TextField, layout shells) |
| **Style system** | CSS variables from seeds → generated roles (`OxygenColorSystem`, `OxygenThemes`) |

## Docs in this section

| Page | Audience |
| ---- | -------- |
| [Forms tutorial](forms.md) | Building real forms (fields, zip, submit, validation) |
| [Builders & components](builders.md) | Deferred builders, packages, app boot, known gaps |
| [Migration: forms & builders](migration-forms.md) | Breaking API changes from Decorator / FormConfig era |
| [Agent guide](agent-guide.md) | Guardrails for AI agents implementing Oxygen UI |

## Quick start mental model

```text
Page
 └─ layout (HolyGrail / CenteredCard)          // oxygen.ui.web.layout
     ├─ chrome (TopBar, SideBar, …)
     ├─ PageMessagesBottomCorner               // auto-included by HolyGrail — do not add yourself
     └─ body
         └─ PForm  (TextField.form <*> Button.form).onSubmit / handleActionStateful
              ├─ widgets (typed Action = Form.Submit where needed)
              └─ values  (decode + validate on submit)
```

**Do not** invent parallel form systems, raw `<form onsubmit>`, or untyped global state. Stay inside `PWidget` / `PForm` / component builders.

## Imports

```scala
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.layout.* // HolyGrail, CenteredCard
import oxygen.ui.web.defaults.* // coreOxygenStyleSheets
import oxygen.ui.web.service.{ColorMode, Theme}
```

Prefer imports over fully-qualified paths at call sites.

## Minimal app wiring

```scala
object MyUI extends PageApp[MyEnv] {

  override val styleSheets: ArraySeq[StyleSheet] =
    coreOxygenStyleSheets // reset + theme vars + core chrome

  override protected def prePageLoad: RIO[MyEnv & Scope, Unit] =
    ColorMode.applyStoredOrSystem *>
      Theme.applyStoredOrDefault *>
      ColorMode.subscribeCrossTab *>
      Theme.subscribeCrossTab

  override val pages: ArraySeq[RoutablePage[MyEnv]] = ArraySeq(
    HomePage,
    // …
  )

  override def layer: TaskLayer[MyEnv] = …
}
```

Details: [Builders — App bootstrap](builders.md#app-bootstrap).

## Package map (short)

| Package | Role |
| ------- | ---- |
| `component` | Controls + chrome widgets |
| `layout` | Full-page shells (`HolyGrail`, `CenteredCard`) |
| `style` | Theme pack *definitions*, color system, breakpoints |
| `service` | Runtime theme/color apply, window, storage, … |
| `defaults` | `coreOxygenStyleSheets`, default pages |

## Next

- [Forms tutorial](forms.md) — the usual first real task
- [Builders & components](builders.md) — conventions + layout + themes
- [Agent guide](agent-guide.md) — if you are an automated agent
)
