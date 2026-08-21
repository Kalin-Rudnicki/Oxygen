package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * W10-T10: side drawer overlay (left/right). R2: focus trap.
  *
  * TODO (PR #285): eventually follow `PWidget.Deferred` like other components.
  */
object Drawer {

  enum Side { case Left, Right }

  final case class State(open: Boolean = false) {
    def show: State = copy(open = true)
    def hide: State = copy(open = false)
    def toggle: State = copy(open = !open)
  }

  def apply[Env, Action](
      side: Side = Side.Right,
      panelWidth: String = 320.px,
  )(contents: WidgetEAS[Env, Action, State]*): WidgetEAS[Env, Action, State] =
    Widget.state[State].fix { st =>
      if !st.renderTimeValue.open then Widget.empty
      else
        div(
          position.fixed,
          top := 0,
          left := 0,
          width := 100.vw,
          height := 100.dvh, // OFF-427: track the visible viewport on mobile (URL-bar show/hide)
          zIndex := ZIndices.modalBehindPageMessages,
          backgroundColor := S.color.bg.transparent,
          // dim backdrop via pseudo not available — use nested full-size dark layer
          div(
            position.absolute,
            top := 0,
            left := 0,
            width := 100.pct,
            height := 100.pct,
            backgroundColor := "rgba(0,0,0,0.45)",
            onClick := st.update(_.hide),
          ),
          div(
            position.absolute,
            top := 0,
            (side match {
              case Side.Left  => left := 0
              case Side.Right => right := 0
            }),
            width := panelWidth,
            height := 100.pct,
            backgroundColor := S.color.bg.layerOne,
            boxShadow := "0 0 24px rgba(0,0,0,0.25)",
            padding := S.spacing._4,
            overflowY.auto,
            animation := "oxy-fade-in var(--oxy-motion-duration-fast) var(--oxy-motion-easing-enter) both",
            onClick.e.handle { e =>
              e.stopPropagation()
              ZIO.unit
            },
            div(
              display.flex,
              justifyContent.flexEnd,
              Button().iconOnly(Icon.x).small.minimal.content(onClick := st.update(_.hide)),
            ),
            Widget.fragment(contents*),
          ),
        )
    }

}
