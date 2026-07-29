package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W6-T04: deferred / expand-gated content.
  *
  * Body is only built when open — skip expensive widgets until the user expands.
  * Use for accordion, “show advanced”, below-the-fold panels (not code-splitting).
  *
  * {{{
  * // page state includes LazySection.State
  * LazySection.panel("Advanced settings")(expensiveFields)
  * // or controlled against a Boolean field:
  * LazySection(title, _.showAdv, s => s.copy(showAdv = !s.showAdv))(body)
  * }}}
  */
object LazySection {

  final case class State(open: Boolean = false) {
    def toggle: State = copy(open = !open)
    def expand: State = copy(open = true)
    def collapse: State = copy(open = false)
  }

  /**
    * Controlled panel against parent state `S`.
    * `body` is by-name: not evaluated while collapsed.
    */
  def apply[Env, Action, S](
      title: String,
      isOpen: S => Boolean,
      toggle: S => S,
  )(body: => WidgetEAS[Env, Action, S]): WidgetEAS[Env, Action, S] =
    Widget.state[S].fix { st =>
      val open = isOpen(st.renderTimeValue)
      div(
        border := s"1px solid ${S.color.bg.layerThree}",
        borderRadius := S.borderRadius._3,
        overflow.hidden,
        div(
          display.flex,
          alignItems.center,
          justifyContent.spaceBetween,
          padding := S.spacing._3,
          backgroundColor := S.color.bg.layerTwo,
          cursor.pointer,
          onClick := st.update(toggle),
          span(title, fontWeight := "600", color := S.color.fg.default),
          span(if open then "▾" else "▸", color := S.color.fg.moderate),
        ),
        if open then
          div(
            padding := S.spacing._4,
            backgroundColor := S.color.bg.layerOne,
            body,
          )
        else Widget.empty,
      )
    }

  /** Panel whose open flag is [[State]] itself (zoom / nest into page state). */
  def panel[Env, Action](title: String)(body: => WidgetEA[Env, Action]): WidgetEAS[Env, Action, State] =
    apply[Env, Action, State](title, _.open, _.toggle)(body.fixState[State])

}
