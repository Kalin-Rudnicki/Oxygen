package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}

object PageBodies {

  object centeredCard {

    def apply[Env, Action, StateGet, StateSet <: StateGet](
        content: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
    ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      fragment(
        OxygenStyleSheet.CenteredCardPage,
        div(
          OxygenStyleSheet.CenteredCardPage.Card,
          Widget.fragment(content),
        ),
      )

  }

}
