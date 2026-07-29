package oxygen.ui.web.component

import monocle.Lens
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.internal.LensUtil

/**
  * Lens-based tabs: selection lives on page state `S` via [[lens]]; each tab panel
  * is a normal stateful widget over the same `S` (no `Int =>` panel callback / zoomOut).
  * Selected tab is identified by string id.
  *
  * {{{
  *   Tabs.empty[PageState](_.tabs)(
  *     Tabs.Tab("gen", "General")( ... ),
  *     Tabs.Tab("sec", "Security")( ... ),
  *   )
  * }}}
  */
final class Tabs[-Env, +Action, S] private (
    private val lens: Lens[S, Tabs.State],
    private val tabs: ArraySeq[Tabs.Tab.Stateful[Env, Action, S]],
) extends PWidget.Deferred.Stateful[Env, Action, S] {

  def apply[Env2 <: Env, Action2 >: Action](
      children: Tabs.Tab.Stateful[Env2, Action2, S]*,
  ): Tabs[Env2, Action2, S] =
    new Tabs[Env2, Action2, S](lens, tabs :++ children)

  override protected def build: PWidget.Stateful[Env, Action, S] =
    Widget.state[S].fixGet { (ws, s) =>
      val tabState: Tabs.State = lens.get(s)
      val selectedTab: Option[Tabs.Tab.Stateful[Env, Action, S]] = tabs.find(_.id == tabState.selected)

      div(
        width := 100.pct,
        // strip
        div(
          display.flex,
          flexWrap.wrap,
          gap := S.spacing._1,
          borderBottom(1.px, "solid", S.color.bg.layerThree),
          marginBottom := S.spacing._4,
          Widget.foreach(tabs) { tab =>
            val active: Boolean = tabState.selected == tab.id
            button(
              O.Button,
              padding(S.spacing._2, S.spacing._4),
              borderStyle := "solid",
              borderWidth := "0",
              borderBottomWidth := 2.px,
              borderBottomColor := (if active then S.color.primary.standard else S.color.bg.transparent),
              borderRadius := "0",
              backgroundColor := S.color.bg.transparent,
              color := (if active then S.color.primary.standard else S.color.fg.moderate),
              // Constant weight: active 600 vs idle 500 reflows the strip (awkward micro-resize).
              // Selection is carried by color + underline only.
              fontWeight := "500",
              cursor.pointer,
              onClick := ws.update(lens.modify(_.select(tab.id))),
              tab.label,
            )
          },
        ),
        // panel — only the selected tab is built
        Widget.foreach(selectedTab) { selected =>
          div(
            padding := S.spacing._2,
            selected,
          )
        },
      )
    }

}
object Tabs {

  final case class State(selected: String) {
    def select(s: String): State = copy(selected = s)
  }
  object State {
    val empty: State = State("")
    def initial(s: String): State = State(s)
  }

  def emptyLens[S](lens: Lens[S, Tabs.State]): Tabs[Any, Nothing, S] =
    new Tabs[Any, Nothing, S](
      lens = lens,
      tabs = ArraySeq.empty,
    )

  inline def empty[S](inline f: S => Tabs.State): Tabs[Any, Nothing, S] =
    emptyLens(LensUtil.genLens(f))

  /**
    * A single tab: id + label + optional panel content (Deferred).
    * [[apply]] appends panel children when using the fixed-panel API.
    */
  final case class Tab[-Env, +Action, -StateGet, +StateSet <: StateGet](
      id: String,
      label: String,
      private val _panel: Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
    ): Tab[Env2, Action2, StateGet2, StateSet2] =
      copy(_panel = fragment(_panel, Widget.fragment(children)))

    override protected def build: PWidget[Env, Action, StateGet, StateSet] = _panel

  }
  object Tab extends WidgetTypes[Tab] {

    def apply(id: String, label: String): Tab.Const =
      Tab(id, label, Widget.empty)

  }

}
