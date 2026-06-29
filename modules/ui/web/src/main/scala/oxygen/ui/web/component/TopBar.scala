package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

final case class TopBar[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _left: Seq[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
    private val _right: Seq[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {
  import TopBar.*

  def left[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_left = _left ++ addChildren)

  def right[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_right = _right ++ addChildren)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    bar(
      shrinkSection(_left*),
      growSection,
      shrinkSection(_right*),
    )

}
object TopBar {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val bar: Node =
    div(
      width := 100.pct,
      height := 100.pct,
      display.flex,
      backgroundColor := S.color.brand.primary1,
    )

  private val shrinkSection: Node =
    div(
      height := 100.pct,
      flexGrow := 0,
      flexShrink := 1,
    )

  private val growSection: Node =
    div(
      height := 100.pct,
      flexGrow := 1,
      flexShrink := 0,
    )

  val item: Node =
    div(
      height := 100.pct,
      cursor.pointer,
      userSelect.none,
      display.inlineFlex,
      justifyContent.center,
      alignItems.center,
      padding := "0 1rem",
      fontSize := OxygenStyleVars.fontSize._5,
      backgroundColor.dynamic.hover := S.color.brand.primary1.dark.getColorValue.darken(15),
      backgroundColor.dynamic.hoverActive := S.color.brand.primary1.dark.getColorValue.darken(30),
    )

  val indexItem: Widget =
    item(
      color := S.color.brand.primary2.light,
      fontSize := S.fontSize._7,
      fontWeight := S.fontWeight.bold,
    )

}
