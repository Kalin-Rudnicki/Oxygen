package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{OxygenStyleVars as S, *, given}

final case class SideBar[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _cache: SideBar.Cache,
    private val _root: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val _top: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val _middle: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val _bottom: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  // TODO (KR) : inline def with lenses
  private def getCached[A](current: A, updated: A, newCache: => SideBar.Cache): SideBar.Cache =
    if current == updated then _cache
    else newCache

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    div(
      display.grid,
      height := 100.pct,
      width := 100.pct,
      minHeight := 0,
      minWidth := 0,
      //
      gridTemplateAreas := _cache.gridTemplateAreas,
      gridTemplateRows := _cache.gridTemplateRows,
      gridTemplateColumns := _cache.gridTemplateColumns,
      //
      _root,
      Widget.when(_cache.showTop) { div(gridArea := "side-bar-top", _top) },
      div(gridArea := "side-bar-middle", _middle),
      Widget.when(_cache.showBottom) { div(gridArea := "side-bar-bottom", _bottom) },
    )

  def root[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): SideBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_root = fragment(this._root, Widget.fragment(addChildren)))

  def top[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): SideBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showTop, true, _cache.copy(showTop = true)), _top = fragment(this._top, Widget.fragment(addChildren)))

  def middle[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): SideBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_middle = fragment(this._middle, Widget.fragment(addChildren)))

  def bottom[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): SideBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showBottom, true, _cache.copy(showBottom = true)), _bottom = fragment(this._bottom, Widget.fragment(addChildren)))

}
object SideBar {

  def basicItem(selected: Boolean): Node =
    div(
      borderBottom(1.px, S.color.fg.default),
      Widget.when(selected) {
        fragment(
          backgroundColor.dynamic := S.color.primary.strong,
          color.dynamic := S.color.fg.inverse,
        )
      },
      backgroundColor.dynamic.hover := S.color.primary,
      color.dynamic.hover := S.color.fg.inverse,
      userSelect.none,
      cursor.pointer,
      padding(S.spacing._2, S.spacing._3),
    )

  final case class Cache(
      showTop: Boolean,
      showBottom: Boolean,
      topSize: String,
      bottomSize: String,
  ) {

    val showAny: Boolean =
      showTop || showBottom

    val gridTemplateAreas: String = util.dynamicTemplateAreas(
      showTop -> Seq(true -> "side-bar-top"),
      true -> Seq(true -> "side-bar-middle"),
      showBottom -> Seq(true -> "side-bar-bottom"),
    )
    val gridTemplateRows: String = util.dynamicTemplateSizes(
      showTop -> topSize,
      true -> "1fr",
      showBottom -> bottomSize,
    )
    val gridTemplateColumns: String = "1fr"

  }
  object Cache {

    val default: Cache =
      Cache(
        showTop = false,
        showBottom = false,
        topSize = "auto",
        bottomSize = "auto",
      )

  }

  def apply(
  ): SideBar[Any, Nothing, Any, Nothing] =
    new SideBar(
      _cache = Cache.default,
      _root = Widget.empty,
      _top = fragment(
        padding(S.spacing._2, S.spacing._3),
      ),
      _middle = fragment(
        O.Scrollable,
      ),
      _bottom = fragment(
        padding(S.spacing._2, S.spacing._3),
      ),
    )

}
