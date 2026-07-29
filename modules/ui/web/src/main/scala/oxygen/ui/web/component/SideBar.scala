package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{OxygenStyleVars as S, *, given}

/**
  * Vertical rail slots for [[oxygen.ui.web.layout.HolyGrail]] (W2-T13).
  * Builder-style: Cache + top/middle/bottom slots; pure CSS vars only.
  *
  * {{{
  * SideBar()
  *   .top("Brand")
  *   .middle(navItems*)
  *   .bottom(userMenu)
  *   .surface
  * }}}
  */
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
      backgroundColor := _cache.bg,
      //
      gridTemplateAreas := _cache.gridTemplateAreas,
      gridTemplateRows := _cache.gridTemplateRows,
      gridTemplateColumns := _cache.gridTemplateColumns,
      //
      _root,
      Widget.when(_cache.showTop) { div(gridArea := "side-bar-top", _top) },
      div(gridArea := "side-bar-middle", minHeight := 0, _middle),
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

  def bg(color: String): SideBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(bg = color))

  /** Layer-one surface rail. */
  def surface: SideBar[Env, Action, StateGet, StateSet] =
    bg(S.color.bg.layerOne)

  /** Brand-tinted rail. */
  def brand: SideBar[Env, Action, StateGet, StateSet] =
    bg(S.color.brand.primary1)

}
object SideBar extends WidgetTypes[SideBar] {

  def basicItem(selected: Boolean): Node =
    div(
      borderBottom(1.px, S.color.fg.default),
      Widget.when(selected) {
        fragment(
          backgroundColor.dynamic := S.color.primary.standard,
          color.dynamic := S.color.primary.on,
        )
      },
      backgroundColor.dynamic.hover := S.color.primary.subtle,
      color.dynamic.hover := S.color.primary.standard,
      userSelect.none,
      cursor.pointer,
      // Default nav density — slightly larger than body microcopy for scanability.
      fontSize := S.fontSize._3,
      padding(S.spacing._2, S.spacing._3),
      O.WrapText,
    )

  final case class Cache(
      showTop: Boolean,
      showBottom: Boolean,
      topSize: String,
      bottomSize: String,
      bg: String,
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
        bg = S.color.bg.layerOne,
      )

  }

  val empty: SideBar.Const =
    SideBar(
      _cache = Cache.default,
      _root = fragment(
        backgroundColor := Cache.default.bg,
        color := S.color.fg.default,
        minWidth := 0,
        minHeight := 0,
      ),
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

  def apply(): SideBar.Const = empty

}
