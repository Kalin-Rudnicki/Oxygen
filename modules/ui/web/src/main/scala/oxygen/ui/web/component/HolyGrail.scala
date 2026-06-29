package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.*

final case class HolyGrail[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _cache: HolyGrail.Cache,
    private val _top: Widget.Polymorphic[Env, Action, StateGet, StateSet], // TODO (KR) : special type
    private val _left: SideBar[Env, Action, StateGet, StateSet],
    private val _center: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val _right: SideBar[Env, Action, StateGet, StateSet],
    private val _bottom: Widget.Polymorphic[Env, Action, StateGet, StateSet], // TODO (KR) : special type
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  // TODO (KR) : inline def with lenses
  private def getCached[A](current: A, updated: A, newCache: => HolyGrail.Cache): HolyGrail.Cache =
    if current == updated then _cache
    else newCache

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    if _cache.showAny then
      div(
        //
        display.grid,
        height := 100.vh,
        width := 100.vw,
        minHeight := 0, // needed?
        minWidth := 0, // needed?
        //
        gridTemplateAreas := _cache.gridTemplateAreas,
        gridTemplateRows := _cache.gridTemplateRows,
        gridTemplateColumns := _cache.gridTemplateColumns,
        //
        Widget.when(_cache.showTop) { div(gridArea := "top-bar", _top) },
        Widget.when(_cache.showLeft) { div(gridArea := "left-bar", _left) },
        div(gridArea := "center", _center),
        Widget.when(_cache.showRight) { div(gridArea := "right-bar", _right) },
        Widget.when(_cache.showBottom) { div(gridArea := "bottom-bar", _bottom) },
      )
    else
      div(
        height := 100.vw,
        width := 100.vh,
        minHeight := 0, // needed?
        minWidth := 0, // needed?
        _center,
      )

  ///////  ///////////////////////////////////////////////////////////////

  def showLeft(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showLeft = s))
  def showRight(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showRight = s))
  def showTop(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showTop = s))
  def showBottom(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showBottom = s))

  def leftWidth(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showLeft = true, leftSize = s))
  def rightWidth(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showRight = true, rightSize = s))
  def topHeight(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showTop = true, topSize = s))
  def bottomHeight(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showBottom = true, bottomSize = s))

  def topLeft(t: CornerType.TopLeft): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(topLeft = t))
  def topRight(t: CornerType.TopRight): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(topRight = t))
  def bottomLeft(t: CornerType.BottomLeft): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(bottomLeft = t))
  def bottomRight(t: CornerType.BottomRight): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(bottomRight = t))

  ///////  ///////////////////////////////////////////////////////////////

  def modLeft[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: SideBar[Env, Action, StateGet, StateSet] => SideBar[Env2, Action2, StateGet2, StateSet2],
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showLeft, true, _cache.copy(showLeft = true)), _left = f(_left))

  def modRight[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: SideBar[Env, Action, StateGet, StateSet] => SideBar[Env2, Action2, StateGet2, StateSet2],
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showRight, true, _cache.copy(showRight = true)), _right = f(_right))

  def modTop[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: Widget.Polymorphic[Env, Action, StateGet, StateSet] => Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showTop, true, _cache.copy(showTop = true)), _top = f(_top))

  def modCenter[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: Widget.Polymorphic[Env, Action, StateGet, StateSet] => Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    copy(_center = f(_center))

  def modBottom[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      f: Widget.Polymorphic[Env, Action, StateGet, StateSet] => Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    copy(_cache = getCached(_cache.showBottom, true, _cache.copy(showBottom = true)), _bottom = f(_bottom))

  ///////  ///////////////////////////////////////////////////////////////

  // =====|  |=====

  def left[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modLeft(_.middle(addChildren*))

  def leftRoot[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modLeft(_.root(addChildren*))

  def leftTop[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modLeft(_.top(addChildren*))

  def leftMiddle[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modLeft(_.middle(addChildren*))

  def leftBottom[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modLeft(_.bottom(addChildren*))

  // =====|  |=====

  def right[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modRight(_.middle(addChildren*))

  def rightRoot[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modRight(_.root(addChildren*))

  def rightTop[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modRight(_.top(addChildren*))

  def rightMiddle[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modRight(_.middle(addChildren*))

  def rightBottom[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modRight(_.bottom(addChildren*))

  // =====|  |=====

  def top[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modTop { fragment(_, Widget.fragment(addChildren)) }

  // =====|  |=====

  def center[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modCenter { fragment(_, Widget.fragment(addChildren)) }

  // =====|  |=====

  def bottom[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): HolyGrail[Env2, Action2, StateGet2, StateSet2] =
    modBottom { fragment(_, Widget.fragment(addChildren)) }

}
object HolyGrail {

  final case class Cache(
      showLeft: Boolean,
      showRight: Boolean,
      showTop: Boolean,
      showBottom: Boolean,
      leftSize: String,
      rightSize: String,
      topSize: String,
      bottomSize: String,
      topLeft: CornerType.TopLeft,
      topRight: CornerType.TopRight,
      bottomLeft: CornerType.BottomLeft,
      bottomRight: CornerType.BottomRight,
  ) {

    val showAny: Boolean =
      showLeft || showRight || showTop || showBottom

    val gridTemplateAreas: String = util.dynamicTemplateAreas(
      showTop -> Seq(showLeft -> (topLeft.sideValue + "-bar"), true -> "top-bar", showRight -> (topRight.sideValue + "-bar")),
      true -> Seq(showLeft -> "left-bar", true -> "center", showRight -> "right-bar"),
      showBottom -> Seq(showLeft -> (bottomLeft.sideValue + "-bar"), true -> "bottom-bar", showRight -> (bottomRight.sideValue + "-bar")),
    )

    val gridTemplateColumns: String = util.dynamicTemplateSizes(
      showLeft -> leftSize,
      true -> "1fr",
      showRight -> rightSize,
    )

    val gridTemplateRows: String = util.dynamicTemplateSizes(
      showTop -> topSize,
      true -> "1fr",
      showBottom -> bottomSize,
    )

  }
  object Cache {

    val default: Cache =
      Cache(
        showLeft = false,
        showRight = false,
        showTop = false,
        showBottom = false,
        leftSize = 250.px,
        rightSize = 250.px,
        topSize = 40.px,
        bottomSize = 40.px,
        topLeft = Side.Top,
        topRight = Side.Top,
        bottomLeft = Side.Left,
        bottomRight = Side.Right,
      )

  }

  def apply(
  ): HolyGrail[Any, Nothing, Any, Nothing] =
    new HolyGrail(
      _cache = Cache.default,
      _top = Widget.empty,
      _left = SideBar(),
      _center = Widget.empty,
      _right = SideBar(),
      _bottom = Widget.empty,
    )

}
