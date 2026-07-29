package oxygen.ui.web.layout

import oxygen.ui.web.*
import oxygen.ui.web.component.{util, CornerType, PageMessagesBottomCorner, Side, SideBar}
import oxygen.ui.web.create.{*, given}

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

  private def centerSlot: Widget.Polymorphic[Env, Action, StateGet, StateSet] = {
    val scrollClass: Widget =
      if _cache.centerScrollable then OxygenStyleSheet.Scrollable else Widget.empty
    div(
      Widget.`class`("oxy-holy-grail-center"),
      gridArea := "center",
      minWidth := 0,
      minHeight := 0,
      height := 100.pct,
      // Contain scroll in the center pane so the shell (top/side) stays fixed.
      scrollClass,
      _center,
    )
  }

  private def pageMessagesSlot: Widget =
    Widget.when(_cache.includePageMessages)(PageMessagesBottomCorner.default)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    val shell: Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      if _cache.showAny then
        div(
          Widget.`class`("oxy-holy-grail"),
          //
          display.grid,
          height := 100.vh,
          width := 100.vw,
          minHeight := 0,
          minWidth := 0,
          overflow.hidden,
          //
          gridTemplateAreas := _cache.gridTemplateAreas,
          gridTemplateRows := _cache.gridTemplateRows,
          gridTemplateColumns := _cache.gridTemplateColumns,
          //
          Widget.when(_cache.showTop) { div(Widget.`class`("oxy-holy-grail-top"), gridArea := "top-bar", flexShrink := "0", _top) },
          Widget.when(_cache.showLeft) {
            div(Widget.`class`("oxy-holy-grail-left"), gridArea := "left-bar", minHeight := 0, overflow.hidden, _left)
          },
          centerSlot,
          Widget.when(_cache.showRight) {
            div(Widget.`class`("oxy-holy-grail-right"), gridArea := "right-bar", minHeight := 0, overflow.hidden, _right)
          },
          Widget.when(_cache.showBottom) { div(Widget.`class`("oxy-holy-grail-bottom"), gridArea := "bottom-bar", _bottom) },
        )
      else
        div(
          Widget.`class`("oxy-holy-grail", "oxy-holy-grail--center-only"),
          height := 100.vh,
          width := 100.vw,
          minHeight := 0,
          minWidth := 0,
          overflow.hidden,
          if _cache.centerScrollable then OxygenStyleSheet.Scrollable else Widget.empty,
          _center,
        )

    // Page messages are position:fixed — sit as a shell sibling, not inside the scrollable center.
    fragment(shell, pageMessagesSlot)
  }

  ///////  ///////////////////////////////////////////////////////////////

  def showLeft(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showLeft = s))
  def showRight(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showRight = s))
  def showTop(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showTop = s))
  def showBottom(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showBottom = s))

  def leftWidth(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showLeft = true, leftSize = s))
  def rightWidth(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showRight = true, rightSize = s))
  def topHeight(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showTop = true, topSize = s))
  def bottomHeight(s: String): HolyGrail[Env, Action, StateGet, StateSet] = copy(_cache = _cache.copy(showBottom = true, bottomSize = s))

  /** Center pane scrolls (default). Shell chrome stays fixed. */
  def scrollableCenter: HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(centerScrollable = true))

  /** Disable center scroll — page/document may scroll instead (or caller manages overflow). */
  def noScrollableCenter: HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(centerScrollable = false))

  def centerScrollable(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(centerScrollable = s))

  /** Include [[PageMessagesBottomCorner.default]] (on by default). */
  def pageMessages: HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(includePageMessages = true))

  /** Opt out of auto-included page messages. */
  def noPageMessages: HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(includePageMessages = false))

  def includePageMessages(s: Boolean): HolyGrail[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(includePageMessages = s))

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
object HolyGrail extends WidgetTypes[HolyGrail] {

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
      /** When true (default), center region is [[OxygenStyleSheet.Scrollable]]. */
      centerScrollable: Boolean,
      /** When true (default), auto-includes [[PageMessagesBottomCorner.default]] as a fixed overlay. */
      includePageMessages: Boolean,
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
        centerScrollable = true,
        includePageMessages = true,
      )

  }

  val empty: HolyGrail.Const =
    new HolyGrail(
      _cache = Cache.default,
      _top = Widget.empty,
      _left = SideBar(),
      _center = Widget.empty,
      _right = SideBar(),
      _bottom = Widget.empty,
    )

  def apply(): HolyGrail.Const = empty

  /**
    * W5-T04: below `md`, hide left/right sidebars and force a single-column shell
    * so fixed desktop grids don't blow up mobile. Drawer/hamburger = Round 2 / TopBar todo.
    *
    * Also: slightly larger root type + touch-friendlier chrome on narrow viewports
    * (pairs with viewport meta — without `width=device-width` phones still look desktop-y).
    */
  val responsiveSheet: StyleSheet =
    MediaCSS.styleSheet("holy-grail-responsive")(
      MediaCSS.belowMd(
        """
          |.oxy-holy-grail {
          |  grid-template-columns: 1fr !important;
          |  grid-template-areas:
          |    "top-bar"
          |    "center"
          |    "bottom-bar" !important;
          |  height: auto;
          |  min-height: 100dvh;
          |  min-height: 100vh;
          |  width: 100%;
          |}
          |.oxy-holy-grail-left,
          |.oxy-holy-grail-right {
          |  display: none !important;
          |}
          |.oxy-holy-grail-center {
          |  min-width: 0;
          |  min-height: 0;
          |}
          |/* Comfortable phone reading size (rem scales with this). */
          |html {
          |  font-size: 17px;
          |}
          |/* Top bar items: larger tap targets when the rail is gone. */
          |.oxy-holy-grail-top {
          |  min-height: 52px;
          |}
          |/* Showcase / page bodies: less wasted side gutter on narrow screens. */
          |.oxy-page-body {
          |  padding: 1rem !important;
          |  max-width: 100% !important;
          |}
          |""".stripMargin,
      ),
    )

}
