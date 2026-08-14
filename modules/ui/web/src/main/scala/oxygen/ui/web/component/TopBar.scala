package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.http.URL

/**
  * App top chrome (W2-T13 shell; typically nested under [[oxygen.ui.web.layout.HolyGrail]]).
  *
  * Prefer this over deprecated [[NavBar]]. Slot APIs: `left` / `right` items;
  * tokens via [[TopBar.Cache]] (`bg`, hover/active). Icons: [[Item.withIcon]].
  *
  * {{{
  * TopBar.empty
  *   .barHeight(48.px) // optional; omit under HolyGrail (grid row owns height)
  *   .left(TopBar.item("Home").onClickPush(HomePage))
  *   .right(TopBar.item.withIcon(Icon.settings, "Settings"))
  * }}}
  */
final case class TopBar[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _cache: TopBar.Cache,
    private val _left: Seq[TopBar.Item[Env, Action, StateGet, StateSet]],
    private val _right: Seq[TopBar.Item[Env, Action, StateGet, StateSet]],
    private val _nav: Seq[DropdownMenu.Item[Env, Action]] = Nil,
    private val _moreLabel: String = "More",
    private val _moreId: String = "topbar-overflow",
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {
  import TopBar.*

  /** Explicit bar height. Prefer leaving unset when nested under [[oxygen.ui.web.layout.HolyGrail]] (grid row owns height). */
  def barHeight(h: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(height = Some(h)))

  def clearBarHeight: TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(height = None))

  def bg(color: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(bg = color))

  def itemHover(color: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(itemHover = color))

  def itemActive(color: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(itemActive = color))

  def itemFg(color: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_cache = _cache.copy(itemFg = color))

  /**
    * Brand shell: solid **primary** fill + [[S.color.primary.on]] ink
    * (luminance-picked — not always white; light primaries get black labels).
    */
  def brand: TopBar[Env, Action, StateGet, StateSet] =
    bg(S.color.primary.standard)
      .itemHover(S.color.primary.hover)
      .itemActive(S.color.primary.active)
      .itemFg(S.color.primary.on)

  /**
    * Surface shell: mode-relative page chrome (raised layer + default text).
    * Use when the bar should track light/dark like the canvas, not solid brand.
    */
  def surface: TopBar[Env, Action, StateGet, StateSet] =
    bg(S.color.bg.layerOne)
      .itemHover(S.color.bg.layerTwo)
      .itemActive(S.color.bg.layerThree)
      .itemFg(S.color.fg.default)

  def left[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: TopBar.Item[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_left = _left ++ addChildren)

  def leftItems[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: (TopBar.Item.Const => TopBar.Item[Env2, Action2, StateGet2, StateSet2])*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_left = _left ++ addChildren.map(_(TopBar.Item.empty)))

  def leftOpt[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Option[TopBar.Item[Env2, Action2, StateGet2, StateSet2]]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_left = _left ++ addChildren.flatten)

  def right[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: TopBar.Item[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_right = _right ++ addChildren)

  def rightItems[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: (TopBar.Item.Const => TopBar.Item[Env2, Action2, StateGet2, StateSet2])*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_right = _right ++ addChildren.map(_(TopBar.Item.empty)))

  def rightOpt[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Option[TopBar.Item[Env2, Action2, StateGet2, StateSet2]]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_right = _right ++ addChildren.flatten)

  /**
    * Responsive nav items (OXY-151). Shown inline (next to the left slot) at `>= md`, and auto-collapsed
    * into a single overflow "More" [[DropdownMenu]] below `md` — no JS/`matchMedia`, the swap is pure CSS
    * (see [[TopBar.responsiveSheet]], registered via [[oxygen.ui.web.defaults.coreOxygenStyleSheets]]).
    *
    * Items are typed [[DropdownMenu.Item]]s (label / icon / `onClickPush` / `onSelect` / `disabled`), the
    * exact same model the overflow panel renders — so nothing is duplicated between the two layouts.
    *
    * {{{
    * TopBar.empty.brand
    *   .left(TopBar.item.index("MyApp").onClickPush(HomePage))
    *   .nav(
    *     TopBar.menuItem("Home").onClickPush(HomePage),
    *     TopBar.menuItem("Products").withIcon(Icon.grid).onClickPush(ProductsPage),
    *     TopBar.menuItem("About").onClickPush(AboutPage),
    *   )
    * }}}
    */
  def nav[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addItems: DropdownMenu.Item[Env2, Action2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    copy(_nav = _nav ++ addItems)

  /** Label for the collapsed overflow menu trigger (default `"More"`). */
  def moreLabel(label: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_moreLabel = label)

  /** Stable id for the overflow menu's open/closed state (must be unique per call site). */
  def moreId(id: String): TopBar[Env, Action, StateGet, StateSet] =
    copy(_moreId = id)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    import oxygen.ui.web.create.{height as heightAttr, width as widthAttr}
    val c = _cache
    val bar: Node =
      div(
        widthAttr := 100.pct,
        // Optional: when unset, parent (e.g. HolyGrail top row) controls height — avoids fighting.
        heightAttr := c.height.getOrElse(100.pct),
        display.flex,
        backgroundColor := c.bg,
        flexShrink := "0",
      )
    val shrinkSection: Node =
      div(
        heightAttr := 100.pct,
        flexGrow := 0,
        flexShrink := 1,
        display.flex,
        alignItems.center,
      )
    val growSection: Node =
      div(
        heightAttr := 100.pct,
        flexGrow := 1,
        flexShrink := 0,
      )

    // Responsive nav (OXY-151): both layouts are rendered; CSS media queries show exactly one.
    //   - `.oxy-topbar-nav`      : inline items, hidden below `md`
    //   - `.oxy-topbar-overflow` : collapsed "More" dropdown, hidden at/above `md`
    val navInline: PWidget[Env, Action, Any, Nothing] =
      Widget.when(_nav.nonEmpty) {
        div(
          Widget.`class`("oxy-topbar-nav"),
          heightAttr := 100.pct,
          display.flex,
          alignItems.center,
          flexShrink := "0",
          Widget.fragment(_nav.map(navItemInline(_, c))),
        )
      }
    val navOverflow: PWidget[Env, Action, Any, Nothing] =
      Widget.when(_nav.nonEmpty) {
        div(
          Widget.`class`("oxy-topbar-overflow"),
          heightAttr := 100.pct,
          display.flex,
          alignItems.center,
          flexShrink := "0",
          overflowMenu(c),
        )
      }

    bar(
      shrinkSection(_left.map(_.withBarColors(c))*),
      navInline,
      navOverflow,
      growSection,
      shrinkSection(_right.map(_.withBarColors(c, alignEnd = true))*),
    )
  }

  private def navItemInline(item: DropdownMenu.Item[Env, Action], c: Cache): PWidget[Env, Action, Any, Nothing] =
    if item.barSeparator then Widget.empty
    else if item.barDisabled then
      TopBar.itemWidget(c)(
        cursor := "not-allowed",
        opacity := "0.55",
        item.barIcon.map(_.md).getOrElse(Widget.empty),
        Widget.when(item.barLabel.nonEmpty)(span(item.barLabel)),
      )
    else
      TopBar.itemWidget(c)(
        item.barIcon.map(_.md).getOrElse(Widget.empty),
        Widget.when(item.barLabel.nonEmpty)(span(item.barLabel)),
        gap := S.spacing._2,
        onClick.a[Action].handle(rh => item.barSelect(rh)),
      )

  private def overflowMenu(c: Cache): PWidget[Env, Action, Any, Nothing] =
    DropdownMenu(_moreId, span(_moreLabel))
      .items(_nav*)
      .caret
      .trigger(
        create.height := 100.pct,
        padding := "0 1rem",
        fontSize := S.fontSize._5,
        color := c.itemFg,
        fontWeight := S.fontWeight.medium,
        backgroundColor.dynamic.hover := c.itemHover,
        backgroundColor.dynamic.hoverActive := c.itemActive,
      )

}
object TopBar extends WidgetTypes[TopBar] {

  val empty: TopBar.Const = TopBar(Cache.default, Nil, Nil)
  def apply(): TopBar.Const = empty

  val item: TopBar.Item.Const = TopBar.Item.empty

  /** A menu entry for a [[Item.dropdown]] (delegates to [[DropdownMenu.item]]). */
  def menuItem(label: String): DropdownMenu.Item.Const = DropdownMenu.item(label)

  /** A separator line inside a [[Item.dropdown]] menu. */
  val menuSeparator: DropdownMenu.Item.Const = DropdownMenu.separator

  final case class Item[-Env, +Action, -StateGet, +StateSet <: StateGet] private (
      private val _children: Growable[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
      private val _bar: Cache,
      private val _dropdownId: Option[String],
      private val _triggerContent: Growable[Widget],
      private val _menuItems: Growable[DropdownMenu.Item[Env, Action]],
      private val _alignEnd: Boolean,
  ) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

    private lazy val _built: Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      _dropdownId match {
        case Some(id) =>
          // Trigger reuses the bar-item chrome (height / colors / hover) + caret; panel is the shared DropdownMenu.
          DropdownMenu(id, _triggerContent.to[Seq]*)
            .items(_menuItems.to[Seq]*)
            .align(if _alignEnd then DropdownMenu.Align.End else DropdownMenu.Align.Start)
            .caret
            .trigger(
              create.height := 100.pct,
              padding := "0 1rem",
              fontSize := S.fontSize._5,
              color := _bar.itemFg,
              fontWeight := S.fontWeight.medium,
              backgroundColor.dynamic.hover := _bar.itemHover,
              backgroundColor.dynamic.hoverActive := _bar.itemActive,
            )
        case None =>
          TopBar.itemWidget(_bar).appendChildren(_children)
      }

    override protected def build: PWidget[Env, Action, StateGet, StateSet] = _built

    private[TopBar] def withBarColors(c: Cache): Item[Env, Action, StateGet, StateSet] =
      copy(_bar = c)

    private[TopBar] def withBarColors(c: Cache, alignEnd: Boolean): Item[Env, Action, StateGet, StateSet] =
      copy(_bar = c, _alignEnd = alignEnd)

    /**
      * Turn this item into a dropdown / popup menu (OXY-152). `id` must be stable + unique per call site
      * (keys the menu's internal open/closed state). Built on the shared [[DropdownMenu]] component so the
      * panel behaviour (scrim, keyboard, a11y) is consistent everywhere.
      *
      * {{{
      * TopBar.item.dropdown("nav-products", "Products")(
      *   TopBar.menuItem("Overview").onClickPush(OverviewPage.nav()),
      *   TopBar.menuItem("Pricing").onClickPush(PricingPage.nav()),
      *   TopBar.menuSeparator,
      * )
      * }}}
      */
    def dropdown[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        id: String,
        label: String,
    )(
        addItems: DropdownMenu.Item[Env2, Action2]*,
    ): Item[Env2, Action2, StateGet2, StateSet2] =
      copy(
        _dropdownId = id.some,
        _triggerContent = Growable.single(span(label)),
        _menuItems = _menuItems ++ Growable.many(addItems),
      )

    /** Dropdown with a leading icon on the trigger. */
    def dropdownWithIcon[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        id: String,
        icon: Icon,
        label: String,
    )(
        addItems: DropdownMenu.Item[Env2, Action2]*,
    ): Item[Env2, Action2, StateGet2, StateSet2] =
      copy(
        _dropdownId = id.some,
        _triggerContent = Growable.many(Seq(icon.md, span(label))),
        _menuItems = _menuItems ++ Growable.many(addItems),
      )

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
    ): TopBar.Item[Env2, Action2, StateGet2, StateSet2] =
      copy(_children = _children ++ Growable.many(addChildren))

    def index: Item[Env, Action, StateGet, StateSet] =
      this.apply(
        color := S.color.brand.primary2.light,
        fontSize := S.fontSize._7,
        fontWeight := S.fontWeight.bold,
      )

    def onClickPush(nav: RoutablePage.Navigate): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.push(nav))
    def onClickPush(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickPush(page.navigate(params))
    def onClickPush(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickPush(page)(())
    def onClickPush(url: => URL): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.push(url))
    def onClickPush(url: String): Item[Env, Action, StateGet, StateSet] = this.onClickPush(unsafeUrl(url))

    def onClickReplace(nav: RoutablePage.Navigate): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.replace(nav))
    def onClickReplace(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(page.navigate(params))
    def onClickReplace(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(page)(())
    def onClickReplace(url: => URL): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.replace(url))
    def onClickReplace(url: String): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(unsafeUrl(url))

    /** Leading icon + label (or icon-only when label empty). */
    def withIcon(icon: Icon, label: String = ""): Item[Env, Action, StateGet, StateSet] =
      if label.isEmpty then this.apply(icon.md)
      else
        this.apply(
          display.inlineFlex,
          alignItems.center,
          gap := S.spacing._2,
          icon.md,
          span(label),
        )

  }
  object Item extends WidgetTypes[TopBar.Item] {

    val empty: TopBar.Item.Const = Item(Growable.empty, Cache.default, None, Growable.empty, Growable.empty, false)

    def apply(text: String): TopBar.Item.Const =
      empty.apply(text)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Cache(
      /** When `None`, no height CSS is set (parent layout controls size). */
      height: Option[String],
      bg: String,
      itemHover: String,
      itemActive: String,
      itemFg: String,
  )
  object Cache {
    val default: Cache =
      Cache(
        height = None,
        bg = S.color.primary.standard,
        itemHover = S.color.primary.hover,
        itemActive = S.color.primary.active,
        itemFg = S.color.primary.on,
      )
  }

  /**
    * OXY-151: auto-swap the responsive [[TopBar.nav]] items between inline (desktop) and a collapsed
    * "More" overflow menu (mobile) purely via CSS `@media` — no JS/`matchMedia`, so it is SSR/hydration
    * safe (no FOUC). Registered by [[oxygen.ui.web.defaults.coreOxygenStyleSheets]].
    */
  val responsiveSheet: StyleSheet =
    MediaCSS.styleSheet("topbar-responsive")(
      MediaCSS.mdUp(
        """.oxy-topbar-overflow { display: none !important; }""",
      ),
      MediaCSS.belowMd(
        """|.oxy-topbar-nav { display: none !important; }
           |.oxy-topbar-overflow { display: flex !important; }
           |""".stripMargin,
      ),
    )

  private def unsafeUrl(url: String): URL = URL.decode(url) match
    case Right(url)  => url
    case Left(error) => throw new RuntimeException(s"Invalid URL [$url]: $error")

  private def itemWidget(c: Cache): Node = {
    import oxygen.ui.web.create.height as heightAttr
    div(
      heightAttr := 100.pct,
      cursor.pointer,
      userSelect.none,
      display.inlineFlex,
      justifyContent.center,
      alignItems.center,
      padding := "0 1rem",
      fontSize := S.fontSize._5,
      color := c.itemFg,
      fontWeight := S.fontWeight.medium,
      backgroundColor.dynamic.hover := c.itemHover,
      backgroundColor.dynamic.hoverActive := c.itemActive,
    )
  }

}
