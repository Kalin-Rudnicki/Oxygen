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
    bar(
      shrinkSection(_left.map(_.withBarColors(c))*),
      growSection,
      shrinkSection(_right.map(_.withBarColors(c))*),
    )
  }

}
object TopBar extends WidgetTypes[TopBar] {

  val empty: TopBar.Const = TopBar(Cache.default, Nil, Nil)
  def apply(): TopBar.Const = empty

  val item: TopBar.Item.Const = TopBar.Item.empty

  final case class Item[-Env, +Action, -StateGet, +StateSet <: StateGet] private (
      private val _children: Growable[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
      private val _bar: Cache,
  ) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

    private lazy val _built: Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      TopBar.itemWidget(_bar).appendChildren(_children)

    override protected def build: PWidget[Env, Action, StateGet, StateSet] = _built

    private[TopBar] def withBarColors(c: Cache): Item[Env, Action, StateGet, StateSet] =
      copy(_bar = c)

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

    val empty: TopBar.Item.Const = Item(Growable.empty, Cache.default)

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
