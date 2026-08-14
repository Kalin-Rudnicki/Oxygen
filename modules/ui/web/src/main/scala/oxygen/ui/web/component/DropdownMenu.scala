package oxygen.ui.web.component

import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.Window
import zio.*
import zio.http.URL

/**
  * Action / nav popup menu (OXY-152; reserved by the `component` package TODO).
  *
  * A click-to-open menu of typed [[DropdownMenu.Item]]s. Owns its own open/closed state internally
  * (a per-instance [[PageLocalState]] keyed by a caller-supplied stable `id`), so it drops in anywhere
  * as a plain stateless widget — no need to thread a `Boolean` through page state. Reused by
  * [[TopBar]] dropdown items and available for SideBar / overflow menus.
  *
  * Behaviour:
  *   - opens on click / `Enter` / `Space` / `ArrowDown` on the trigger,
  *   - closes on outside-click (transparent scrim), `Escape` (restores focus to trigger), `Tab`, or select,
  *   - `ArrowUp`/`ArrowDown` roving (wraps), `Home`/`End`, `Enter`/`Space` activate the focused item.
  *
  * A11y: trigger `role=button` + `aria-haspopup=menu` + `aria-expanded`; panel `role=menu`; items
  * `role=menuitem` (+ `aria-disabled`). Styles live in [[oxygen.ui.web.create.OxygenStyleSheet.DropdownMenu]]
  * (`O.DropdownMenu`), included via [[oxygen.ui.web.defaults.coreOxygenStyleSheets]].
  *
  * {{{
  * DropdownMenu("nav-products", "Products")
  *   .items(
  *     DropdownMenu.item("Overview").onClickPush(OverviewPage.nav()),
  *     DropdownMenu.item("Pricing").withIcon(Icon.tag).onClickPush(PricingPage.nav()),
  *     DropdownMenu.separator,
  *     DropdownMenu.item("Coming soon").disabled,
  *   )
  * }}}
  *
  * NOTE (v1): one flat level (no nested submenus); positioned `absolute` under the trigger (an ancestor
  * with `overflow:hidden` could clip — a portal/`fixed` variant is future work). Trigger opens on click,
  * not hover.
  */
final case class DropdownMenu[-Env, +Action](
    private val _id: String,
    private val _trigger: Growable[Widget],
    private val _items: Seq[DropdownMenu.Item[Env, Action]],
    private val _align: DropdownMenu.Align,
    private val _showCaret: Boolean,
    private val _ariaLabel: Option[String],
) extends PWidget.Deferred[Env, Action, Any, Nothing] {

  def items[Env2 <: Env, Action2 >: Action](
      addItems: DropdownMenu.Item[Env2, Action2]*,
  ): DropdownMenu[Env2, Action2] =
    copy(_items = _items ++ addItems)

  def trigger(mods: Widget*): DropdownMenu[Env, Action] = copy(_trigger = _trigger ++ Growable.many(mods))

  def align(a: DropdownMenu.Align): DropdownMenu[Env, Action] = copy(_align = a)
  def alignStart: DropdownMenu[Env, Action] = align(DropdownMenu.Align.Start)
  def alignEnd: DropdownMenu[Env, Action] = align(DropdownMenu.Align.End)

  def caret: DropdownMenu[Env, Action] = copy(_showCaret = true)
  def noCaret: DropdownMenu[Env, Action] = copy(_showCaret = false)

  def ariaLabel(label: String): DropdownMenu[Env, Action] = copy(_ariaLabel = label.some)

  override protected def build: PWidget[Env, Action, Any, Nothing] =
    DropdownMenu.openStateFor(_id).attach { st =>
      val isOpen: Boolean = st.renderTimeValue
      val ddId: String = _id

      val triggerNode: WidgetEAS[Env, Action, Boolean] =
        div(
          O.DropdownMenu.Trigger,
          id := DropdownMenu.triggerElemId(ddId),
          Widget.raw.htmlAttr("role", "button"),
          Widget.raw.htmlAttr("aria-haspopup", "menu"),
          Widget.raw.htmlAttr("aria-expanded", isOpen.toString),
          Widget.raw.htmlAttr("tabindex", "0"),
          _ariaLabel.map(Widget.raw.htmlAttr("aria-label", _)).getOrElse(Widget.empty),
          Widget.fragment(_trigger.to[Seq]),
          Widget.when(_showCaret)(span(O.DropdownMenu.Caret, Icon.chevronDown.sm)),
          onClick.s[Boolean].handle { s =>
            if isOpen then s.set(false)
            else DropdownMenu.openAndFocus(s, ddId)
          },
          onKeyDown.es[Boolean].handle { (s, e) =>
            e.key match {
              case "Enter" | " " | "ArrowDown" =>
                e.preventDefault()
                DropdownMenu.openAndFocus(s, ddId)
              case "Escape" =>
                s.set(false)
              case _ =>
                ZIO.unit
            }
          },
        )

      div(
        O.DropdownMenu.optMods(_.Open -> isOpen),
        triggerNode,
        Widget.when(isOpen)(
          fragment(
            div(
              O.DropdownMenu.Scrim,
              onClick.s[Boolean].handle(_.set(false)),
            ),
            div(
              O.DropdownMenu.Panel.optMods(
                _.AlignStart -> (_align == DropdownMenu.Align.Start),
                _.AlignEnd -> (_align == DropdownMenu.Align.End),
              ),
              Widget.raw.htmlAttr(DropdownMenu.panelDataAttr, ddId),
              Widget.raw.htmlAttr("role", "menu"),
              onKeyDown.es[Boolean].handle { (s, e) => DropdownMenu.panelKeyDown(s, e, ddId) },
              Widget.foreach(_items.toList) { item => DropdownMenu.renderItem(item) },
            ),
          ),
        ),
      )
    }

}
object DropdownMenu {

  enum Align { case Start, End }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Entry points
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * @param id stable identifier for this menu's open/closed state — must be unique + stable per call site
    *           (used to key the internal [[PageLocalState]] across re-renders).
    */
  def apply(id: String, triggerContent: Widget*): DropdownMenu[Any, Nothing] =
    new DropdownMenu(id, Growable.many(triggerContent), Nil, Align.Start, _showCaret = true, None)

  /** Label + optional caret trigger. */
  def apply(id: String): DropdownMenu[Any, Nothing] = apply(id, Widget.empty)

  val item: Item.Const = Item.empty
  def item(label: String): Item.Const = Item.empty.label(label)

  val separator: Item.Const = Item.empty.copy(_isSeparator = true)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Item
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * One menu entry: a label (+ optional leading icon), or a [[separator]]. Activation is one of
    * `onClickPush` (navigate), `onSelectAction` (raise an action), or `onSelect` (arbitrary effect).
    */
  final case class Item[-Env, +Action] private[DropdownMenu] (
      private[DropdownMenu] val _label: String,
      private[DropdownMenu] val _icon: Option[Icon],
      private[DropdownMenu] val _isDisabled: Boolean,
      private[DropdownMenu] val _isSeparator: Boolean,
      private[DropdownMenu] val _onSelect: RaiseHandler[Any, Action] => ZIO[Env & Scope, UIError, Unit],
  ) {

    def label(text: String): Item[Env, Action] = copy(_label = text)
    def withIcon(icon: Icon): Item[Env, Action] = copy(_icon = icon.some)
    def disabled: Item[Env, Action] = copy(_isDisabled = true)
    def disabled(value: Boolean): Item[Env, Action] = copy(_isDisabled = value)

    def onClickPush(nav: RoutablePage.Navigate): Item[Env, Action] = copy(_onSelect = _ => nav.push)
    def onClickPush(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action] = onClickPush(page.navigate(params))
    def onClickPush(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action] = onClickPush(page)(ev(()))
    def onClickPush(url: => URL): Item[Env, Action] = copy(_onSelect = _ => Window.location.assign(url.encode))

    def onClickReplace(nav: RoutablePage.Navigate): Item[Env, Action] = copy(_onSelect = _ => nav.replace)
    def onClickReplace(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action] = onClickReplace(page.navigate(params))
    def onClickReplace(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action] = onClickReplace(page)(ev(()))

    /** Raise a page-level action when selected. */
    def onSelectAction[Action2 >: Action](action: Action2): Item[Env, Action2] =
      new Item[Env, Action2](_label, _icon, _isDisabled, _isSeparator, _.raiseAction(action))

    /** Run an arbitrary effect when selected. */
    def onSelect[Env2 <: Env](effect: => ZIO[Env2 & Scope, UIError, Unit]): Item[Env2, Action] =
      new Item[Env2, Action](_label, _icon, _isDisabled, _isSeparator, _ => effect)

  }
  object Item {
    type Const = Item[Any, Nothing]
    val empty: Item.Const = Item("", None, _isDisabled = false, _isSeparator = false, _ => ZIO.unit)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Internal state registry
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // scala.js is single-threaded; a plain mutable map memoizes one PageLocalState per stable id
  // (state itself is scoped per page instance internally). Keeps `id`-keyed state stable across re-renders.
  private val openStates: scala.collection.mutable.Map[String, PageLocalState[Boolean]] =
    scala.collection.mutable.Map.empty

  private def openStateFor(id: String): PageLocalState[Boolean] =
    openStates.getOrElseUpdate(id, new PageLocalState[Boolean](s"DropdownMenu[$id]")(false) {})

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Rendering
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def renderItem[Env, Action](item: Item[Env, Action]): WidgetEAS[Env, Action, Boolean] =
    if item._isSeparator then
      div(O.DropdownMenu.Separator, Widget.raw.htmlAttr("role", "separator"))
    else
      div(
        O.DropdownMenu.Item.optMods(_.Disabled -> item._isDisabled),
        Widget.raw.htmlAttr("role", "menuitem"),
        Widget.raw.htmlAttr("tabindex", "-1"),
        Widget.when(item._isDisabled)(Widget.raw.htmlAttr("aria-disabled", "true")),
        item._icon.map(icon => span(O.DropdownMenu.ItemIcon, icon.sm)).getOrElse(Widget.empty),
        span(item._label),
        Widget.when(!item._isDisabled)(
          onClick.as[Action, Boolean].handle { (s, rh) => item._onSelect(rh) *> s.set(false) },
        ),
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Keyboard + focus (driven by real DOM events → safe without a mount hook)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val panelDataAttr: String = "data-oxy-dd"
  private def triggerElemId(id: String): String = s"oxy-dd-trigger-$id"

  private def openAndFocus(st: WidgetState[Boolean], id: String): UIO[Unit] =
    st.set(true) *> ZIO.succeed { afterRender(() => focusItem(id, 0)) }

  private def panelKeyDown(st: WidgetState[Boolean], e: dom.KeyboardEvent, id: String): UIO[Unit] =
    e.key match {
      case "ArrowDown"   => e.preventDefault(); ZIO.succeed(moveFocus(id, +1))
      case "ArrowUp"     => e.preventDefault(); ZIO.succeed(moveFocus(id, -1))
      case "Home"        => e.preventDefault(); ZIO.succeed(focusItem(id, 0))
      case "End"         => e.preventDefault(); ZIO.succeed(focusItem(id, Int.MaxValue))
      case "Enter" | " " =>
        e.preventDefault()
        ZIO.succeed(clickActive())
      case "Escape" =>
        st.set(false) *> ZIO.succeed(focusTrigger(id))
      case "Tab" =>
        st.set(false)
      case _ =>
        ZIO.unit
    }

  private def afterRender(f: () => Unit): Unit =
    dom.window.requestAnimationFrame(_ => f()): Unit

  private def enabledItems(id: String): Seq[HTMLElement] = {
    val nodes = dom.document.querySelectorAll(s"[$panelDataAttr='$id'] [role='menuitem']:not([aria-disabled='true'])")
    (0 until nodes.length).map(i => nodes(i).asInstanceOf[HTMLElement])
  }

  /** Focus item at `idx` (clamped); `Int.MaxValue` = last. */
  private def focusItem(id: String, idx: Int): Unit = {
    val items = enabledItems(id)
    if items.nonEmpty then items(idx.max(0).min(items.size - 1)).focus()
  }

  private def moveFocus(id: String, delta: Int): Unit = {
    val items = enabledItems(id)
    if items.nonEmpty then {
      val active = dom.document.activeElement
      val current = items.indexWhere(_ eq active)
      val next = if current < 0 then (if delta > 0 then 0 else items.size - 1) else Math.floorMod(current + delta, items.size)
      items(next).focus()
    }
  }

  private def clickActive(): Unit =
    dom.document.activeElement match {
      case el: HTMLElement => el.click()
      case _               => ()
    }

  private def focusTrigger(id: String): Unit =
    afterRender { () =>
      dom.document.getElementById(triggerElemId(id)) match {
        case el: HTMLElement => el.focus()
        case _               => ()
      }
    }

}
