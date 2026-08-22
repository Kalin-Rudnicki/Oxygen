package oxygen.ui.web.component

import monocle.Lens
import org.scalajs.dom
import org.scalajs.dom.HTMLElement
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.internal.LensUtil
import oxygen.ui.web.service.Window
import zio.*
import zio.http.URL

/**
  * Action / nav popup menu (OXY-152; reserved by the `component` package TODO).
  *
  * A click-to-open menu of typed [[DropdownMenu.Item]]s. Open/closed lives on page state `S` via a
  * [[Lens]] (see [[DropdownMenu.State]]) — the same lens-into-`S` pattern as [[Tabs]] / [[Drawer]], no
  * component-owned/global state. Each panel item is a normal `(Env, Action)` entry; selecting one runs its
  * effect and closes the menu.
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
  * DropdownMenu[PageState](_.productsMenu, span("Products"))
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
final class DropdownMenu[-Env, +Action, S] private (
    private val lens: Lens[S, DropdownMenu.State],
    private val _trigger: Growable[Widget],
    private val _items: Seq[DropdownMenu.Item[Env, Action]],
    private val _align: DropdownMenu.Align,
    private val _showCaret: Boolean,
    private val _ariaLabel: Option[String],
) extends PWidget.Deferred.Stateful[Env, Action, S] {

  private def copy[Env2 <: Env, Action2 >: Action](
      _trigger: Growable[Widget] = _trigger,
      _items: Seq[DropdownMenu.Item[Env2, Action2]] = _items,
      _align: DropdownMenu.Align = _align,
      _showCaret: Boolean = _showCaret,
      _ariaLabel: Option[String] = _ariaLabel,
  ): DropdownMenu[Env2, Action2, S] =
    new DropdownMenu(lens, _trigger, _items, _align, _showCaret, _ariaLabel)

  def items[Env2 <: Env, Action2 >: Action](
      addItems: DropdownMenu.Item[Env2, Action2]*,
  ): DropdownMenu[Env2, Action2, S] =
    copy(_items = _items ++ addItems)

  def trigger(mods: Widget*): DropdownMenu[Env, Action, S] = copy(_trigger = _trigger ++ Growable.many(mods))

  def align(a: DropdownMenu.Align): DropdownMenu[Env, Action, S] = copy(_align = a)
  def alignStart: DropdownMenu[Env, Action, S] = align(DropdownMenu.Align.Start)
  def alignEnd: DropdownMenu[Env, Action, S] = align(DropdownMenu.Align.End)

  def caret: DropdownMenu[Env, Action, S] = copy(_showCaret = true)
  def noCaret: DropdownMenu[Env, Action, S] = copy(_showCaret = false)

  def ariaLabel(label: String): DropdownMenu[Env, Action, S] = copy(_ariaLabel = label.some)

  override protected def build: PWidget.Stateful[Env, Action, S] =
    Widget.state[S].fixGet { (ws, s) =>
      import DropdownMenu.*
      val isOpen: Boolean = lens.get(s).open

      def open(trigger: HTMLElement): UIO[Unit] =
        ws.update(lens.modify(_.show)) *> ZIO.succeed(afterRender(() => panelOf(trigger).foreach(focusItem(_, 0))))

      val triggerNode: WidgetEAS[Env, Action, S] =
        div(
          O.DropdownMenu.Trigger,
          Widget.raw.htmlAttr("role", "button"),
          Widget.raw.htmlAttr("aria-haspopup", "menu"),
          Widget.raw.htmlAttr("aria-expanded", isOpen.toString),
          Widget.raw.htmlAttr("tabindex", "0"),
          _ariaLabel.map(Widget.raw.htmlAttr("aria-label", _)).getOrElse(Widget.empty),
          Widget.fragment(_trigger.to[Seq]),
          Widget.when(_showCaret)(span(O.DropdownMenu.Caret, Icon.chevronDown.sm)),
          onClick.es[S].handle { (st, e) =>
            if isOpen then st.update(lens.modify(_.hide))
            else open(e.currentTarget.asInstanceOf[HTMLElement])
          },
          onKeyDown.es[S].handle { (st, e) =>
            e.key match {
              case "Enter" | " " | "ArrowDown" =>
                e.preventDefault()
                open(e.currentTarget.asInstanceOf[HTMLElement])
              case "Escape" =>
                st.update(lens.modify(_.hide))
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
              onClick.s[S].handle(_.update(lens.modify(_.hide))),
            ),
            div(
              O.DropdownMenu.Panel.optMods(
                _.AlignStart -> (_align == Align.Start),
                _.AlignEnd -> (_align == Align.End),
              ),
              Widget.raw.htmlAttr("role", "menu"),
              onKeyDown.es[S].handle { (st, e) =>
                val panel = e.currentTarget.asInstanceOf[HTMLElement]
                e.key match {
                  case "ArrowDown"   => e.preventDefault(); ZIO.succeed(moveFocus(panel, +1))
                  case "ArrowUp"     => e.preventDefault(); ZIO.succeed(moveFocus(panel, -1))
                  case "Home"        => e.preventDefault(); ZIO.succeed(focusItem(panel, 0))
                  case "End"         => e.preventDefault(); ZIO.succeed(focusItem(panel, Int.MaxValue))
                  case "Enter" | " " => e.preventDefault(); ZIO.succeed(clickActive())
                  case "Escape"      =>
                    val trigger = triggerOf(panel)
                    st.update(lens.modify(_.hide)) *> ZIO.succeed(afterRender(() => trigger.foreach(_.focus())))
                  case "Tab" =>
                    st.update(lens.modify(_.hide))
                  case _ =>
                    ZIO.unit
                }
              },
              Widget.foreach(_items.toList) { item =>
                DropdownMenu.renderItem(item)(rh => item._onSelect(rh) *> ws.update(lens.modify(_.hide)))
              },
            ),
          ),
        ),
      )
    }

}
object DropdownMenu {

  enum Align { case Start, End }

  /** Open/closed state for one menu; lives on page state via a [[Lens]] (see class docs). */
  final case class State(open: Boolean = false) {
    def show: State = copy(open = true)
    def hide: State = copy(open = false)
    def toggle: State = copy(open = !open)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Entry points
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Build over an explicit lens into page state `S` (used by [[TopBar]]'s dropdown items). */
  def fromLens[S](lens: Lens[S, State], trigger: Growable[Widget]): DropdownMenu[Any, Nothing, S] =
    new DropdownMenu(lens, trigger, Nil, Align.Start, _showCaret = true, None)

  /**
    * @param f accessor to this menu's [[State]] on page state `S` (e.g. `_.productsMenu`); its open/closed
    *          flag lives there, threaded like any other stateful widget.
    */
  inline def apply[S](inline f: S => State, trigger: Widget*): DropdownMenu[Any, Nothing, S] =
    fromLens(LensUtil.genLens(f), Growable.many(trigger))

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
  //      Rendering
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def renderItem[Env, Action, S](item: Item[Env, Action])(onSelect: RaiseHandler[Any, Action] => ZIO[Env & Scope, UIError, Unit]): WidgetEAS[Env, Action, S] =
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
          onClick.as[Action, S].handle { (_, rh) => onSelect(rh) },
        ),
      )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Keyboard + focus (driven by real DOM events → safe without a mount hook)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def afterRender(f: () => Unit): Unit =
    dom.window.requestAnimationFrame(_ => f()): Unit

  /** The `role=menu` panel that is a sibling of this menu's trigger (both live under the same root). */
  private def panelOf(trigger: HTMLElement): Option[HTMLElement] =
    Option(trigger.parentElement).flatMap(root => Option(root.querySelector("[role='menu']"))).map(_.asInstanceOf[HTMLElement])

  /** The `role=button` trigger that is a sibling of this menu's panel. */
  private def triggerOf(panel: HTMLElement): Option[HTMLElement] =
    Option(panel.parentElement).flatMap(root => Option(root.querySelector("[role='button']"))).map(_.asInstanceOf[HTMLElement])

  private def enabledItems(panel: HTMLElement): Seq[HTMLElement] = {
    val nodes = panel.querySelectorAll("[role='menuitem']:not([aria-disabled='true'])")
    (0 until nodes.length).map(i => nodes(i).asInstanceOf[HTMLElement])
  }

  /** Focus item at `idx` (clamped); `Int.MaxValue` = last. */
  private def focusItem(panel: HTMLElement, idx: Int): Unit = {
    val items = enabledItems(panel)
    if items.nonEmpty then items(idx.max(0).min(items.size - 1)).focus()
  }

  private def moveFocus(panel: HTMLElement, delta: Int): Unit = {
    val items = enabledItems(panel)
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

}
