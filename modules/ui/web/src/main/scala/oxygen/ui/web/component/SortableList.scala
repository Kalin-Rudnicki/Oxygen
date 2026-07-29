package oxygen.ui.web.component

import monocle.Lens
import org.scalajs.dom
import org.scalajs.dom.{DataTransferDropEffectKind, DataTransferEffectAllowedKind, HTMLElement}
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.ZIO

/**
  * Sortable list: each row is a real [[Widget.Stateful]] over element type `A`
  * (same idea as Tabs panels over page state — no `(A, Int) => Widget` callback).
  *
  * List owns [[State]] (`items: Vector[A]`); call sites zoom out from page state.
  * Transient drag gesture is **not** page state — re-rendering mid-drag cancels HTML5 DnD.
  *
  * {{{
  *   SortableList[String](
  *     Widget.state[String].get(name => span(name)),
  *   ).onto.rowBg(S.color.bg.layerOne).noHandle.zoomOut[PageState](_.tasks)
  * }}}
  *
  * Styles live in [[sheet]] — included via [[oxygen.ui.web.defaults.coreOxygenStyleSheets]].
  */
final case class SortableList[-Env, +Action, A](
    private val elemWidget: Widget.Stateful[Env, Action, A],
    private val dropMode: SortableList.DropMode,
    private val style: SortableList.Style,
) extends PWidget.Deferred.Stateful[Env, Action, SortableList.State[A]] {

  def mode(m: SortableList.DropMode): SortableList[Env, Action, A] = copy(dropMode = m)
  def onto: SortableList[Env, Action, A] = mode(SortableList.DropMode.Onto)
  def between: SortableList[Env, Action, A] = mode(SortableList.DropMode.Between)

  def elem[Env2 <: Env, Action2 >: Action](
      w: Widget.Stateful[Env2, Action2, A],
  ): SortableList[Env2, Action2, A] =
    copy(elemWidget = w)

  def style(s: SortableList.Style): SortableList[Env, Action, A] = copy(style = s)
  def modStyle(f: SortableList.Style => SortableList.Style): SortableList[Env, Action, A] =
    copy(style = f(style))

  def rowBg(c: String): SortableList[Env, Action, A] = modStyle(_.rowBg(c))
  def rowPadding(p: String): SortableList[Env, Action, A] = modStyle(_.rowPadding(p))
  def rowGap(g: String): SortableList[Env, Action, A] = modStyle(_.rowGap(g))
  def rowRadius(r: String): SortableList[Env, Action, A] = modStyle(_.rowRadius(r))
  def handleColor(c: String): SortableList[Env, Action, A] = modStyle(_.handleColor(c))
  def showHandle(v: Boolean): SortableList[Env, Action, A] = modStyle(_.showHandle(v))
  def noHandle: SortableList[Env, Action, A] = showHandle(false)
  def withHandle: SortableList[Env, Action, A] = showHandle(true)
  def betweenSlotHeight(px: Int): SortableList[Env, Action, A] = modStyle(_.betweenSlotHeight(px))
  def betweenSlotMargin(m: String): SortableList[Env, Action, A] = modStyle(_.betweenSlotMargin(m))

  override protected def build: PWidget[Env, Action, SortableList.State[A], SortableList.State[A]] =
    SortableList.render(elemWidget, dropMode, style)

}
object SortableList {

  enum DropMode {
    case Onto
    case Between
  }

  /**
    * Presentational knobs for the list chrome (row shell + between slots).
    * Drag highlight classes stay in [[sheet]] (CSS class based — not re-renderable mid-drag).
    */
  final case class Style(
      rowBg: String,
      rowPadding: String,
      /** Gap between rows in [[DropMode.Onto]] (Between uses fixed 0 + slot height). */
      rowGap: String,
      rowRadius: String,
      handleColor: String,
      showHandle: Boolean,
      betweenSlotHeightPx: Int,
      betweenSlotMargin: String,
  ) {
    def rowBg(c: String): Style = copy(rowBg = c)
    def rowPadding(p: String): Style = copy(rowPadding = p)
    def rowGap(g: String): Style = copy(rowGap = g)
    def rowRadius(r: String): Style = copy(rowRadius = r)
    def handleColor(c: String): Style = copy(handleColor = c)
    def showHandle(v: Boolean): Style = copy(showHandle = v)
    def noHandle: Style = showHandle(false)
    def withHandle: Style = showHandle(true)
    def betweenSlotHeight(px: Int): Style = copy(betweenSlotHeightPx = px)
    def betweenSlotMargin(m: String): Style = copy(betweenSlotMargin = m)
  }
  object Style {
    val default: Style =
      Style(
        rowBg = S.color.bg.layerTwo,
        rowPadding = S.spacing._3,
        rowGap = S.spacing._2,
        rowRadius = S.borderRadius._2,
        handleColor = S.color.fg.subtle,
        showHandle = true,
        betweenSlotHeightPx = 8,
        betweenSlotMargin = css(0.px, S.spacing._2),
      )
  }

  /** `text/plain` is universally readable on drop (custom mimes are flaky). */
  private val Mime: String = "text/plain"

  private val ClDragging = "oxy-sortable-dragging"
  private val ClDropRow = "oxy-sortable-drop-row"
  private val ClDropSlot = "oxy-sortable-drop-slot-active"

  /**
    * Drag chrome classes. Prefer classes over mutating `element.style.backgroundColor`:
    * clearing inline bg after highlight was wiping the framework's resting fill.
    *
    * Included in [[oxygen.ui.web.defaults.coreOxygenStyleSheets]] — do not inject a style tag.
    */
  val sheet: StyleSheet =
    StyleSheet.makeConst("oxygen-sortable-list")(
      s"""
         |.$ClDragging {
         |  opacity: 0.5 !important;
         |  cursor: grabbing !important;
         |}
         |.$ClDropRow {
         |  outline: 2px solid ${S.color.primary.standard} !important;
         |  background-color: ${S.color.primary.subtle} !important;
         |}
         |.$ClDropSlot {
         |  background-color: ${S.color.primary.standard} !important;
         |}
         |""".stripMargin,
    )

  /**
    * Transient drag gesture (not page state — avoids re-render cancels).
    * One active drag per list instance family is fine for this UX.
    */
  private object Gesture {
    var from: Int = -1
    var dropAt: Int = -1
    private var highlight: Option[HTMLElement] = None
    private var draggingEl: Option[HTMLElement] = None

    def begin(fromIdx: Int, el: HTMLElement): Unit = {
      reset()
      from = fromIdx
      dropAt = fromIdx
      draggingEl = Some(el)
      el.classList.add(ClDragging)
    }

    def reset(): Unit = {
      clearHighlight()
      draggingEl.foreach(_.classList.remove(ClDragging))
      draggingEl = None
      from = -1
      dropAt = -1
    }

    def clearHighlight(): Unit =
      highlight.foreach { el =>
        el.classList.remove(ClDropRow)
        el.classList.remove(ClDropSlot)
        highlight = None
      }

    def setHighlight(el: HTMLElement, slot: Boolean): Unit =
      if !highlight.contains(el) then {
        clearHighlight()
        el.classList.add(if slot then ClDropSlot else ClDropRow)
        highlight = Some(el)
      }
  }

  final case class State[A](items: Vector[A]) {

    /**
      * Onto-style move: `to` is the target row index; item is removed then inserted
      * so it lands at that row (clamped after removal).
      */
    def moveOnto(from: Int, to: Int): State[A] = {
      if from < 0 || from >= items.size || to < 0 || to > items.size || from == to then this
      else {
        val buf = items.toBuffer
        val x = buf.remove(from)
        val insertAt = to.max(0).min(buf.size)
        buf.insert(insertAt, x)
        State(buf.toVector)
      }
    }

    /**
      * Between-style move: `slot` is the gap index in the pre-remove list
      * (`0` = before first, `items.size` = after last).
      */
    def moveBetween(from: Int, slot: Int): State[A] = {
      if from < 0 || from >= items.size || slot < 0 || slot > items.size then this
      else if slot == from || slot == from + 1 then this // already in that gap
      else {
        val buf = items.toBuffer
        val x = buf.remove(from)
        val insertAt = (if slot > from then slot - 1 else slot).max(0).min(buf.size)
        buf.insert(insertAt, x)
        State(buf.toVector)
      }
    }

  }
  object State {
    def empty[A]: State[A] = State(Vector.empty)
    def of[A](items: Seq[A]): State[A] = State(items.toVector)
  }

  /** Pure reorder used by tests (Onto insert semantics). */
  def reorder[A](items: Vector[A], from: Int, to: Int): Vector[A] =
    State(items).moveOnto(from, to).items

  /** Pure between-slot reorder (for tests). */
  def reorderToSlot[A](items: Vector[A], from: Int, slot: Int): Vector[A] =
    State(items).moveBetween(from, slot).items

  def empty[A]: SortableList[Any, Nothing, A] =
    new SortableList(Widget.empty.fixState[A], DropMode.Onto, Style.default)

  def apply[Env, Action, A](elem: Widget.Stateful[Env, Action, A]): SortableList[Env, Action, A] =
    new SortableList(elem, DropMode.Onto, Style.default)

  def apply[Env, Action, A](
      elem: Widget.Stateful[Env, Action, A],
      dropMode: DropMode,
  ): SortableList[Env, Action, A] =
    new SortableList(elem, dropMode, Style.default)

  /** Lens from list state into a single item by index. */
  private def itemLens[A](idx: Int): Lens[State[A], A] =
    Lens[State[A], A](_.items(idx))(a => s => s.copy(items = s.items.updated(idx, a)))

  private def render[Env, Action, A](
      elemWidget: Widget.Stateful[Env, Action, A],
      dropMode: DropMode,
      style: Style,
  ): Widget.Stateful[Env, Action, State[A]] =
    Widget.state[State[A]].fixGet { (st, s) =>
      dropMode match {
        case DropMode.Onto    => ontoList(st, s.items, elemWidget, style)
        case DropMode.Between => betweenList(st, s.items, elemWidget, style)
      }
    }

  /////// Shared handlers ///////////////////////////////////////////////////////////////

  private def dragStartHandler(from: Int): Widget =
    create.onDragStart.e.handle { e =>
      e.dataTransfer.setData(Mime, from.toString)
      e.dataTransfer.effectAllowed = DataTransferEffectAllowedKind.move
      val el = e.currentTarget.asInstanceOf[HTMLElement]
      Gesture.begin(from, el)
      ZIO.unit
    }

  private def dragEndHandler: Widget =
    create.onDragEnd.e.handle { _ =>
      Gesture.reset()
      ZIO.unit
    }

  private def parseFrom(raw: String): Int = {
    val fromData = raw.toIntOption.getOrElse(-1)
    if fromData >= 0 then fromData else Gesture.from
  }

  private def handleChrome(style: Style): Widget =
    if style.showHandle then
      span(
        display.inlineFlex,
        color := style.handleColor,
        cursor.grab,
        Icon.drag.sm,
      )
    else Widget.empty

  private def rowShell[Env, Action, A](
      idx: Int,
      elemWidget: Widget.Stateful[Env, Action, A],
      style: Style,
      dropHandlers: Widget*,
  ): Widget.Stateful[Env, Action, State[A]] =
    div(
      draggable.enable,
      dragStartHandler(idx),
      dragEndHandler,
      DnD.allowDrop,
      Widget.fragment(dropHandlers),
      display.flex,
      alignItems.center,
      gap := S.spacing._3,
      padding := style.rowPadding,
      backgroundColor := style.rowBg,
      border(2.px, "solid", S.color.bg.transparent),
      borderRadius := style.rowRadius,
      cursor.grab,
      userSelect.none,
      handleChrome(style),
      div(
        Widget.raw.css("flex", "1 1 auto"),
        minWidth := 0.px,
        elemWidget.zoomOutLens(itemLens[A](idx)),
      ),
    )

  /////// Onto (drop on row) ///////////////////////////////////////////////////////////////

  private def ontoList[Env, Action, A](
      st: WidgetState[State[A]],
      items: Vector[A],
      elemWidget: Widget.Stateful[Env, Action, A],
      style: Style,
  ): Widget.Stateful[Env, Action, State[A]] =
    div(
      display.flex,
      flexDirection.column,
      gap := style.rowGap,
      Widget.foreach((0 until items.size).toList) { idx =>
        rowShell(
          idx,
          elemWidget,
          style,
          onDragEnter.e.handle { e =>
            e.preventDefault()
            Gesture.dropAt = idx
            Gesture.setHighlight(e.currentTarget.asInstanceOf[HTMLElement], slot = false)
            ZIO.unit
          },
          onDragOver.e.handle { e =>
            e.preventDefault()
            e.dataTransfer.dropEffect = DataTransferDropEffectKind.move
            Gesture.dropAt = idx
            Gesture.setHighlight(e.currentTarget.asInstanceOf[HTMLElement], slot = false)
            ZIO.unit
          },
          onDrop.e.handle { e =>
            e.preventDefault()
            e.stopPropagation()
            val from = parseFrom(e.dataTransfer.getData(Mime))
            val to = idx
            Gesture.reset()
            if from >= 0 && from != to then st.update(_.moveOnto(from, to))
            else ZIO.unit
          },
        )
      },
    )

  /////// Between (drop slots with horizontal line) ////////////////////////////////////////

  private def betweenList[Env, Action, A](
      st: WidgetState[State[A]],
      items: Vector[A],
      elemWidget: Widget.Stateful[Env, Action, A],
      style: Style,
  ): Widget.Stateful[Env, Action, State[A]] = {
    val n = items.size
    div(
      display.flex,
      flexDirection.column,
      gap := 0.px,
      Widget.foreach((0 to n).toList) { slot =>
        fragment(
          betweenSlot(st, slot, style),
          if slot < n then betweenRow(st, slot, elemWidget, style) else Widget.empty,
        )
      },
    )
  }

  private def betweenSlot[A](st: WidgetState[State[A]], slot: Int, style: Style): Widget = {
    val h = style.betweenSlotHeightPx.px
    div(
      Widget.raw.htmlAttr("data-oxy-drop-slot", slot.toString),
      DnD.allowDrop,
      onDragEnter.e.handle { e =>
        e.preventDefault()
        Gesture.dropAt = slot
        Gesture.setHighlight(e.currentTarget.asInstanceOf[HTMLElement], slot = true)
        ZIO.unit
      },
      onDragOver.e.handle { e =>
        e.preventDefault()
        e.dataTransfer.dropEffect = DataTransferDropEffectKind.move
        Gesture.dropAt = slot
        Gesture.setHighlight(e.currentTarget.asInstanceOf[HTMLElement], slot = true)
        ZIO.unit
      },
      onDrop.e.handle { e =>
        e.preventDefault()
        e.stopPropagation()
        val from = parseFrom(e.dataTransfer.getData(Mime))
        val toSlot = slot
        Gesture.reset()
        if from >= 0 then st.update(_.moveBetween(from, toSlot))
        else ZIO.unit
      },
      // Constant height — highlight is a class, not a height change.
      // Literal CSS transparent (NOT S.color.bg.transparent — that token is 70% black scrim).
      height := h,
      minHeight := h,
      maxHeight := h,
      margin := style.betweenSlotMargin,
      borderRadius := style.rowRadius,
      backgroundColor := "transparent",
      flexShrink := "0",
      boxSizing.borderBox,
      cursor := "default",
    )
  }

  private def betweenRow[Env, Action, A](
      st: WidgetState[State[A]],
      idx: Int,
      elemWidget: Widget.Stateful[Env, Action, A],
      style: Style,
  ): Widget.Stateful[Env, Action, State[A]] =
    rowShell(
      idx,
      elemWidget,
      style,
      onDragOver.e.handle { e =>
        e.preventDefault()
        e.dataTransfer.dropEffect = DataTransferDropEffectKind.move
        val rect = e.currentTarget.asInstanceOf[dom.Element].getBoundingClientRect()
        val mid = rect.top + rect.height / 2
        val slot = if e.clientY < mid then idx else idx + 1
        Gesture.dropAt = slot
        val row = e.currentTarget.asInstanceOf[HTMLElement]
        val root = row.parentNode
        if root != null && root.isInstanceOf[HTMLElement] then {
          val slotEl = root.asInstanceOf[HTMLElement].querySelector(s"""[data-oxy-drop-slot="$slot"]""")
          if slotEl != null then Gesture.setHighlight(slotEl.asInstanceOf[HTMLElement], slot = true)
        }
        ZIO.unit
      },
      onDrop.e.handle { e =>
        e.preventDefault()
        e.stopPropagation()
        val from = parseFrom(e.dataTransfer.getData(Mime))
        val toSlot = if Gesture.dropAt >= 0 then Gesture.dropAt else idx
        Gesture.reset()
        if from >= 0 then st.update(_.moveBetween(from, toSlot))
        else ZIO.unit
      },
    )

}
