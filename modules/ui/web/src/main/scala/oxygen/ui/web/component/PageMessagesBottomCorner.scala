package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.PageMessage.Type
import oxygen.ui.web.create.{*, given}
import oxygen.zio.instances.given
import zio.Chunk

/**
  * Bottom-corner page messages (W2). Pure CSS-var styling; no Decorator.
  */
final case class PageMessagesBottomCorner(
    private val primary: PageMessage.Styling,
    private val positive: PageMessage.Styling,
    private val negative: PageMessage.Styling,
    private val info: PageMessage.Styling,
    private val warning: PageMessage.Styling,
    private val error: PageMessage.Styling,
    private val minWidth: String,
    private val maxWidth: String,
    private val maxHeight: String,
) extends PWidget.Deferred[Any, Nothing, Any, Nothing] {

  def primary(s: PageMessage.Styling): PageMessagesBottomCorner = copy(primary = s)
  def positive(s: PageMessage.Styling): PageMessagesBottomCorner = copy(positive = s)
  def negative(s: PageMessage.Styling): PageMessagesBottomCorner = copy(negative = s)
  def info(s: PageMessage.Styling): PageMessagesBottomCorner = copy(info = s)
  def warning(s: PageMessage.Styling): PageMessagesBottomCorner = copy(warning = s)
  def error(s: PageMessage.Styling): PageMessagesBottomCorner = copy(error = s)
  def minWidth(w: String): PageMessagesBottomCorner = copy(minWidth = w)
  def maxWidth(w: String): PageMessagesBottomCorner = copy(maxWidth = w)
  def maxHeight(h: String): PageMessagesBottomCorner = copy(maxHeight = h)

  override protected def build: PWidget[Any, Nothing, Any, Nothing] =
    PageMessagesBottomCorner.render(this).attach(PageMessages.PageLocal)

}
object PageMessagesBottomCorner {

  val empty: PageMessagesBottomCorner =
    PageMessagesBottomCorner(
      primary = PageMessage.Styling.fromRole(S.color.primary.standard, S.color.primary.subtle, S.color.primary.hover),
      positive = PageMessage.Styling.fromRole(S.color.status.positive.standard, S.color.status.positive.subtle, S.color.status.positive.hover),
      negative = PageMessage.Styling.fromRole(S.color.status.negative.standard, S.color.status.negative.subtle, S.color.status.negative.hover),
      info = PageMessage.Styling.fromRole(S.color.status.informational.standard, S.color.status.informational.subtle, S.color.status.informational.hover),
      warning = PageMessage.Styling.fromRole(S.color.status.alert.standard, S.color.status.alert.subtle, S.color.status.alert.hover),
      error = PageMessage.Styling.fromRole(S.color.status.negative.standard, S.color.status.negative.subtle, S.color.status.negative.hover),
      minWidth = 250.px,
      maxWidth = 750.px,
      maxHeight = 75.vh,
    )

  def apply(): PageMessagesBottomCorner = empty

  def apply(configure: PageMessagesBottomCorner => PageMessagesBottomCorner): PageMessagesBottomCorner =
    configure(empty)

  lazy val default: PageMessagesBottomCorner = empty

  private def message(cfg: PageMessagesBottomCorner, msg: PageMessage, parentState: WidgetState[PageMessages]): Widget = {
    val s: PageMessage.Styling = msg.`type` match
      case Type.Primary         => cfg.primary
      case Type.Positive        => cfg.positive
      case Type.Negative        => cfg.negative
      case Type.Info            => cfg.info
      case Type.Warning         => cfg.warning
      case Type.Error           => cfg.error
      case Type.Custom(styling) => styling

    // Flex card (not absolute × on overflow): long content wraps inside maxWidth;
    // white-space:pre was the overflow bomb (no wrap, fit-content grows forever).
    div(
      backgroundColor := s.backgroundColor,
      color := s.fontColor,
      border := s"2px solid ${s.borderColor}",
      borderRadius := 12.px,
      padding := css(S.spacing._3, S.spacing._3),
      boxShadow := "0 4px 16px rgba(0,0,0,0.12)",
      boxSizing.borderBox,
      // Grow with content up to the stack width; never paint outside the card.
      // flex-shrink: 0 so a tall stack scrolls the list instead of squashing toasts.
      flexShrink := "0",
      width.fitContent,
      maxWidth := 100.pct,
      minWidth := s"min(${cfg.minWidth}, 100%)",
      overflow.hidden,
      display.flex,
      flexDirection.row,
      alignItems.flexStart,
      gap := S.spacing._3,
    )(
      div(
        flex := "1 1 auto",
        minWidth := 0.px, // allow flex child to shrink below content min-size
        maxWidth := 100.pct,
        fontSize := S.fontSize._3,
        fontWeight := S.fontWeight.semiBold,
        lineHeight := "1.4",
        // Preserve intentional newlines; wrap long runs / unbroken strings.
        whiteSpace.preWrap,
        wordWrap := "break-word",
        wordBreak := "break-word",
        Widget.raw.css("overflow-wrap", "anywhere"),
        msg.content,
      ),
      button(
        O.Button,
        flex := "0 0 auto",
        flexShrink := "0",
        backgroundColor := s.buttonColor,
        color := S.color.fg.globalWhite,
        border := "none",
        fontSize := S.fontSize._5,
        lineHeight := "1",
        cursor.pointer,
        borderRadius := S.borderRadius._3,
        userSelect.none,
        fontWeight := S.fontWeight.semiBold,
        width := 28.px,
        height := 28.px,
        minWidth := 28.px,
        minHeight := 28.px,
        padding := 0.px,
        display.inlineFlex,
        alignItems.center,
        justifyContent.center,
        boxSizing.borderBox,
      )(
        onClick := parentState.update(_ - msg),
        Widget.raw.htmlAttr("aria-label", "Dismiss message"),
        Widget.raw.htmlAttr("title", "Dismiss"),
        "×",
      ),
    )
  }

  private def render(cfg: PageMessagesBottomCorner): WidgetS[PageMessages] =
    Widget.state[PageMessages].fix { state =>
      // Outer is only a positioning shell. The *list* owns maxHeight + O.Scrollable so
      // overflowing stacks scroll instead of painting off-screen or compacting cards.
      val stackMaxW = s"min(${cfg.maxWidth}, calc(100vw - 24px))"
      // Leave room for Close All + padding so the scrollport fits in the viewport.
      val listMaxH = s"min(${cfg.maxHeight}, calc(100dvh - 96px))"

      div(
        position.fixed,
        bottom := 0.px,
        right := 0.px,
        zIndex := ZIndices.pageMessages,
        background := "none",
        maxWidth := stackMaxW,
        width := stackMaxW,
        boxSizing.borderBox,
        display.flex,
        flexDirection.column,
        // Right-align: short toasts hug content width; long ones still cap at maxWidth.
        alignItems.flexEnd,
        gap := 10.px,
        padding := css(0.px, 12.px, 20.px, 12.px),
        // Do not put maxHeight/overflow on the shell — that clips without a scrollbar.
        pointerEvents.none,
      )(
        Widget.when(state.get.pageMessages.size > 1) {
          button(
            pointerEvents.auto,
            flexShrink := "0",
            display.inlineBlock,
            padding(5.px, 15.px),
            backgroundColor := S.color.bg.layerThree,
            boxShadow := "none",
            border(1.px, "solid", S.color.fg.subtle),
            color := S.color.fg.default,
            borderRadius := 10.px,
            cursor.pointer,
          )(
            "Close All",
            onClick.s[PageMessages].updateState(_.copy(pageMessages = Chunk.empty)),
          )
        },
        div(
          O.Scrollable,
          pointerEvents.auto,
          // Hard cap: content taller than this scrolls (O.Scrollable → overflow-y: auto).
          maxHeight := listMaxH,
          // Flex min-size:auto would let content force the box past maxHeight in some engines.
          minHeight := 0.px,
          maxWidth := 100.pct,
          width := 100.pct,
          boxSizing.borderBox,
          display.flex,
          flexDirection.column,
          alignItems.flexEnd,
          gap := 10.px,
          overflowX.hidden,
          // Keep cards at natural size; only the list scrolls.
          flexWrap.nowrap,
          Widget.foreach(state.get.pageMessages)(message(cfg, _, state)),
        ),
      )
    }

}
