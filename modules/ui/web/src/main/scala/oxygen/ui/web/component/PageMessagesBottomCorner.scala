package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import oxygen.zio.instances.given
import zio.Chunk

object PageMessagesBottomCorner extends Component.WithoutProps.Stateful[Any, Nothing, PageMessages] {

  val attached: Widget = PageMessagesBottomCorner().attach(PageMessages.PageLocal)

  private final case class Styling(
      fontColor: String,
      messageBackgroundColor: String,
      backgroundHighlightColor: String,
  )
  private object Styling {

    private def make(base: CSSVar): Styling = {
      val color = base.getColorValue
      Styling(
        fontColor = color.darken(66.0),
        messageBackgroundColor = color.lighten(20.0),
        backgroundHighlightColor = color.darken(20.0),
      )
    }

    private lazy val success: Styling = make(S.color.status.positive)
    private lazy val info: Styling = make(S.color.status.informational)
    private lazy val warning: Styling = make(S.color.status.alert)
    private lazy val error: Styling = make(S.color.status.negative)

    def of(tpe: PageMessage.Type): Styling = tpe match
      case PageMessage.Type.Success => Styling.success
      case PageMessage.Type.Info    => Styling.info
      case PageMessage.Type.Warning => Styling.warning
      case PageMessage.Type.Error   => Styling.error

  }

  override protected def component(state: PageMessages): WidgetS[PageMessages] =
    div(
      position.fixed,
      bottom := 0.px,
      right := 20.px,
      zIndex := ZIndices.pageMessages,
      maxHeight := "75vh",
      display.flex,
      flexDirection.column,
      alignItems.flexEnd,
      background := "none",
      gap := "10px",
      OxygenStyleSheet.Scrollable,
      paddingBottom := 20.px,
    )(
      // TODO (KR) : have a "close all" option
      Widget.when(state.pageMessages.size > 1) {
        button(
          display.inlineBlock,
          padding(5.px, 15.px),
          backgroundColor := "#000B",
          boxShadow := "none",
          border := (1.px __ "solid" __ OxygenStyleVars.color.fg.default),
          color := OxygenStyleVars.color.fg.default,
          borderRadius := 10.px,
          marginRight := 10.px,
          cursor.pointer,
        )(
          "Close All",
          onClick.s[PageMessages].updateState(_.copy(pageMessages = Chunk.empty)),
        )
      },
      Widget.foreach(state.pageMessages) { msg =>
        val s = Styling.of(msg.`type`)

        div(
          backgroundColor := s.messageBackgroundColor,
          color := s.fontColor,
          border := s"2px solid ${s.backgroundHighlightColor}",
          borderRadius := "12px",
          padding := "1rem 1.5rem 1rem 1rem",
          boxShadow := "0 4px 16px rgba(0,0,0,0.12)",
          minWidth := "250px",
          maxWidth := "750px",
          width := "fit-content",
          display.inlineBlock,
          position.relative,
        )(
          // Close button in top right
          button(
            background := s.backgroundHighlightColor,
            color := s.fontColor,
            border := "none",
            fontSize := "1.5rem",
            cursor.pointer,
            padding("0.25rem", "0.5rem"),
            borderRadius := "16px",
            userSelect.none,
            transition := "background 0.2s",
            position.absolute,
            top := "0.5rem",
            right := "0.5rem",
            zIndex := "2",
          )(
            onClick.s[PageMessages].updateState(_ - msg),
            "×",
          ),
          // Message content
          div(
            fontSize := "1rem",
            marginRight := "2.5rem",
            msg.content,
            whiteSpace.pre,
          ),
        )
      },
    )

}
