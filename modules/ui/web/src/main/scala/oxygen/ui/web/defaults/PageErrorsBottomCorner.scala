package oxygen.ui.web.defaults

import oxygen.ui.web.*
import oxygen.ui.web.component.{*, given}
import oxygen.zio.instances.given

object PageErrorsBottomCorner extends Component.WithoutProps.Stateful[Any, Nothing, PageMessages] {

  def lifted: Widget.Const = Widget.WithPageLocalState(PageErrorsBottomCorner(), PageMessages.PageLocal)

  private final case class Styling(
      fontColor: String,
      messageBackgroundColor: String,
      backgroundHighlightColor: String,
  )
  private object Styling {

    private val success: Styling =
      Styling(
        fontColor = "#155724",
        messageBackgroundColor = "#d4edda",
        backgroundHighlightColor = "#c3e6cb",
      )

    private val info: Styling =
      Styling(
        fontColor = "#0c5460",
        messageBackgroundColor = "#d1ecf1",
        backgroundHighlightColor = "#bee5eb",
      )

    private val warning: Styling =
      Styling(
        fontColor = "#856404",
        messageBackgroundColor = "#fff3cd",
        backgroundHighlightColor = "#FFE499",
      )

    private val error: Styling =
      Styling(
        fontColor = "#721c24",
        messageBackgroundColor = "#f8d7da",
        backgroundHighlightColor = "#f5c6cb",
      )

    def of(tpe: PageMessage.Type): Styling = tpe match
      case PageMessage.Type.Success => Styling.success
      case PageMessage.Type.Info    => Styling.info
      case PageMessage.Type.Warning => Styling.warning
      case PageMessage.Type.Error   => Styling.error

  }

  override protected def component(state: PageMessages): Widget.Stateful[Any, Nothing, PageMessages] =
    div(
      position.fixed,
      bottom := "20px",
      right := "20px",
      zIndex := "100",
      maxHeight := "75vh",
      overflowY.auto, // TODO (KR) : fix ugly scroll bar
      display.flex,
      flexDirection.column,
      alignItems.flexEnd,
      background := "none",
      gap := "10px",
    )(
      // TODO (KR) : have a "close all" option
      Widget.foreach(state.pageMessages) { msg =>
        val s = Styling.of(msg.`type`)

        div(
          backgroundColor := s.messageBackgroundColor,
          color := s.fontColor,
          border := s"1px solid ${s.backgroundHighlightColor}",
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
            onClick.rs[PageMessages].handle { _.updateState(_ - msg) },
            "×",
          ),
          // Message content
          div(
            fontSize := "1rem",
            marginRight := "2.5rem",
            msg.content,
          ),
        )
      },
    )

}
