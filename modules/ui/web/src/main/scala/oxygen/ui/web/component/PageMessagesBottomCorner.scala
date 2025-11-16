package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.PageMessage.Type
import oxygen.ui.web.create.{*, given}
import oxygen.zio.instances.given
import zio.Chunk

object PageMessagesBottomCorner extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      primary: PageMessage.Styling,
      positive: PageMessage.Styling,
      negative: PageMessage.Styling,
      info: PageMessage.Styling,
      warning: PageMessage.Styling,
      error: PageMessage.Styling,
      minWidth: String,
      maxWidth: String,
      maxHeight: String,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        primary = PageMessage.Styling.make(S.color.primary.getColorValue),
        positive = PageMessage.Styling.make(S.color.status.positive.getColorValue),
        negative = PageMessage.Styling.make(S.color.status.negative.getColorValue),
        info = PageMessage.Styling.make(S.color.status.informational.getColorValue),
        warning = PageMessage.Styling.make(S.color.status.alert.getColorValue),
        error = PageMessage.Styling.make(S.color.status.negative.getColorValue),
        minWidth = 250.px,
        maxWidth = 750.px,
        maxHeight = 75.vh,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final def primary(styling: PageMessage.Styling): Decorator = make("styling(primary)") { _.copy(primary = styling) }
    final def positive(styling: PageMessage.Styling): Decorator = make("styling(positive)") { _.copy(positive = styling) }
    final def negative(styling: PageMessage.Styling): Decorator = make("styling(negative)") { _.copy(negative = styling) }
    final def info(styling: PageMessage.Styling): Decorator = make("styling(info)") { _.copy(info = styling) }
    final def warning(styling: PageMessage.Styling): Decorator = make("styling(warning)") { _.copy(warning = styling) }
    final def error(styling: PageMessage.Styling): Decorator = make("styling(error)") { _.copy(error = styling) }

  }

  final class Decorator private[PageMessagesBottomCorner] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // TODO (KR) : support an icon
  private def message(props: Props, msg: PageMessage, parentState: WidgetState[PageMessages]): Widget = {
    val s: PageMessage.Styling = msg.`type` match
      case Type.Primary         => props.primary
      case Type.Positive        => props.positive
      case Type.Negative        => props.negative
      case Type.Info            => props.info
      case Type.Warning         => props.warning
      case Type.Error           => props.error
      case Type.Custom(styling) => styling

    div(
      backgroundColor := s.backgroundColor,
      color := s.fontColor,
      border := s"2px solid ${s.backgroundColor}",
      borderRadius := 12.px,
      padding := "1rem 1.5rem 1rem 1rem",
      boxShadow := "0 4px 16px rgba(0,0,0,0.12)",
      minWidth := props.minWidth,
      maxWidth := props.maxWidth,
      width.fitContent,
      display.inlineBlock,
      position.relative,
    )(
      // Close button in top right
      button(
        background := s.buttonColor,
        color := s.fontColor,
        border := "none",
        fontSize := "1.5rem",
        cursor.pointer,
        borderRadius := 12.px,
        userSelect.none,
        transition := "background 0.2s",
        position.absolute,
        fontWeight := S.fontWeight.semiBold,
        top := "0.5rem",
        right := "0.5rem",
        zIndex := "2",
        width := 30.px,
        height := 30.px,
      )(
        onClick := parentState.update(_ - msg),
        "×",
      ),
      // Message content
      div(
        fontSize := "1rem",
        marginRight := "2.5rem",
        fontWeight := S.fontWeight.semiBold,
        msg.content,
        whiteSpace.pre,
      ),
    )
  }

  private def internal(decorator: Decorator): WidgetS[PageMessages] = {
    val props = decorator.computed

    Widget.state[PageMessages].fix { state =>
      div(
        position.fixed,
        bottom := 0.px,
        right := 20.px,
        zIndex := ZIndices.pageMessages,
        background := "none",
        maxHeight := props.maxHeight,
        display.flex,
        flexDirection.column,
        alignItems.flexEnd,
        gap := 10.px,
        paddingBottom := 20.px,
      )(
        Widget.when(state.get.pageMessages.size > 1) {
          button(
            display.inlineBlock,
            padding(5.px, 15.px),
            backgroundColor := "#000B",
            boxShadow := "none",
            border(1.px, OxygenStyleVars.color.fg.default),
            color := OxygenStyleVars.color.fg.default,
            borderRadius := 10.px,
            marginRight := 10.px,
            cursor.pointer,
          )(
            "Close All",
            onClick.s[PageMessages].updateState(_.copy(pageMessages = Chunk.empty)),
          )
        },
        div(
          OxygenStyleSheet.Scrollable,
          maxHeight := props.maxHeight,
          display.flex,
          flexDirection.column,
          alignItems.flexEnd,
          gap := 10.px,
          Widget.foreach(state.get.pageMessages)(message(props, _, state)),
        ),
      )
    }
  }

  def apply(decorator: Decorator): Widget =
    internal(decorator).attach(PageMessages.PageLocal)

  def apply(decorator: Decorator => Decorator): Widget =
    apply(decorator(Decorator.defaultStyling))

  def apply(): Widget =
    default

  lazy val default: Widget =
    apply(Decorator.defaultStyling)

}
