package oxygen.ui.web.component

import oxygen.meta.typing.UnionRemoving
import oxygen.ui.web.create.{*, given}
import zio.*

object Modal extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      width: String,
      height: String,
      opacityPercent: Double,
      backgroundColor: String,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        width = 50.vw,
        height = 50.vh,
        opacityPercent = 70,
        backgroundColor = S.color.bg.layerOne,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final def width(width: String): Decorator = make(s"custom(width = $width)") { _.copy(width = width) }
    final def height(height: String): Decorator = make(s"custom(height = $height)") { _.copy(height = height) }
    final def opacityPercent(opacityPercent: Double): Decorator = make(s"custom(opacityPercent = $opacityPercent)") { _.copy(opacityPercent = opacityPercent) }
    final def backgroundColor(backgroundColor: String): Decorator = make(s"custom(backgroundColor = $backgroundColor)") { _.copy(backgroundColor = backgroundColor) }

  }

  final class Decorator private[Modal] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  type Close = Close.type
  case object Close

  def option[Env: HasNoScope, Action >: Close, State](
      decorator: Decorator,
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] = {
    val props = decorator.computed

    val tmp1: WidgetEAS[Env, Action, State] =
      div(
        O.ModalOverlay,
        backgroundColor := CSSColor("#000").setOpacity(props.opacityPercent),
        onClick.action(Close),
        div(
          O.ModalOverlay.Modal,
          width := props.width,
          height := props.height,
          backgroundColor := props.backgroundColor,
          onClick.e.handle { e =>
            e.stopPropagation(); ZIO.unit
          },
        )(contents*),
      )

    val tmp2: WidgetEAS[Env, Action, Option[State]] =
      div(Widget.sum.option(tmp1))

    tmp2.handleActionStateful.ps[Close] { case (s, Close) => s.set(None) }
  }

  def option[Env: HasNoScope, Action >: Close, State](
      decorator: Decorator => Decorator,
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] =
    Modal.option[Env, Action, State](decorator(Decorator.defaultStyling))(contents*)

  def option[Env: HasNoScope, Action >: Close, State](
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] =
    Modal.option[Env, Action, State](Decorator.defaultStyling)(contents*)

}
