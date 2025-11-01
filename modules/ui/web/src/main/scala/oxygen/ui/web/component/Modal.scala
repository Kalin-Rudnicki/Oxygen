package oxygen.ui.web.component

import oxygen.meta.typing.UnionRemoving
import oxygen.ui.web.create.{*, given}
import zio.*

object Modal {

  type Close = Close.type
  case object Close

  // TODO (KR) : optionally include a close button
  final case class Props(
      width: String = 50.vw,
      height: String = 50.vh,
      opacityPercent: Double = 70,
      backgroundColor: String = S.color.bg.layerOne,
  )

  def option[Env: HasNoScope, Action >: Close, State](
      props: Props.type => Props = _(),
  )(contents: WidgetEAS[Env, Action, State]*)(using
      ev: UnionRemoving[Action, Close],
  ): WidgetEAS[Env, ev.Remaining, Option[State]] = {
    val _props: Props = props(Props)

    val tmp1: WidgetEAS[Env, Action, State] =
      div(
        O.ModalOverlay,
        backgroundColor := CSSColor("#000").setOpacity(_props.opacityPercent),
        onClick.action(Close),
        div(
          O.ModalOverlay.Modal,
          width := _props.width,
          height := _props.height,
          backgroundColor := _props.backgroundColor,
          onClick.e.handle { e =>
            e.stopPropagation(); ZIO.unit
          },
        )(contents*),
      )

    val tmp2: WidgetEAS[Env, Action, Option[State]] =
      div(Widget.sum.option(tmp1))

    tmp2.handleActionStateful.ps[Close] { case (s, Close) => s.set(None) }
  }

}
