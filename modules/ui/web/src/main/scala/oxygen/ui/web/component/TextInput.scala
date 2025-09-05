package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}
import zio.*

object TextInput {

  final case class Props(
      inputType: String = "text",
      width: String = 50.ch,
      inputMod: NodeModifier = NodeModifier.empty,
  )

  def apply(props: Props = Props()): WidgetAS[Form.Submit, String] =
    input(
      `type` := props.inputType,
      width := props.width,
      padding(S.spacing._2, S.spacing._4),
      margin(S.spacing._2, S.spacing._0),
      borderRadius := S.borderRadius._4,
      border := "none",
      outline := "none",
      Widget.state[String].fix { state =>
        value := state.unsafeCurrentValue
      },
      onKeyUp.eas[Form.Submit, String].handle { (s, rh, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
        val targetValue = target.value.asInstanceOf[String]

        if (e.keyCode == KeyCode.Enter.keyCode) {
          ZIO.succeed { e.preventDefault() } *>
            s.set(targetValue) *>
            rh.raiseAction(Form.Submit)
        } else
          s.set(targetValue)
      },
      onChange.es[String].handle { (s, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
        val targetValue = target.value.asInstanceOf[String]

        s.set(targetValue)
      },
    )(props.inputMod)

}
