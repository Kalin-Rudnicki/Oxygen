package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}
import zio.*

object TextArea {

  final case class Props(
      inputType: String = "text",
      width: String = 50.ch,
      height: String = 4.rem,
      inputMod: NodeModifier = NodeModifier.empty,
  )

  def apply(props: Props = Props()): WidgetAS[Form.Submit, String] =
    textArea(
      `type` := props.inputType,
      width := props.width,
      height := props.height,
      padding(S.spacing._1, S.spacing._3),
      margin(S.spacing._2, S.spacing._0),
      borderRadius := S.borderRadius._4,
      border := "none",
      outline := "none",
      Widget.state[String].fix { state =>
        value := state.unsafeCurrentValue
      },
      onKeyUp.eas[Form.Submit, String].handle { (s, rh, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
        def targetValue: String = target.value.asInstanceOf[String]
        // weird weird out-of-order dom events

        if (e.keyCode == KeyCode.Enter.keyCode && e.ctrlKey) {
          // cant be in a ZIO
          e.preventDefault()
          
          s.update(_ => targetValue) *>
            rh.raiseAction(Form.Submit)
        } else
          s.update(_ => targetValue)
      },
      onChange.es[String].handle { (s, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]
        def targetValue: String = target.value.asInstanceOf[String]
        // weird weird out-of-order dom events

        s.update(_ => targetValue)
      },
    )(props.inputMod)

}
