package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

object Section {

  object section1 {

    // TODO (KR) : add more of these spacing properties to Props
    final case class Props(
        headerColor: String = S.color.primary,
    )

    val empty: Node =
      div(
        margin(S.spacing._2, S.spacing._14),
        padding(S.spacing._5, S.spacing._10),
        borderRadius := S.borderRadius._8,
        backgroundColor := S.color.bg.layerOne,
      )

    def header(headerText: String, props: Props.type => Props = _()): Node = {
      val _props: Props = props(Props)
      h2(
        headerText,
        color := _props.headerColor,
        padding(S.spacing._1, S.spacing._10),
        margin(0.px, S.spacing._10),
        borderBottom(2.px, "solid", _props.headerColor),
        width.fitContent,
      )
    }

    def apply(headerText: String, props: Props.type => Props = _()): Node =
      empty(
        header(headerText, props),
        div(height := S.spacing._5),
      )

  }

  object section2 {

    // TODO (KR) : add more of these spacing properties to Props
    final case class Props(
        headerColor: String = S.color.fg.moderate,
    )

    val empty: Node =
      div(
        margin(S.spacing._2, S.spacing._0),
        padding(S.spacing._5, S.spacing._10),
        borderRadius := S.borderRadius._5,
        backgroundColor := S.color.bg.layerTwo,
      )

    def header(headerText: String, props: Props.type => Props = _()): Node = {
      val _props: Props = props(Props)
      h2(
        headerText,
        color := _props.headerColor,
        padding(S.spacing._1, S.spacing._5),
        margin(0.px, S.spacing._10),
        borderBottom(2.px, "solid", _props.headerColor),
        width.fitContent,
      )
    }

    def apply(headerText: String, props: Props.type => Props = _()): Node =
      empty(
        header(headerText, props),
        div(height := S.spacing._5),
      )

  }

  object info {

    final case class Props(
        borderColor: String = S.color.status.informational,
        backHighlight: Boolean = false, // if you want to use this, your color must be a 6-digit hex string, or a variable that points to one
    )

    def apply(props: Props.type => Props = _()): Node = {
      val _props: Props = props(Props)
      p(
        margin(S.spacing._2, S.spacing._0),
        padding(S.spacing._2, S.spacing._4),
        borderLeft(2.px, "solid", _props.borderColor),
        Widget.when(_props.backHighlight) {
          backgroundColor := CSSColor.eval(_props.borderColor).setOpacity(10.0)
        },
      )
    }

  }

}
