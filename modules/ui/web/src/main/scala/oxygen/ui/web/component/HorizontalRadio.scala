package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object HorizontalRadio {

  final case class Props(
      selectedStyle: Style = Style.Primary,
      notSelectedStyle: Style = Style.Off,
      size: Size = Size.Medium,
      borderColor: String = S.color.fg.inverse,
  )

  enum Style {
    case Primary
    case Positive
    case Negative
    case Alert
    case Info
    case BrandPrimary1
    case BrandPrimary2
    case Off

    private[HorizontalRadio] def modifierClasses: ClassAttr = this match
      case Style.Primary       => O.HorizontalRadio.Primary
      case Style.Positive      => O.HorizontalRadio.Positive
      case Style.Negative      => O.HorizontalRadio.Negative
      case Style.Alert         => O.HorizontalRadio.Alert
      case Style.Info          => O.HorizontalRadio.Info
      case Style.BrandPrimary1 => O.HorizontalRadio.BrandPrimary1
      case Style.BrandPrimary2 => O.HorizontalRadio.BrandPrimary2
      case Style.Off           => O.HorizontalRadio.Off

  }

  enum Size {
    case Small
    case Medium
    case Large

    private[HorizontalRadio] def modifierClasses: ClassAttr = this match
      case Size.Small  => O.HorizontalRadio.Small
      case Size.Medium => O.HorizontalRadio.Medium
      case Size.Large  => O.HorizontalRadio.Large

  }

  final case class State[S](
      options: Seq[S],
      selected: S,
  ) {

    private val lastIdx: Int = options.size - 1
    private[HorizontalRadio] val elems: Seq[(Boolean, S, Boolean)] =
      options.zipWithIndex.map { case (value, idx) =>
        (idx == 0, value, idx == lastIdx)
      }

  }
  object State {

    def initialFirst[S: StrictEnum as e]: State[S] =
      State(e.enumValues, e.enumValues.head)

    def initial[S: StrictEnum as e](initial: S): State[S] =
      State(e.enumValues, initial)

  }

  def apply[S](props: Props.type => Props = _(), show: S => String = (_: S).toString): WidgetS[HorizontalRadio.State[S]] = {
    val _props: Props = props(Props)

    Widget.state[HorizontalRadio.State[S]].fix { state =>
      val current: S = state.renderTimeValue.selected
      span(O.HorizontalRadio, _props.size.modifierClasses)(
        borderColor := _props.borderColor,
        Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
          span(
            O.HorizontalRadio.Button.optMods(_.First -> isFirst, _.Last -> isLast),
            if (opt == current) _props.selectedStyle.modifierClasses else _props.notSelectedStyle.modifierClasses,
          )(
            borderRightColor := _props.borderColor,
            show(opt),
            onClick := state.update(_.copy(selected = opt)),
          )
        },
      )
    }
  }

}
