package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}

object ToggleThumb {

  final case class Props(
      onStyle: Style = Style.Primary,
      offStyle: Style = Style.Off,
      size: Size = Size.Medium,
      trackMods: NodeModifier = NodeModifier.empty,
      thumbMods: NodeModifier = NodeModifier.empty,
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

    private[ToggleThumb] def modifierClasses: ClassAttr = this match
      case Style.Primary       => O.ToggleThumb.Primary
      case Style.Positive      => O.ToggleThumb.Positive
      case Style.Negative      => O.ToggleThumb.Negative
      case Style.Alert         => O.ToggleThumb.Alert
      case Style.Info          => O.ToggleThumb.Info
      case Style.BrandPrimary1 => O.ToggleThumb.BrandPrimary1
      case Style.BrandPrimary2 => O.ToggleThumb.BrandPrimary2
      case Style.Off           => O.ToggleThumb.Off

  }

  enum Size {
    case Small
    case Medium
    case Large

    private[ToggleThumb] def modifierClasses: ClassAttr = this match
      case Size.Small  => O.ToggleThumb.Small
      case Size.Medium => O.ToggleThumb.Medium
      case Size.Large  => O.ToggleThumb.Large

  }

  private def mkShared[S](
      props: Props.type => Props,
      isTrue: S => Boolean,
      onClickToggle: S => S,
  ): WidgetS[S] = {
    val _props: Props = props(Props)

    Widget.state[S].fix { state =>
      div(
        if (isTrue(state.renderTimeValue)) fragment(O.ToggleThumb.Enabled, _props.onStyle.modifierClasses)
        else fragment(O.ToggleThumb.Disabled, _props.offStyle.modifierClasses),
        _props.size.modifierClasses,
        div(O.ToggleThumb.Thumb)(_props.thumbMods),
        onClick := state.update(onClickToggle),
      )(_props.trackMods)
    }
  }

  def apply(props: Props.type => Props = _()): WidgetS[Boolean] =
    boolean(props)

  def boolean(props: Props.type => Props = _()): WidgetS[Boolean] =
    mkShared(
      props,
      identity,
      !_,
    )

  def set[A](value: A, props: Props.type => Props = _()): WidgetS[Set[A]] =
    mkShared(
      props,
      _.contains(value),
      set =>
        if (set.contains(value)) set - value
        else set + value,
    )

}
