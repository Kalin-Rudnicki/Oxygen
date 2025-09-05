package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}

object Button {
  // TODO (KR) : have a way to represent "loading"
  // TODO (KR) : have a way to add icons (left/right)
  // TODO (KR) : have a way to add description text

  final case class Props(
      style: Style = Style.Primary,
      size: Size = Size.Medium,
      mod: NodeModifier = NodeModifier.empty,
  )

  // TODO (KR) : create these as a shared styling
  //           : can be used for page messages and/or other things
  enum Style {
    case Primary, PrimarySubtle, PrimaryMinimal
    case Destructive, DestructiveSubtle, DestructiveMinimal
    case Positive, PositiveSubtle, PositiveMinimal
    case Negative, NegativeSubtle, NegativeMinimal
    case Alert, AlertSubtle, AlertMinimal
    case Info, InfoSubtle, InfoMinimal
    case Disabled, DisabledProgress

    private[Button] def modifierClasses: ClassAttr = this match
      case Style.Primary            => O.Button.Primary
      case Style.PrimarySubtle      => O.Button.PrimarySubtle
      case Style.PrimaryMinimal     => O.Button.PrimaryMinimal
      case Style.Destructive        => O.Button.Destructive
      case Style.DestructiveSubtle  => O.Button.DestructiveSubtle
      case Style.DestructiveMinimal => O.Button.DestructiveMinimal
      case Style.Positive           => O.Button.Positive
      case Style.PositiveSubtle     => O.Button.PositiveSubtle
      case Style.PositiveMinimal    => O.Button.PositiveMinimal
      case Style.Negative           => O.Button.Negative
      case Style.NegativeSubtle     => O.Button.NegativeSubtle
      case Style.NegativeMinimal    => O.Button.NegativeMinimal
      case Style.Alert              => O.Button.Alert
      case Style.AlertSubtle        => O.Button.AlertSubtle
      case Style.AlertMinimal       => O.Button.AlertMinimal
      case Style.Info               => O.Button.Info
      case Style.InfoSubtle         => O.Button.InfoSubtle
      case Style.InfoMinimal        => O.Button.InfoMinimal
      case Style.Disabled           => O.Button.Disabled
      case Style.DisabledProgress   => O.Button.DisabledProgress

  }

  enum Size {
    case Small
    case Medium
    case Large

    private[Button] def modifierClasses: ClassAttr = this match
      case Size.Small  => O.Button.Small
      case Size.Medium => O.Button.Medium
      case Size.Large  => O.Button.Large

  }

  def apply(mainText: String, props: Props.type => Props = _()): Node = {
    val _props: Props = props(Props)
    button(
      O.Button,
      _props.style.modifierClasses,
      _props.size.modifierClasses,
      mainText,
    )(_props.mod)
  }

}
