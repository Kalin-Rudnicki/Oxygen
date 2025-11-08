package oxygen.ui.web.component

import oxygen.ui.web.create.{*, given}

object ToggleThumb extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      _enabledColor: String,
      _disabledColor: String,
      _sizing: Sizing,
      _trackMod: NodeModifier,
      _thumbMod: NodeModifier,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props("transparent", "transparent", Sizing(0, 0, 0, 0, 0), NodeModifier.empty, NodeModifier.empty)

  }

  final case class Sizing(
      trackHeight: Int,
      trackWidth: Int,
      thumbPadding: Int,
      trackBorderSize: Int,
      thumbBorderSize: Int,
  ) {

    val thumbSize: Int = trackHeight - 2 * (trackBorderSize + thumbPadding)
    val translation: Int = trackWidth - trackHeight

  }
  object Sizing {

    val extraSmall: Sizing = Sizing(15, 30, 2, 2, 1)
    val small: Sizing = Sizing(20, 40, 3, 2, 1)
    val medium: Sizing = Sizing(25, 50, 3, 2, 1)
    val large: Sizing = Sizing(30, 60, 3, 2, 1)
    val extraLarge: Sizing = Sizing(35, 70, 4, 3, 2)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    /////// Size ///////////////////////////////////////////////////////////////

    private def makeSize(name: String, size: Sizing): Decorator =
      make(name)(_.copy(_sizing = size))

    final lazy val extraSmall: Decorator = makeSize("ExtraSmall", Sizing.extraSmall)
    final lazy val small: Decorator = makeSize("Small", Sizing.small)
    final lazy val medium: Decorator = makeSize("Medium", Sizing.medium)
    final lazy val large: Decorator = makeSize("Large", Sizing.large)
    final lazy val extraLarge: Decorator = makeSize("ExtraLarge", Sizing.extraLarge)
    final def size(size: Sizing): Decorator = makeSize("CustomSize", size)

    /////// Colors ///////////////////////////////////////////////////////////////

    private def makeEnabled(name: String, color: String): Decorator = make(s"EnabledColor($name)") { _.copy(_enabledColor = color) }
    private def makeDisabled(name: String, color: String): Decorator = make(s"DisabledColor($name)") { _.copy(_disabledColor = color) }

    final lazy val primaryEnabled: Decorator = makeEnabled("Primary", S.color.primary)
    final lazy val positiveEnabled: Decorator = makeEnabled("Positive", S.color.status.positive)
    final lazy val negativeEnabled: Decorator = makeEnabled("Negative", S.color.status.negative)
    final lazy val alertEnabled: Decorator = makeEnabled("Alert", S.color.status.alert)
    final lazy val informationalEnabled: Decorator = makeEnabled("Informational", S.color.status.informational)
    final lazy val brandPrimary1Enabled: Decorator = makeEnabled("BrandPrimary1", S.color.brand.primary1)
    final lazy val brandPrimary2Enabled: Decorator = makeEnabled("BrandPrimary2", S.color.brand.primary2)
    final lazy val offEnabled: Decorator = makeEnabled("Off", S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0))

    final lazy val primaryDisabled: Decorator = makeDisabled("Primary", S.color.primary)
    final lazy val positiveDisabled: Decorator = makeDisabled("Positive", S.color.status.positive)
    final lazy val negativeDisabled: Decorator = makeDisabled("Negative", S.color.status.negative)
    final lazy val alertDisabled: Decorator = makeDisabled("Alert", S.color.status.alert)
    final lazy val informationalDisabled: Decorator = makeDisabled("Informational", S.color.status.informational)
    final lazy val brandPrimary1Disabled: Decorator = makeDisabled("BrandPrimary1", S.color.brand.primary1)
    final lazy val brandPrimary2Disabled: Decorator = makeDisabled("BrandPrimary2", S.color.brand.primary2)
    final lazy val offDisabled: Decorator = makeDisabled("Off", S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0))

    final lazy val primary: Decorator = primaryEnabled.offDisabled
    final lazy val positive: Decorator = positiveEnabled.offDisabled
    final lazy val negative: Decorator = negativeEnabled.offDisabled
    final lazy val alert: Decorator = alertEnabled.offDisabled
    final lazy val informational: Decorator = informationalEnabled.offDisabled
    final lazy val brandPrimary1: Decorator = brandPrimary1Enabled.offDisabled
    final lazy val brandPrimary2: Decorator = brandPrimary2Enabled.offDisabled
    final lazy val positiveNegative: Decorator = positiveEnabled.negativeDisabled

    final def enabledColor(color: String): Decorator = makeEnabled(color, color)
    final def disabledColor(color: String): Decorator = makeDisabled(color, color)
    final def colors(enabled: String, disabled: String): Decorator = enabledColor(enabled) >> disabledColor(disabled)

    /////// Misc ///////////////////////////////////////////////////////////////

    final def setCustomMod(mod: NodeModifier): Decorator = make("custom(setMod)") { _.copy(_trackMod = mod) }
    final def addCustomMod(mod: NodeModifier): Decorator = make("custom(addMod)") { p => p.copy(_trackMod = p._trackMod <> mod) }
    final def prepend(before: Widget*): Decorator = addCustomMod(NodeModifier.before(before*))
    final def append(after: Widget*): Decorator = addCustomMod(NodeModifier.after(after*))
    final def surround(before: Widget*)(after: Widget*): Decorator = addCustomMod(NodeModifier.surround(before*)(after*))

    final def setCustomThumbMod(mod: NodeModifier): Decorator = make("custom(setThumbMod)") { _.copy(_thumbMod = mod) }
    final def addCustomThumbMod(mod: NodeModifier): Decorator = make("custom(addThumbMod)") { p => p.copy(_thumbMod = p._thumbMod <> mod) }
    final def prependThumb(before: Widget*): Decorator = addCustomThumbMod(NodeModifier.before(before*))
    final def appendThumb(after: Widget*): Decorator = addCustomThumbMod(NodeModifier.after(after*))
    final def surroundThumb(before: Widget*)(after: Widget*): Decorator = addCustomThumbMod(NodeModifier.surround(before*)(after*))

  }

  final class Decorator private[ToggleThumb] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.primary.medium

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def mkShared[S](
      decorator: Decorator,
      isTrue: S => Boolean,
      onClickToggle: S => S,
  ): WidgetS[S] = {
    val props: Props = decorator.computed

    Widget.state[S].fix { state =>
      val enabled: Boolean = isTrue(state.renderTimeValue)

      val thumb: Widget =
        div(
          O.ToggleThumb.Thumb,
          width := props._sizing.thumbSize.px,
          height := props._sizing.thumbSize.px,
          top := props._sizing.thumbPadding.px,
          left := props._sizing.thumbPadding.px,
          borderWidth := props._sizing.thumbBorderSize.px,
          if (enabled)
            fragment(
              transform := s"translateX(${props._sizing.translation.px})",
            )
          else
            Widget.empty,
        )(props._thumbMod)

      div(
        O.ToggleThumb.Track,
        width := props._sizing.trackWidth.px,
        height := props._sizing.trackHeight.px,
        borderWidth := props._sizing.trackBorderSize.px,
        borderRadius := props._sizing.trackHeight.px,
        if (enabled)
          fragment(
            backgroundColor := props._enabledColor,
          )
        else
          fragment(
            backgroundColor := props._disabledColor,
          ),
        thumb,
        onClick := state.update(onClickToggle),
      )(props._trackMod)
    }
  }

  def boolean(decorator: Decorator): WidgetS[Boolean] =
    mkShared[Boolean](decorator, identity, !_)

  def boolean(decorator: Decorator => Decorator): WidgetS[Boolean] =
    boolean(decorator(Decorator.defaultStyling))

  def boolean: WidgetS[Boolean] =
    boolean(Decorator.defaultStyling)

  def set[A](value: A, decorator: Decorator): WidgetS[Set[A]] =
    mkShared[Set[A]](
      decorator,
      _.contains(value),
      set =>
        if (set.contains(value)) set - value
        else set + value,
    )

  def set[A](value: A, decorator: Decorator => Decorator): WidgetS[Set[A]] =
    set[A](value, decorator(Decorator.defaultStyling))

  def set[A](value: A): WidgetS[Set[A]] =
    set[A](value, Decorator.defaultStyling)

}
