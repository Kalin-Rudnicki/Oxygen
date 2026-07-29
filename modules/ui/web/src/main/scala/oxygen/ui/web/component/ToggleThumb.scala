package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Toggle switch (W2-T07). Pure CSS vars; no Decorator.
  *
  * Config builder; [[boolean]] / [[set]] produce Deferred widgets.
  */
final case class ToggleThumb(
    private val _enabledColor: String,
    private val _disabledColor: String,
    private val _sizing: ToggleThumb.Sizing,
    private val _trackExtra: Widget,
    private val _thumbExtra: Widget,
) {

  def size(s: ToggleThumb.Sizing): ToggleThumb = copy(_sizing = s)
  def extraSmall: ToggleThumb = size(ToggleThumb.Sizing.extraSmall)
  def small: ToggleThumb = size(ToggleThumb.Sizing.small)
  def medium: ToggleThumb = size(ToggleThumb.Sizing.medium)
  def large: ToggleThumb = size(ToggleThumb.Sizing.large)
  def extraLarge: ToggleThumb = size(ToggleThumb.Sizing.extraLarge)

  def enabledColor(c: String): ToggleThumb = copy(_enabledColor = c)
  def disabledColor(c: String): ToggleThumb = copy(_disabledColor = c)
  def colors(enabled: String, disabled: String): ToggleThumb = copy(_enabledColor = enabled, _disabledColor = disabled)

  def primaryEnabled: ToggleThumb = enabledColor(S.color.primary)
  def positiveEnabled: ToggleThumb = enabledColor(S.color.status.positive)
  def negativeEnabled: ToggleThumb = enabledColor(S.color.status.negative)
  def alertEnabled: ToggleThumb = enabledColor(S.color.status.alert)
  def informationalEnabled: ToggleThumb = enabledColor(S.color.status.informational)
  def brandPrimary1Enabled: ToggleThumb = enabledColor(S.color.brand.primary1)
  def brandPrimary2Enabled: ToggleThumb = enabledColor(S.color.brand.primary2)
  def offEnabled: ToggleThumb = enabledColor(S.color.bg.layerTwo)

  def primaryDisabled: ToggleThumb = disabledColor(S.color.primary)
  def positiveDisabled: ToggleThumb = disabledColor(S.color.status.positive)
  def negativeDisabled: ToggleThumb = disabledColor(S.color.status.negative)
  def alertDisabled: ToggleThumb = disabledColor(S.color.status.alert)
  def informationalDisabled: ToggleThumb = disabledColor(S.color.status.informational)
  def brandPrimary1Disabled: ToggleThumb = disabledColor(S.color.brand.primary1)
  def brandPrimary2Disabled: ToggleThumb = disabledColor(S.color.brand.primary2)
  def offDisabled: ToggleThumb = disabledColor(S.color.bg.layerThree)

  def primary: ToggleThumb = primaryEnabled.offDisabled
  def positive: ToggleThumb = positiveEnabled.offDisabled
  def negative: ToggleThumb = negativeEnabled.offDisabled
  def alert: ToggleThumb = alertEnabled.offDisabled
  def informational: ToggleThumb = informationalEnabled.offDisabled
  def brandPrimary1: ToggleThumb = brandPrimary1Enabled.offDisabled
  def brandPrimary2: ToggleThumb = brandPrimary2Enabled.offDisabled
  def positiveNegative: ToggleThumb = positiveEnabled.negativeDisabled

  def trackExtra(mods: Widget*): ToggleThumb = copy(_trackExtra = fragment(this._trackExtra, Widget.fragment(mods)))
  def thumbExtra(mods: Widget*): ToggleThumb = copy(_thumbExtra = fragment(this._thumbExtra, Widget.fragment(mods)))

  def boolean: ToggleThumb.BooleanToggle =
    ToggleThumb.BooleanToggle(this)

  def set[A](value: A): ToggleThumb.SetToggle[A] =
    ToggleThumb.SetToggle(this, value)

}
object ToggleThumb {

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

  /** Boolean toggle widget (Deferred). */
  final case class BooleanToggle(
      private val cfg: ToggleThumb,
  ) extends PWidget.Deferred[Any, Nothing, Boolean, Boolean] {
    override protected def build: PWidget[Any, Nothing, Boolean, Boolean] =
      mkShared[Boolean](cfg, identity, !_)
  }

  /** Set-membership toggle widget (Deferred). */
  final case class SetToggle[A](
      private val cfg: ToggleThumb,
      private val value: A,
  ) extends PWidget.Deferred[Any, Nothing, Set[A], Set[A]] {
    override protected def build: PWidget[Any, Nothing, Set[A], Set[A]] =
      mkShared[Set[A]](
        cfg,
        _.contains(value),
        set => if set.contains(value) then set - value else set + value,
      )
  }

  val empty: ToggleThumb =
    ToggleThumb(
      _enabledColor = S.color.primary,
      _disabledColor = S.color.bg.layerThree,
      _sizing = Sizing.medium,
      _trackExtra = Widget.empty,
      _thumbExtra = Widget.empty,
    )

  def apply(): ToggleThumb = empty

  def apply(configure: ToggleThumb => ToggleThumb): ToggleThumb =
    configure(empty)

  def boolean(configure: ToggleThumb => ToggleThumb = identity): BooleanToggle =
    configure(empty).boolean

  def boolean: BooleanToggle =
    empty.boolean

  def set[A](value: A, configure: ToggleThumb => ToggleThumb = identity): SetToggle[A] =
    configure(empty).set(value)

  def set[A](value: A): SetToggle[A] =
    empty.set(value)

  private def mkShared[S](
      cfg: ToggleThumb,
      isTrue: S => Boolean,
      onClickToggle: S => S,
  ): WidgetS[S] =
    Widget.state[S].fix { state =>
      val enabled: Boolean = isTrue(state.renderTimeValue)
      val sizing = cfg._sizing

      val thumb: Widget =
        div(
          O.ToggleThumb.Thumb,
          width := sizing.thumbSize.px,
          height := sizing.thumbSize.px,
          top := sizing.thumbPadding.px,
          left := sizing.thumbPadding.px,
          borderWidth := sizing.thumbBorderSize.px,
          if enabled then fragment(transform := s"translateX(${sizing.translation.px})")
          else Widget.empty,
          cfg._thumbExtra,
        )

      div(
        O.ToggleThumb.Track,
        width := sizing.trackWidth.px,
        height := sizing.trackHeight.px,
        borderWidth := sizing.trackBorderSize.px,
        borderRadius := sizing.trackHeight.px,
        if enabled then fragment(backgroundColor := cfg._enabledColor)
        else fragment(backgroundColor := cfg._disabledColor),
        thumb,
        onClick := state.update(onClickToggle),
        cfg._trackExtra,
      )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled boolean, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled boolean toggle form.
    *
    * {{{
    * ToggleThumb.form("Notifications").primary.zoomOut[Page](_.notify)
    * ToggleThumb.form("Dark mode").modToggle(_.positive)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _toggle: ToggleThumb,
      private val _label: Label,
      private val _surroundingPadding: String,
  ) extends PForm.Deferred.Stateful[Any, Nothing, Boolean, Boolean] {

    override protected lazy val build: PForm[Any, Nothing, Boolean, Boolean, Boolean] =
      Form.makeWith(
        List(_fieldName),
        div(
          padding := _surroundingPadding,
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _toggle.boolean,
          _label,
        ),
      )(identity)

    def modToggle(f: ToggleThumb => ToggleThumb): form = copy(_toggle = f(_toggle))
    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def toggle: ToggleThumb = _toggle
    def label: Label = _label

    def small: form = modToggle(_.small)
    def medium: form = modToggle(_.medium)
    def large: form = modToggle(_.large)
    def primary: form = modToggle(_.primary)
    def positive: form = modToggle(_.positive)
    def negative: form = modToggle(_.negative)
    def alert: form = modToggle(_.alert)
    def informational: form = modToggle(_.informational)

    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)

  }
  object form {

    def apply(label: String): ToggleThumb.form =
      new ToggleThumb.form(
        _fieldName = label,
        _toggle = ToggleThumb.empty,
        _label = Label(label),
        _surroundingPadding = 10.px,
      )

  }

}
