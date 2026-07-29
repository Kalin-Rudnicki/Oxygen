package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.{RaiseHandler, UIError, *}
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * Segmented horizontal radio (W2-T08). Pure CSS vars; no Decorator / ColorTransform.
  */
// TODO (KR) : clean this up, extend `Deferred`
final case class HorizontalRadio(
    private val selectedFG: String,
    private val selectedBG: String,
    private val selectedHover: String,
    private val notSelectedFG: String,
    private val notSelectedBG: String,
    private val notSelectedHover: String,
    private val padding: StandardProps.Padding,
    private val externalBorderSize: String,
    private val internalBorderSize: String,
    private val borderRadius: String,
    private val borderColor: String,
    private val fontSize: String,
    private val extra: Widget,
    private val selectedButtonExtra: Widget,
    private val notSelectedButtonExtra: Widget,
) {

  def small: HorizontalRadio =
    copy(padding = StandardProps.Padding(S.spacing._1, S.spacing._3), borderRadius = S.borderRadius._3, fontSize = S.fontSize._2)
  def medium: HorizontalRadio =
    copy(padding = StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._4), borderRadius = S.borderRadius._4, fontSize = S.fontSize._4)
  def large: HorizontalRadio =
    copy(padding = StandardProps.Padding(S.spacing._2, S.spacing._5), borderRadius = S.borderRadius._5, fontSize = S.fontSize._6)

  def selectedColors(bg: String, fg: String, hover: String): HorizontalRadio =
    copy(selectedBG = bg, selectedFG = fg, selectedHover = hover)
  def notSelectedColors(bg: String, fg: String, hover: String): HorizontalRadio =
    copy(notSelectedBG = bg, notSelectedFG = fg, notSelectedHover = hover)

  def primarySelected: HorizontalRadio =
    selectedColors(S.color.primary.standard, S.color.primary.on, S.color.primary.hover)
  def positiveSelected: HorizontalRadio =
    selectedColors(S.color.status.positive.standard, S.color.status.positive.on, S.color.status.positive.hover)
  def negativeSelected: HorizontalRadio =
    selectedColors(S.color.status.negative.standard, S.color.status.negative.on, S.color.status.negative.hover)
  def alertSelected: HorizontalRadio =
    selectedColors(S.color.status.alert.standard, S.color.status.alert.on, S.color.status.alert.hover)
  def informationalSelected: HorizontalRadio =
    selectedColors(S.color.status.informational.standard, S.color.status.informational.on, S.color.status.informational.hover)
  def brandPrimary1Selected: HorizontalRadio =
    selectedColors(S.color.brand.primary1.standard, S.color.fg.default, S.color.brand.primary1.light)
  def brandPrimary2Selected: HorizontalRadio =
    selectedColors(S.color.brand.primary2.standard, S.color.fg.default, S.color.brand.primary2.light)
  def offSelected: HorizontalRadio =
    selectedColors(S.color.bg.layerTwo, S.color.fg.default, S.color.bg.layerOne)

  def primaryNotSelected: HorizontalRadio =
    notSelectedColors(S.color.primary.subtle, S.color.fg.inverse, S.color.primary.minimal)
  def positiveNotSelected: HorizontalRadio =
    notSelectedColors(S.color.status.positive.subtle, S.color.fg.inverse, S.color.status.positive.minimal)
  def negativeNotSelected: HorizontalRadio =
    notSelectedColors(S.color.status.negative.subtle, S.color.fg.inverse, S.color.status.negative.minimal)
  def alertNotSelected: HorizontalRadio =
    notSelectedColors(S.color.status.alert.subtle, S.color.fg.inverse, S.color.status.alert.minimal)
  def informationalNotSelected: HorizontalRadio =
    notSelectedColors(S.color.status.informational.subtle, S.color.fg.inverse, S.color.status.informational.minimal)
  def brandPrimary1NotSelected: HorizontalRadio =
    notSelectedColors(S.color.brand.primary1.light, S.color.fg.inverse, S.color.brand.primary1.standard)
  def brandPrimary2NotSelected: HorizontalRadio =
    notSelectedColors(S.color.brand.primary2.light, S.color.fg.inverse, S.color.brand.primary2.standard)
  def offNotSelected: HorizontalRadio =
    notSelectedColors(S.color.bg.layerThree, S.color.fg.default, S.color.bg.layerTwo)

  def primary: HorizontalRadio = primarySelected.offNotSelected
  def positive: HorizontalRadio = positiveSelected.offNotSelected
  def negative: HorizontalRadio = negativeSelected.offNotSelected
  def alert: HorizontalRadio = alertSelected.offNotSelected
  def informational: HorizontalRadio = informationalSelected.offNotSelected
  def brandPrimary1: HorizontalRadio = brandPrimary1Selected.offNotSelected
  def brandPrimary2: HorizontalRadio = brandPrimary2Selected.offNotSelected
  def positiveNegative: HorizontalRadio = positiveSelected.negativeNotSelected

  def buttonExtra(mods: Widget*): HorizontalRadio =
    copy(
      selectedButtonExtra = fragment(selectedButtonExtra, Widget.fragment(mods)),
      notSelectedButtonExtra = fragment(notSelectedButtonExtra, Widget.fragment(mods)),
    )
  def extra(mods: Widget*): HorizontalRadio =
    copy(extra = fragment(this.extra, Widget.fragment(mods)))

  /** Type-parameterized builder entry (`apply` is reserved for children on Deferred widgets). */
  def of[A]: HorizontalRadio.Builder1[A] =
    new HorizontalRadio.Builder1[A](this)

}
object HorizontalRadio {

  val empty: HorizontalRadio =
    HorizontalRadio(
      selectedFG = S.color.fg.inverse,
      selectedBG = S.color.status.positive.standard,
      selectedHover = S.color.status.positive.hover,
      notSelectedFG = S.color.fg.default,
      notSelectedBG = S.color.bg.layerThree,
      notSelectedHover = S.color.bg.layerTwo,
      padding = StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._4),
      externalBorderSize = 2.px,
      internalBorderSize = 2.px,
      borderRadius = S.borderRadius._4,
      borderColor = S.color.fg.inverse,
      fontSize = S.fontSize._4,
      extra = Widget.empty,
      selectedButtonExtra = Widget.empty,
      notSelectedButtonExtra = Widget.empty,
    ).positive.medium

  def apply(): HorizontalRadio = empty

  def of[S]: Builder1[S] = empty.of[S]

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

  class Builder1[S](cfg: HorizontalRadio) extends Builder2[S](cfg, _.toString) {
    final def show(f: S => String): Builder2[S] = new Builder2(cfg, f)
    final def usingShow(using ev: Show[S]): Builder2[S] = show(ev.show)
    final def toStringShow: Builder2[S] = this
  }

  class Builder2[S](cfg: HorizontalRadio, showF: S => String)
      extends Builder3[Nothing, S](cfg, showF, (_, _) => ZIO.unit) {
    final def onSelectRaise: Builder3[S, S] =
      Builder3[S, S](cfg, showF, _.raiseAction(_))
  }

  case class Builder3[A, S](
      private val cfg: HorizontalRadio,
      private val showF: S => String,
      private val onSelectF: (RaiseHandler[Any, A], S) => ZIO[Scope, UIError, Unit],
  ) extends PWidget.Deferred[Any, A, State[S], State[S]] {

    final def configure(f: HorizontalRadio => HorizontalRadio): Builder3[A, S] =
      Builder3(f(cfg), showF, onSelectF)

    override protected def build: PWidget[Any, A, State[S], State[S]] =
      Widget.state[HorizontalRadio.State[S]].fix { state =>
        val current: S = state.renderTimeValue.selected
        span(
          O.HorizontalRadio,
          borderStyle.solid,
          borderColor := cfg.borderColor,
          borderWidth := cfg.externalBorderSize,
          borderRadius := cfg.borderRadius,
        )(
          Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
            val isSelected: Boolean = opt == current
            span(
              O.HorizontalRadio.Button,
              borderLeft := "none",
              borderTop := "none",
              borderBottom := "none",
              padding := cfg.padding.show,
              fontSize := cfg.fontSize,
              if isSelected then
                fragment(
                  color := cfg.selectedFG,
                  backgroundColor.dynamic := cfg.selectedBG,
                  backgroundColor.dynamic.hover := cfg.selectedHover,
                )
              else
                fragment(
                  color := cfg.notSelectedFG,
                  backgroundColor.dynamic := cfg.notSelectedBG,
                  backgroundColor.dynamic.hover := cfg.notSelectedHover,
                )
              ,
              if isFirst then
                fragment(
                  borderTopLeftRadius := cfg.borderRadius,
                  borderBottomLeftRadius := cfg.borderRadius,
                )
              else Widget.empty,
              if isLast then
                fragment(
                  borderTopRightRadius := cfg.borderRadius,
                  borderBottomRightRadius := cfg.borderRadius,
                )
              else
                fragment(
                  borderRight.csss(cfg.internalBorderSize, "solid", cfg.borderColor),
                )
              ,
              showF(opt),
              onClick.a[A].handle { rh => onSelectF(rh, opt) *> state.update(_.copy(selected = opt)) },
              if isSelected then cfg.selectedButtonExtra else cfg.notSelectedButtonExtra,
            )
          },
          cfg.extra,
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled horizontal-radio form builder. Value is the selected option `A`.
    *
    * {{{
    * HorizontalRadio.form[Mode]("Mode").describe("Pick").modRadio(_.primarySelected)
    * }}}
    */
  final case class form[A] private (
      private val _fieldName: String,
      private val _radio: HorizontalRadio,
      private val _label: Label,
      private val _show: A => String,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred.Stateful[Any, Nothing, HorizontalRadio.State[A], A] {

    override protected lazy val build: PForm[Any, Nothing, HorizontalRadio.State[A], HorizontalRadio.State[A], A] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          width.fitContent,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          div(_radio.of[A].show(_show)),
        ),
      )(_.selected)

    def modRadio(f: HorizontalRadio => HorizontalRadio): form[A] = copy(_radio = f(_radio))
    def modLabel(f: Label => Label): form[A] = copy(_label = f(_label))
    def radio: HorizontalRadio = _radio
    def label: Label = _label

    def show(f: A => String): form[A] = copy(_show = f)
    def describe(d: Widget): form[A] = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form[A] = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form[A] = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form[A] = copy(_labelSpacing = s)
    def noLabelSpacing: form[A] = labelSpacing(None)

    def small: form[A] = modRadio(_.small)
    def medium: form[A] = modRadio(_.medium)
    def large: form[A] = modRadio(_.large)
    def primarySelected: form[A] = modRadio(_.primarySelected)
    def positiveSelected: form[A] = modRadio(_.positiveSelected)
    def negativeSelected: form[A] = modRadio(_.negativeSelected)

  }
  object form {

    def apply[A](label: String, show: A => String): HorizontalRadio.form[A] =
      new HorizontalRadio.form[A](
        _fieldName = label,
        _radio = HorizontalRadio.empty,
        _label = Label(label),
        _show = show,
        _surroundingPadding = 10.px,
        _labelSpacing = Label.defaultInputSpacing.some,
      )

    def apply[A](label: String): HorizontalRadio.form[A] =
      apply[A](label, _.toString)

  }

}
