package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.{RaiseHandler, UIError, *}
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * Dropdown select (W2). Pure CSS vars; no Decorator / ColorTransform.
  */
final case class Dropdown(
    private val width: String,
    private val optionsMaxHeight: String,
    private val displayFG: String,
    private val displayBG: String,
    private val selectedFG: String,
    private val selectedBG: String,
    private val selectedHover: String,
    private val notSelectedFG: String,
    private val notSelectedBG: String,
    private val displayPadding: StandardProps.Padding,
    private val optionPadding: StandardProps.Padding,
    private val externalBorderSize: String,
    private val internalBorderSize: String,
    private val displayBorderRadius: String,
    private val optionsBorderRadius: String,
    private val externalBorderColor: String,
    private val internalBorderColor: String,
    private val fontSize: String,
    private val _closeOnMouseLeave: Boolean,
    private val displayNone: String,
    private val showSetNone: Option[String],
    private val extra: Widget,
) {

  def small: Dropdown =
    copy(
      displayPadding = StandardProps.Padding(S.spacing._2px, S.spacing._3),
      optionPadding = StandardProps.Padding(S.spacing._2px, S.spacing._3),
      displayBorderRadius = S.borderRadius._3,
      optionsBorderRadius = S.borderRadius._2,
      fontSize = S.fontSize._2,
      width = 20.ch,
      optionsMaxHeight = 150.px,
    )
  def medium: Dropdown =
    copy(
      displayPadding = StandardProps.Padding(S.spacing._1, S.spacing._4),
      optionPadding = StandardProps.Padding(S.spacing._1, S.spacing._4),
      displayBorderRadius = S.borderRadius._4,
      optionsBorderRadius = S.borderRadius._2,
      fontSize = S.fontSize._3,
      width = 30.ch,
      optionsMaxHeight = 250.px,
    )
  def large: Dropdown =
    copy(
      displayPadding = StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._5),
      optionPadding = StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._5),
      displayBorderRadius = S.borderRadius._5,
      optionsBorderRadius = S.borderRadius._3,
      fontSize = S.fontSize._4,
      width = 50.ch,
      optionsMaxHeight = 400.px,
    )

  def selectedColors(bg: String, fg: String, hover: String): Dropdown =
    copy(selectedBG = bg, selectedFG = fg, selectedHover = hover)
  def notSelectedColors(bg: String, fg: String): Dropdown =
    copy(notSelectedBG = bg, notSelectedFG = fg)

  def primarySelected: Dropdown = selectedColors(S.color.primary.standard, S.color.primary.on, S.color.primary.hover)
  def positiveSelected: Dropdown = selectedColors(S.color.status.positive.standard, S.color.status.positive.on, S.color.status.positive.hover)
  def negativeSelected: Dropdown = selectedColors(S.color.status.negative.standard, S.color.status.negative.on, S.color.status.negative.hover)
  def alertSelected: Dropdown = selectedColors(S.color.status.alert.standard, S.color.status.alert.on, S.color.status.alert.hover)
  def informationalSelected: Dropdown = selectedColors(S.color.status.informational.standard, S.color.status.informational.on, S.color.status.informational.hover)
  def brandPrimary1Selected: Dropdown = selectedColors(S.color.brand.primary1.standard, S.color.fg.default, S.color.brand.primary1.light)
  def brandPrimary2Selected: Dropdown = selectedColors(S.color.brand.primary2.standard, S.color.fg.default, S.color.brand.primary2.light)
  def offSelected: Dropdown = selectedColors(S.color.bg.layerTwo, S.color.fg.default, S.color.bg.layerOne)

  def primaryNotSelected: Dropdown = notSelectedColors(S.color.primary.subtle, S.color.fg.inverse)
  def positiveNotSelected: Dropdown = notSelectedColors(S.color.status.positive.subtle, S.color.fg.inverse)
  def negativeNotSelected: Dropdown = notSelectedColors(S.color.status.negative.subtle, S.color.fg.inverse)
  def alertNotSelected: Dropdown = notSelectedColors(S.color.status.alert.subtle, S.color.fg.inverse)
  def informationalNotSelected: Dropdown = notSelectedColors(S.color.status.informational.subtle, S.color.fg.inverse)
  def brandPrimary1NotSelected: Dropdown = notSelectedColors(S.color.brand.primary1.light, S.color.fg.inverse)
  def brandPrimary2NotSelected: Dropdown = notSelectedColors(S.color.brand.primary2.light, S.color.fg.inverse)
  def offNotSelected: Dropdown = notSelectedColors(S.color.bg.layerThree, S.color.fg.default)

  def primary: Dropdown = primarySelected.offNotSelected
  def positive: Dropdown = positiveSelected.offNotSelected
  def negative: Dropdown = negativeSelected.offNotSelected
  def alert: Dropdown = alertSelected.offNotSelected
  def informational: Dropdown = informationalSelected.offNotSelected
  def brandPrimary1: Dropdown = brandPrimary1Selected.offNotSelected
  def brandPrimary2: Dropdown = brandPrimary2Selected.offNotSelected
  def positiveNegative: Dropdown = positiveSelected.negativeNotSelected

  def width(w: String): Dropdown = copy(width = w)
  def maxDropdownHeight(h: String): Dropdown = copy(optionsMaxHeight = h)
  def externalBorder(w: String, c: String): Dropdown = copy(externalBorderSize = w, externalBorderColor = c)
  def internalBorder(w: String, c: String): Dropdown = copy(internalBorderSize = w, internalBorderColor = c)
  def setNone(value: String): Dropdown = copy(showSetNone = value.some)
  def noSetNone: Dropdown = copy(showSetNone = None)
  def closeOnMouseLeave: Dropdown = copy(_closeOnMouseLeave = true)
  def displayNone(value: String): Dropdown = copy(displayNone = value)
  def extra(mods: Widget*): Dropdown = copy(extra = fragment(this.extra, Widget.fragment(mods)))

  /** Type-parameterized builder entry (`apply` is reserved for children on Deferred widgets). */
  def of[A]: Dropdown.Builder1[A] = new Dropdown.Builder1[A](this)

}
object Dropdown {

  val empty: Dropdown =
    Dropdown(
      width = 30.ch,
      optionsMaxHeight = 250.px,
      displayFG = S.color.fg.default,
      displayBG = S.color.bg.layerOne,
      selectedFG = S.color.fg.inverse,
      selectedBG = S.color.primary.standard,
      selectedHover = S.color.primary.hover,
      notSelectedFG = S.color.fg.default,
      notSelectedBG = S.color.bg.layerTwo,
      displayPadding = StandardProps.Padding(S.spacing._1, S.spacing._4),
      optionPadding = StandardProps.Padding(S.spacing._1, S.spacing._4),
      externalBorderSize = 1.px,
      internalBorderSize = 1.px,
      displayBorderRadius = S.borderRadius._4,
      optionsBorderRadius = S.borderRadius._2,
      externalBorderColor = S.color.fg.moderate,
      internalBorderColor = S.color.fg.moderate,
      fontSize = S.fontSize._3,
      _closeOnMouseLeave = false,
      displayNone = "",
      showSetNone = None,
      extra = Widget.empty,
    ).primary.medium

  def apply(): Dropdown = empty
  def of[S]: Builder1[S] = empty.of[S]

  final case class State[S](
      options: Seq[S],
      selected: Option[S],
      expanded: Boolean,
  ) {
    private val lastIdx: Int = options.size - 1
    private[Dropdown] val elems: Seq[(Boolean, S, Boolean)] =
      options.zipWithIndex.map { case (value, idx) =>
        (idx == 0, value, idx == lastIdx)
      }
  }
  object State {
    def initialNone[S: StrictEnum as e]: State[S] = State(e.enumValues, None, false)
    def initialNone[S](options: Seq[S]): State[S] = State(options, None, false)
    def initialFirst[S: StrictEnum as e]: State[S] = State(e.enumValues, e.enumValues.head.some, false)
    def initialFirst[S](options: Seq[S]): State[S] = State(options, options.headOption, false)
    def initial[S: StrictEnum as e](initial: S): State[S] = State(e.enumValues, initial.some, false)
    def initial[S](options: Seq[S], initial: S): State[S] = State(options, initial.some, false)
    def empty[S]: State[S] = State(Nil, None, false)
  }

  class Builder1[S](cfg: Dropdown) extends Builder2[S](cfg, _.toString) {
    final def show(f: S => String): Builder2[S] = new Builder2(cfg, f)
    final def usingShow(using ev: Show[S]): Builder2[S] = show(ev.show)
    final def toStringShow: Builder2[S] = this
  }

  class Builder2[S](cfg: Dropdown, showF: S => String)
      extends Builder3[Nothing, S](cfg, showF, (_, _) => ZIO.unit) {
    final def onSelectRaise: Builder3[Option[S], S] =
      Builder3[Option[S], S](cfg, showF, _.raiseAction(_))
    final def onSelectSomeRaise: Builder3[S, S] =
      Builder3[S, S](cfg, showF, (rh, s) => ZIO.foreachDiscard(s)(rh.raiseAction))
  }

  case class Builder3[A, S](
      private val cfg: Dropdown,
      private val showF: S => String,
      private val onSelectF: (RaiseHandler[Any, A], Option[S]) => ZIO[Scope, UIError, Unit],
  ) extends PWidget.Deferred[Any, A, State[S], State[S]] {

    final def configure(f: Dropdown => Dropdown): Builder3[A, S] =
      Builder3(f(cfg), showF, onSelectF)

    override protected def build: PWidget[Any, A, State[S], State[S]] =
      Widget.state[Dropdown.State[S]].fix { state =>
        val current: Option[S] = state.renderTimeValue.selected
        val isExpanded: Boolean = state.renderTimeValue.expanded

        def makeOption(value: Option[S], text: String, isFirst: Boolean, isLast: Boolean): WidgetA[A] = {
          val isSelected: Boolean = current == value
          div(
            O.Dropdown.Options.Option.optMods(_.Selected -> isSelected, _.First -> isFirst, _.Last -> isLast),
            onClick.a[A].handle { rh => onSelectF(rh, value) *> state.update(_.copy(selected = value, expanded = false)) },
            backgroundColor.dynamic.hover := cfg.selectedHover,
            padding := cfg.optionPadding.show,
            fontSize := cfg.fontSize,
            borderTop(cfg.internalBorderSize, cfg.internalBorderColor),
            if isSelected then
              fragment(
                color := cfg.selectedFG,
                backgroundColor.dynamic := cfg.selectedBG,
              )
            else
              fragment(
                color := cfg.notSelectedFG,
                backgroundColor.dynamic := cfg.notSelectedBG,
              )
            ,
            text,
          )
        }

        val display: Widget =
          div(
            O.Dropdown.Display,
            onClick := state.update { s => s.copy(expanded = !s.expanded) },
            padding := cfg.displayPadding.show,
            fontSize := cfg.fontSize,
            color := cfg.displayFG,
            backgroundColor := cfg.displayBG,
            borderTop(cfg.externalBorderSize, cfg.externalBorderColor),
            borderLeft(cfg.externalBorderSize, cfg.externalBorderColor),
            borderRight(cfg.externalBorderSize, cfg.externalBorderColor),
            borderTopLeftRadius := cfg.displayBorderRadius,
            borderTopRightRadius := cfg.displayBorderRadius,
            Widget.when(!isExpanded)(
              fragment(
                borderBottomLeftRadius := cfg.displayBorderRadius,
                borderBottomRightRadius := cfg.displayBorderRadius,
              ),
            ),
            Widget.when(!isExpanded || (cfg.showSetNone.isEmpty && state.renderTimeValue.elems.isEmpty))(
              fragment(
                borderBottom(cfg.externalBorderSize, cfg.externalBorderColor),
              ),
            ),
            current.fold(cfg.displayNone)(showF) match {
              case ""  => util.nonBreakingSpace
              case str => str
            },
          )

        val options: WidgetA[A] =
          div(
            O.Dropdown.Options,
            O.Scrollable,
            O.Scrollable.scrollbarColor := cfg.notSelectedBG,
            O.Scrollable.scrollbarBottomRightRadius := cfg.optionsBorderRadius,
            overflowX.hidden,
            maxHeight := cfg.optionsMaxHeight,
            borderLeft(cfg.externalBorderSize, cfg.externalBorderColor),
            borderRight(cfg.externalBorderSize, cfg.externalBorderColor),
            borderBottom(cfg.externalBorderSize, cfg.externalBorderColor),
            borderBottomLeftRadius := cfg.optionsBorderRadius,
            borderBottomRightRadius := cfg.optionsBorderRadius,
            Widget.foreach(cfg.showSetNone) { str =>
              makeOption(None, str, true, state.renderTimeValue.options.isEmpty)
            },
            Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
              makeOption(opt.some, showF(opt), isFirst && cfg.showSetNone.isEmpty, isLast)
            },
          )

        div(
          O.Dropdown.optMods(_.Expanded -> isExpanded),
          Widget.when(cfg._closeOnMouseLeave) { onMouseLeave := state.update(_.copy(expanded = false)).whenDiscard(isExpanded) },
          width := cfg.width,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          display,
          options,
          cfg.extra,
        )
      }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled dropdown form builder. Value is the selected option (`Option[A]`).
    *
    * {{{
    * Dropdown.form[Status]("Status").describe("Pick one").modDropdown(_.negative)
    * Dropdown.form[Status]("Status", _.label).width(24.ch)
    * }}}
    */
  final case class form[A] private (
      private val _fieldName: String,
      private val _dropdown: Dropdown,
      private val _label: Label,
      private val _show: A => String,
      private val _surroundingPadding: String,
      private val _width: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred.Stateful[Any, Nothing, Dropdown.State[A], Option[A]] {

    override protected lazy val build: PForm[Any, Nothing, Dropdown.State[A], Dropdown.State[A], Option[A]] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          oxygen.ui.web.create.width := _width,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          div(_dropdown.of[A].show(_show)),
        ),
      )(_.selected)

    def modDropdown(f: Dropdown => Dropdown): form[A] = copy(_dropdown = f(_dropdown))
    def modLabel(f: Label => Label): form[A] = copy(_label = f(_label))
    def dropdown: Dropdown = _dropdown
    def label: Label = _label

    def show(f: A => String): form[A] = copy(_show = f)
    def describe(d: Widget): form[A] = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form[A] = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form[A] = copy(_surroundingPadding = p)
    def width(w: String): form[A] = copy(_width = w, _dropdown = _dropdown.width(100.pct))
    def labelSpacing(s: Option[String]): form[A] = copy(_labelSpacing = s)
    def noLabelSpacing: form[A] = labelSpacing(None)

    def small: form[A] = modDropdown(_.small)
    def medium: form[A] = modDropdown(_.medium)
    def large: form[A] = modDropdown(_.large)

  }
  object form {

    def apply[A](label: String, show: A => String): Dropdown.form[A] =
      new Dropdown.form[A](
        _fieldName = label,
        _dropdown = Dropdown.empty,
        _label = Label(label),
        _show = show,
        _surroundingPadding = 10.px,
        _width = "fit-content",
        _labelSpacing = Label.defaultInputSpacing.some,
      )

    def apply[A](label: String): Dropdown.form[A] =
      apply[A](label, _.toString)

  }

}
