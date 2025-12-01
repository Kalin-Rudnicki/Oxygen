package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.{RaiseHandler, UIError}
import oxygen.ui.web.create.{*, given}
import zio.*

object Dropdown extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      _width: String,
      _optionsMaxHeight: String,
      _displayFGColor: String,
      _displayBGColor: String,
      _selectedFGColor: String,
      _selectedBGColor: CSSColor,
      _notSelectedFGColor: String,
      _notSelectedBGColor: CSSColor,
      _hoverTransform: ColorTransform,
      _displayPadding: StandardProps.Padding,
      _optionPadding: StandardProps.Padding,
      _externalBorderSize: String,
      _internalBorderSize: String,
      _displayBorderRadius: String,
      _optionsBorderRadius: String,
      _externalBorderColor: String,
      _internalBorderColor: String,
      _fontSize: String,
      _mod: NodeModifier,
      _closeOnMouseLeave: Boolean,
      _displayMod: NodeModifier,
      _selectedOptionMod: NodeModifier,
      _notSelectedOptionMod: NodeModifier,
      _displayNone: String,
      _showSetNone: Specified[String],
  ) {

    lazy val hoverColor: CSSColor = _hoverTransform.transform(_selectedBGColor)

  }
  object Props extends PropsCompanion {

    protected lazy val initialProps: Props =
      Props(
        _width = 25.ch,
        _optionsMaxHeight = 100.px,
        _displayFGColor = "black",
        _displayBGColor = "white",
        _selectedFGColor = "transparent",
        _selectedBGColor = CSSColor.transparent,
        _notSelectedFGColor = "transparent",
        _notSelectedBGColor = CSSColor.transparent,
        _hoverTransform = ColorTransform.none,
        _displayPadding = StandardProps.Padding.none,
        _optionPadding = StandardProps.Padding.none,
        _externalBorderSize = "0",
        _internalBorderSize = 1.px,
        _displayBorderRadius = "0",
        _optionsBorderRadius = "0",
        _externalBorderColor = S.color.fg.moderate,
        _internalBorderColor = S.color.fg.moderate,
        _fontSize = "1pt",
        _closeOnMouseLeave = false,
        _mod = NodeModifier.empty,
        _displayMod = NodeModifier.empty,
        _selectedOptionMod = NodeModifier.empty,
        _notSelectedOptionMod = NodeModifier.empty,
        _displayNone = "",
        _showSetNone = ___,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    /////// Size ///////////////////////////////////////////////////////////////

    private def makeSize(name: String, padding: StandardProps.Padding, displayBorderRadius: String, optionsBorderRadius: String, fontSize: String, width: String, optionsMaxHeight: String): Decorator =
      make(name) {
        _.copy(
          _displayPadding = padding,
          _optionPadding = padding,
          _displayBorderRadius = displayBorderRadius,
          _optionsBorderRadius = optionsBorderRadius,
          _fontSize = fontSize,
          _width = width,
          _optionsMaxHeight = optionsMaxHeight,
        )
      }

    final lazy val small: Decorator = makeSize("Small", StandardProps.Padding(S.spacing._2px, S.spacing._3), S.borderRadius._3, S.borderRadius._2, S.fontSize._2, 20.ch, 150.px)
    final lazy val medium: Decorator = makeSize("Medium", StandardProps.Padding(S.spacing._1, S.spacing._4), S.borderRadius._4, S.borderRadius._2, S.fontSize._3, 30.ch, 250.px)
    final lazy val large: Decorator = makeSize("Large", StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._5), S.borderRadius._5, S.borderRadius._3, S.fontSize._4, 50.ch, 400.px)

    /////// Style ///////////////////////////////////////////////////////////////

    private def makeSelected(name: String, bgColor: String, fgColor: String, transform: ColorTransform): Decorator =
      make(s"Selected($name)") { _.copy(_selectedBGColor = CSSColor.eval(bgColor), _selectedFGColor = fgColor, _hoverTransform = transform) }
    private def makeNotSelected(name: String, bgColor: String, fgColor: String): Decorator =
      make(s"NotSelected($name)") { _.copy(_notSelectedBGColor = CSSColor.eval(bgColor), _notSelectedFGColor = fgColor) }

    final lazy val primarySelected: Decorator = makeSelected("Primary", S.color.primary, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val positiveSelected: Decorator = makeSelected("Positive", S.color.status.positive, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val negativeSelected: Decorator = makeSelected("Negative", S.color.status.negative, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val alertSelected: Decorator = makeSelected("Alert", S.color.status.alert, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val informationalSelected: Decorator = makeSelected("Informational", S.color.status.informational, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val brandPrimary1Selected: Decorator = makeSelected("BrandPrimary1", S.color.brand.primary1, S.color.fg.default, ColorTransform.lighten(30.0))
    final lazy val brandPrimary2Selected: Decorator = makeSelected("BrandPrimary2", S.color.brand.primary2, S.color.fg.default, ColorTransform.lighten(30.0))
    final lazy val offSelected: Decorator = makeSelected("Off", "#DDD", S.color.fg.globalBlack, ColorTransform.lighten(30.0))

    final lazy val primaryNotSelected: Decorator = makeNotSelected("Primary", S.color.primary, S.color.fg.inverse)
    final lazy val positiveNotSelected: Decorator = makeNotSelected("Positive", S.color.status.positive, S.color.fg.inverse)
    final lazy val negativeNotSelected: Decorator = makeNotSelected("Negative", S.color.status.negative, S.color.fg.inverse)
    final lazy val alertNotSelected: Decorator = makeNotSelected("Alert", S.color.status.alert, S.color.fg.inverse)
    final lazy val informationalNotSelected: Decorator = makeNotSelected("Informational", S.color.status.informational, S.color.fg.inverse)
    final lazy val brandPrimary1NotSelected: Decorator = makeNotSelected("BrandPrimary1", S.color.brand.primary1, S.color.fg.inverse)
    final lazy val brandPrimary2NotSelected: Decorator = makeNotSelected("BrandPrimary2", S.color.brand.primary2, S.color.fg.inverse)
    final lazy val offNotSelected: Decorator = makeNotSelected("Off", "#DDD", S.color.fg.globalBlack)

    final lazy val primary: Decorator = primarySelected.offNotSelected
    final lazy val positive: Decorator = positiveSelected.offNotSelected
    final lazy val negative: Decorator = negativeSelected.offNotSelected
    final lazy val alert: Decorator = alertSelected.offNotSelected
    final lazy val informational: Decorator = informationalSelected.offNotSelected
    final lazy val brandPrimary1: Decorator = brandPrimary1Selected.offNotSelected
    final lazy val brandPrimary2: Decorator = brandPrimary2Selected.offNotSelected
    final lazy val positiveNegative: Decorator = positiveSelected.negativeNotSelected

    final def displayFGColor(color: String): Decorator = make("custom(displayFGColor)") { _.copy(_displayFGColor = color) }
    final def displayBGColor(color: String): Decorator = make("custom(displayBGColor)") { _.copy(_displayBGColor = color) }
    final def selectedFGColor(color: String): Decorator = make("custom(selectedFGColor)") { _.copy(_selectedFGColor = color) }
    final def selectedBGColor(color: String): Decorator = make("custom(selectedBGColor)") { _.copy(_selectedBGColor = CSSColor.eval(color)) }
    final def notSelectedFGColor(color: String): Decorator = make("custom(notSelectedFGColor)") { _.copy(_notSelectedFGColor = color) }
    final def notSelectedBGColor(color: String): Decorator = make("custom(notSelectedBGColor)") { _.copy(_notSelectedBGColor = CSSColor.eval(color)) }
    final def hoverColorTransform(transform: ColorTransform): Decorator = make("custom(hoverColorTransform)") { _.copy(_hoverTransform = transform) }

    /////// Misc ///////////////////////////////////////////////////////////////

    final def setCustomMod(mod: NodeModifier): Decorator = make("custom(setMod)") { _.copy(_mod = mod) }
    final def addCustomMod(mod: NodeModifier): Decorator = make("custom(addMod)") { p => p.copy(_mod = p._mod <> mod) }
    final def prepend(before: Widget*): Decorator = addCustomMod(NodeModifier.before(before*))
    final def append(after: Widget*): Decorator = addCustomMod(NodeModifier.after(after*))
    final def surround(before: Widget*)(after: Widget*): Decorator = addCustomMod(NodeModifier.surround(before*)(after*))

    final def setCustomDisplayMod(mod: NodeModifier): Decorator = make("custom(setDisplayMod)") { _.copy(_displayMod = mod) }
    final def addCustomDisplayMod(mod: NodeModifier): Decorator = make("custom(addDisplayMod)") { p => p.copy(_displayMod = p._displayMod <> mod) }
    final def prependDisplay(before: Widget*): Decorator = addCustomDisplayMod(NodeModifier.before(before*))
    final def appendDisplay(after: Widget*): Decorator = addCustomDisplayMod(NodeModifier.after(after*))
    final def surroundDisplay(before: Widget*)(after: Widget*): Decorator = addCustomDisplayMod(NodeModifier.surround(before*)(after*))

    final def setCustomOptionMod(mod: NodeModifier): Decorator =
      make("custom(setOptionMod)") { _.copy(_selectedOptionMod = mod, _notSelectedOptionMod = mod) }
    final def addCustomOptionMod(mod: NodeModifier): Decorator =
      make("custom(addOptionMod)") { p => p.copy(_selectedOptionMod = p._selectedOptionMod <> mod, _notSelectedOptionMod = p._notSelectedOptionMod <> mod) }
    final def prependOption(before: Widget*): Decorator = addCustomOptionMod(NodeModifier.before(before*))
    final def appendOption(after: Widget*): Decorator = addCustomOptionMod(NodeModifier.after(after*))
    final def surroundOption(before: Widget*)(after: Widget*): Decorator = addCustomOptionMod(NodeModifier.surround(before*)(after*))

    final def setCustomSelectedOptionMod(mod: NodeModifier): Decorator = make("custom(setSelectedOptionMod)") { _.copy(_selectedOptionMod = mod) }
    final def addCustomSelectedOptionMod(mod: NodeModifier): Decorator = make("custom(addSelectedOptionMod)") { p => p.copy(_selectedOptionMod = p._selectedOptionMod <> mod) }
    final def prependSelectedOption(before: Widget*): Decorator = addCustomSelectedOptionMod(NodeModifier.before(before*))
    final def appendSelectedOption(after: Widget*): Decorator = addCustomSelectedOptionMod(NodeModifier.after(after*))
    final def surroundSelectedOption(before: Widget*)(after: Widget*): Decorator = addCustomSelectedOptionMod(NodeModifier.surround(before*)(after*))

    final def setCustomNotSelectedOptionMod(mod: NodeModifier): Decorator = make("custom(setNotSelectedOptionMod)") { _.copy(_notSelectedOptionMod = mod) }
    final def addCustomNotSelectedOptionMod(mod: NodeModifier): Decorator = make("custom(addNotSelectedOptionMod)") { p => p.copy(_notSelectedOptionMod = p._notSelectedOptionMod <> mod) }
    final def prependNotSelectedOption(before: Widget*): Decorator = addCustomNotSelectedOptionMod(NodeModifier.before(before*))
    final def appendNotSelectedOption(after: Widget*): Decorator = addCustomNotSelectedOptionMod(NodeModifier.after(after*))
    final def surroundNotSelectedOption(before: Widget*)(after: Widget*): Decorator = addCustomNotSelectedOptionMod(NodeModifier.surround(before*)(after*))

    final def externalBorder(width: String, color: String): Decorator = make("custom(externalBorder)") { _.copy(_externalBorderSize = width, _externalBorderColor = color) }
    final def internalBorder(width: String, color: String): Decorator = make("custom(internalBorder)") { _.copy(_internalBorderSize = width, _internalBorderColor = color) }

    final def displayNone(value: String): Decorator = make("CustomDisplayNone") { _.copy(_displayNone = value) }
    final def setNone(value: String): Decorator = make("CustomSetNone") { _.copy(_showSetNone = value) }
    final lazy val noSetNone: Decorator = make("NoSetNone") { _.copy(_showSetNone = ___) }

    final lazy val closeOnMouseLeave: Decorator = make("CloseOnMouseLeave") { _.copy(_closeOnMouseLeave = true) }

    final def maxDropdownHeight(height: String): Decorator = make("custom(maxDropdownHeight") { _.copy(_optionsMaxHeight = height) }

    final def width(width: String): Decorator = make("custom(width") { _.copy(_width = width) }
    final def fontSize(size: String): Decorator = make("custom(frontSize") { _.copy(_fontSize = size) }

  }

  final class Decorator private[Dropdown] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.primary.medium

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

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

    def initialNone[S: StrictEnum as e]: State[S] =
      State(e.enumValues, None, false)

    def initialNone[S](options: Seq[S]): State[S] =
      State(options, None, false)

    def initialFirst[S: StrictEnum as e]: State[S] =
      State(e.enumValues, e.enumValues.head.some, false)

    def initialFirst[S](options: Seq[S]): State[S] =
      State(options, options.headOption, false)

    def initial[S: StrictEnum as e](initial: S): State[S] =
      State(e.enumValues, initial.some, false)

    def initial[S](options: Seq[S], initial: S): State[S] =
      State(options, initial.some, false)

    def empty[S]: State[S] = State(Nil, None, false)

  }

  def apply[S]: Builder1[S] = new Builder1[S]

  class Builder1[S]
      extends Builder2[S](
        _.toString,
      ) {

    final def show(f: S => String): Builder2[S] = new Builder2(f)
    final def usingShow(using ev: Show[S]): Builder2[S] = show(ev.show)
    final def toStringShow: Builder2[S] = this

  }

  class Builder2[S](showF: S => String)
      extends Builder3[Nothing, S](
        showF,
        (_, _) => ZIO.unit,
      ) {

    final def onSelectRaise: Builder3[Option[S], S] =
      new Builder3[Option[S], S](showF, _.raiseAction(_))

    final def onSelectSomeRaise: Builder3[S, S] =
      new Builder3[S, S](showF, (rh, s) => ZIO.foreachDiscard(s)(rh.raiseAction))

  }

  class Builder3[A, S](
      showF: S => String,
      onSelectF: (RaiseHandler[Any, A], Option[S]) => ZIO[Scope, UIError, Unit],
  ) {

    // TODO (KR) : there could be some improvements to the way the borders are done here...
    final def decorate(decorator: Decorator): WidgetAS[A, State[S]] = {
      val props: Props = decorator.computed

      Widget.state[Dropdown.State[S]].fix { state =>
        val current: Option[S] = state.renderTimeValue.selected
        val isExpanded: Boolean = state.renderTimeValue.expanded

        def makeOption(value: Option[S], text: String, isFirst: Boolean, isLast: Boolean): WidgetA[A] = {
          val isSelected: Boolean = current == value

          div(
            O.Dropdown.Options.Option.optMods(_.Selected -> isSelected, _.First -> isFirst, _.Last -> isLast),
            onClick.a[A].handle { rh => onSelectF(rh, value) *> state.update(_.copy(selected = value, expanded = false)) },
            backgroundColor.dynamic.hover := props.hoverColor,
            padding := props._optionPadding.show,
            fontSize := props._fontSize,
            borderTop(props._internalBorderSize, props._internalBorderColor),
            if isFirst then // is first
              fragment(
              )
            else // is not first
              fragment(
              )
            ,
            if isLast then // is last
              fragment(
                // borderBottom(props._externalBorderSize, props._externalBorderColor),
                // borderBottomLeftRadius := props._optionsBorderRadius,
                // borderBottomRightRadius := props._optionsBorderRadius,
              )
            else // is not last
              fragment(
              )
            ,
            if isSelected then // is selected
              fragment(
                color := props._selectedFGColor,
                backgroundColor.dynamic := props._selectedBGColor,
              )
            else // is not selected
              fragment(
                color := props._notSelectedFGColor,
                backgroundColor.dynamic := props._notSelectedBGColor,
              )
            ,
            text,
          )(if isSelected then props._selectedOptionMod else props._notSelectedOptionMod)
        }

        val display: Widget =
          div(
            O.Dropdown.Display,
            onClick := state.update { s => s.copy(expanded = !s.expanded) },
            padding := props._displayPadding.show,
            fontSize := props._fontSize,
            color := props._displayFGColor,
            backgroundColor := props._displayBGColor,
            borderTop(props._externalBorderSize, props._externalBorderColor),
            borderLeft(props._externalBorderSize, props._externalBorderColor),
            borderRight(props._externalBorderSize, props._externalBorderColor),
            borderTopLeftRadius := props._displayBorderRadius,
            borderTopRightRadius := props._displayBorderRadius,
            Widget.when(!isExpanded)(
              fragment(
                borderBottomLeftRadius := props._displayBorderRadius,
                borderBottomRightRadius := props._displayBorderRadius,
              ),
            ),
            Widget.when(!isExpanded || (props._showSetNone.isNotSpecified && state.renderTimeValue.elems.isEmpty))(
              fragment(
                borderBottom(props._externalBorderSize, props._externalBorderColor),
              ),
            ),
            current.fold(props._displayNone)(showF) match {
              case ""  => util.nonBreakingSpace
              case str => str
            },
          )(props._displayMod)

        val options: WidgetA[A] =
          div(
            O.Dropdown.Options,
            O.Scrollable,
            O.Scrollable.scrollbarColor := props._notSelectedBGColor,
            O.Scrollable.scrollbarBottomRightRadius := props._optionsBorderRadius,
            overflowX.hidden,
            maxHeight := props._optionsMaxHeight,
            borderLeft(props._externalBorderSize, props._externalBorderColor),
            borderRight(props._externalBorderSize, props._externalBorderColor),
            borderBottom(props._externalBorderSize, props._externalBorderColor),
            borderBottomLeftRadius := props._optionsBorderRadius,
            borderBottomRightRadius := props._optionsBorderRadius,
            Widget.foreach(props._showSetNone.toOption) { str =>
              makeOption(
                None,
                str,
                true,
                state.renderTimeValue.options.isEmpty,
              )
            },
            Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
              makeOption(
                opt.some,
                showF(opt),
                isFirst && !props._showSetNone.isSpecified,
                isLast,
              )
            },
          )

        div(
          O.Dropdown.optMods(_.Expanded -> isExpanded),
          Widget.when(props._closeOnMouseLeave) { onMouseLeave := state.update(_.copy(expanded = false)).whenDiscard(isExpanded) },
          width := props._width,
          display,
          options,
        )(props._mod)
      }
    }

    final def decorate(decorator: Decorator => Decorator): WidgetAS[A, State[S]] =
      decorate(decorator(Decorator.defaultStyling))

    final def default: WidgetAS[A, State[S]] =
      decorate(Decorator.defaultStyling)

    final def apply(decorator: Decorator): WidgetAS[A, State[S]] =
      decorate(decorator)

    final def apply(decorator: Decorator => Decorator): WidgetAS[A, State[S]] =
      decorate(decorator)

    final def apply(): WidgetAS[A, State[S]] =
      default

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object form extends Decorable {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Props
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Props(
        label: Label.Decorator,
        dropdown: Dropdown.Decorator,
        labelSpacing: Option[String],
        surroundingPadding: String,
        width: String,
    )
    object Props extends PropsCompanion {

      override protected lazy val initialProps: Props =
        Props(
          label = Label.Decorator.defaultStyling,
          dropdown = Dropdown.Decorator.defaultStyling,
          labelSpacing = Label.defaultInputSpacing.some,
          surroundingPadding = 10.px,
          width = "fit-content",
        )

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Decorator
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    trait DecoratorBuilder extends DecoratorBuilder0 {

      final def label(f: Label.Decorator => Label.Decorator): Decorator = {
        val dec = f(Label.Decorator.empty)
        make(s"label { ${dec.name} }") { current => current.copy(label = current.label >> dec) }
      }

      final def dropdown(f: Dropdown.Decorator => Dropdown.Decorator): Decorator = {
        val dec = f(Dropdown.Decorator.empty)
        make(s"dropdown { ${dec.name} }") { current => current.copy(dropdown = current.dropdown >> dec) }
      }

      final def describe(description: Widget): Decorator =
        make("describe") { current => current.copy(label = current.label.describe(description)) }

      final def width(width: String): Decorator =
        make(s"custom(width = $width)") { current => current.copy(width = width, dropdown = current.dropdown.width(100.pct)) }

    }

    final class Decorator private[form] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
    object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

      override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

      override lazy val defaultStyling: Decorator = empty

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Widget
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    def apply[A](
        label: String,
        show: A => String,
        decorator: Decorator => Decorator,
    ): FormS[Dropdown.State[A], Option[A]] = {
      val props = decorator(Decorator.empty.label(_.label(label))).computed

      Form.makeWith(
        label,
        div(
          padding := props.surroundingPadding,
          width.fitContent,
          Label(props.label),
          Spacing.vertical.opt(props.labelSpacing),
          div(Dropdown[A].show(show).decorate(props.dropdown)),
        ),
      )(_.selected)
    }

    def apply[A](
        label: String,
        decorator: Decorator => Decorator,
    ): FormS[Dropdown.State[A], Option[A]] =
      apply[A](label, _.toString, decorator)

    def apply[A](
        label: String,
    ): FormS[Dropdown.State[A], Option[A]] =
      apply[A](label, _.toString, identity)

  }

}
