package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.{RaiseHandler, UIError}
import oxygen.ui.web.create.{*, given}
import zio.*

object HorizontalRadio extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      _selectedFGColor: String,
      _selectedBGColor: CSSColor,
      _notSelectedFGColor: String,
      _notSelectedBGColor: CSSColor,
      _selectedHoverTransform: ColorTransform,
      _notSelectedHoverTransform: ColorTransform,
      _padding: StandardProps.Padding,
      _externalBorderSize: String,
      _internalBorderSize: String,
      _borderRadius: String,
      _borderColor: String,
      _fontSize: String,
      _mod: NodeModifier,
      _selectedButtonMod: NodeModifier,
      _notSelectedButtonMod: NodeModifier,
  ) {

    lazy val selectedHoverColor: CSSColor = _selectedHoverTransform.transform(_selectedBGColor)
    lazy val notSelectedHoverColor: CSSColor = _notSelectedHoverTransform.transform(_notSelectedBGColor)

  }
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        _selectedFGColor = "transparent",
        _selectedBGColor = CSSColor.transparent,
        _notSelectedFGColor = "transparent",
        _notSelectedBGColor = CSSColor.transparent,
        _selectedHoverTransform = ColorTransform.none,
        _notSelectedHoverTransform = ColorTransform.none,
        _padding = StandardProps.Padding.none,
        _externalBorderSize = 2.px,
        _internalBorderSize = 2.px,
        _borderRadius = "0",
        _borderColor = S.color.fg.inverse,
        _fontSize = "1pt",
        _mod = NodeModifier.empty,
        _selectedButtonMod = NodeModifier.empty,
        _notSelectedButtonMod = NodeModifier.empty,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    /////// Size ///////////////////////////////////////////////////////////////

    private def makeSize(name: String, padding: StandardProps.Padding, borderRadius: String, fontSize: String): Decorator =
      make(name) { _.copy(_padding = padding, _borderRadius = borderRadius, _fontSize = fontSize) }

    final lazy val small: Decorator = makeSize("Small", StandardProps.Padding(S.spacing._1, S.spacing._3), S.borderRadius._3, S.fontSize._2)
    final lazy val medium: Decorator = makeSize("Medium", StandardProps.Padding(s"calc(${S.spacing._1} * 1.5)", S.spacing._4), S.borderRadius._4, S.fontSize._4)
    final lazy val large: Decorator = makeSize("Large", StandardProps.Padding(S.spacing._2, S.spacing._5), S.borderRadius._5, S.fontSize._6)

    /////// Colors ///////////////////////////////////////////////////////////////

    private def makeSelected(name: String, bgColor: String, fgColor: String, transform: ColorTransform): Decorator =
      make(s"Selected($name)") { _.copy(_selectedBGColor = CSSColor.eval(bgColor), _selectedFGColor = fgColor, _selectedHoverTransform = transform) }
    private def makeNotSelected(name: String, bgColor: String, fgColor: String, transform: ColorTransform): Decorator =
      make(s"NotSelected($name)") { _.copy(_notSelectedBGColor = CSSColor.eval(bgColor), _notSelectedFGColor = fgColor, _notSelectedHoverTransform = transform) }

    final lazy val primarySelected: Decorator = makeSelected("Primary", S.color.primary, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val positiveSelected: Decorator = makeSelected("Positive", S.color.status.positive, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val negativeSelected: Decorator = makeSelected("Negative", S.color.status.negative, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val alertSelected: Decorator = makeSelected("Alert", S.color.status.alert, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val informationalSelected: Decorator = makeSelected("Informational", S.color.status.informational, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val brandPrimary1Selected: Decorator = makeSelected("BrandPrimary1", S.color.brand.primary1, S.color.fg.default, ColorTransform.lighten(30.0))
    final lazy val brandPrimary2Selected: Decorator = makeSelected("BrandPrimary2", S.color.brand.primary2, S.color.fg.default, ColorTransform.lighten(30.0))
    final lazy val offSelected: Decorator =
      makeSelected(
        "Off",
        S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0),
        S.color.fg.default,
        ColorTransform.const(S.color.bg.base.getColorValue.lighten(40.0).setOpacity(60.0)),
      )

    final lazy val primaryNotSelected: Decorator = makeNotSelected("Primary", S.color.primary, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val positiveNotSelected: Decorator = makeNotSelected("Positive", S.color.status.positive, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val negativeNotSelected: Decorator = makeNotSelected("Negative", S.color.status.negative, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val alertNotSelected: Decorator = makeNotSelected("Alert", S.color.status.alert, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val informationalNotSelected: Decorator = makeNotSelected("Informational", S.color.status.informational, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val brandPrimary1NotSelected: Decorator = makeNotSelected("BrandPrimary1", S.color.brand.primary1, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val brandPrimary2NotSelected: Decorator = makeNotSelected("BrandPrimary2", S.color.brand.primary2, S.color.fg.inverse, ColorTransform.lighten(30.0))
    final lazy val offNotSelected: Decorator =
      makeNotSelected(
        "Off",
        S.color.bg.base.getColorValue.lighten(15.0).setOpacity(60.0),
        S.color.fg.default,
        ColorTransform.const(S.color.bg.base.getColorValue.lighten(40.0).setOpacity(60.0)),
      )

    final lazy val primary: Decorator = primarySelected.offNotSelected
    final lazy val positive: Decorator = positiveSelected.offNotSelected
    final lazy val negative: Decorator = negativeSelected.offNotSelected
    final lazy val alert: Decorator = alertSelected.offNotSelected
    final lazy val informational: Decorator = informationalSelected.offNotSelected
    final lazy val brandPrimary1: Decorator = brandPrimary1Selected.offNotSelected
    final lazy val brandPrimary2: Decorator = brandPrimary2Selected.offNotSelected
    final lazy val positiveNegative: Decorator = positiveSelected.negativeNotSelected

    final def selectedFGColor(color: String): Decorator = make("custom(selectedFGColor)") { _.copy(_selectedFGColor = color) }
    final def selectedBGColor(color: String): Decorator = make("custom(selectedBGColor)") { _.copy(_selectedBGColor = CSSColor.eval(color)) }
    final def notSelectedFGColor(color: String): Decorator = make("custom(notSelectedFGColor)") { _.copy(_notSelectedFGColor = color) }
    final def notSelectedBGColor(color: String): Decorator = make("custom(notSelectedBGColor)") { _.copy(_notSelectedBGColor = CSSColor.eval(color)) }
    final def selectedHoverColorTransform(transform: ColorTransform): Decorator = make("custom(selectedHoverColorTransform)") { _.copy(_selectedHoverTransform = transform) }
    final def notSelectedHoverColorTransform(transform: ColorTransform): Decorator = make("custom(notSelectedHoverColorTransform)") { _.copy(_notSelectedHoverTransform = transform) }

    /////// Misc ///////////////////////////////////////////////////////////////

    final lazy val mod: FocusNodeModifier = focusNodeModifier("mod")(_._mod)
    final lazy val buttonMod: FocusNodeModifier = focusNodeModifier("buttonMod")(_._selectedButtonMod, _._notSelectedButtonMod)
    final lazy val selectedButtonMod: FocusNodeModifier = focusNodeModifier("selectedButtonMod")(_._selectedButtonMod)
    final lazy val notSelectedButtonMod: FocusNodeModifier = focusNodeModifier("notSelectedButtonMod")(_._notSelectedButtonMod)

  }

  final class Decorator private[HorizontalRadio] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.positive.medium

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

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

    final def onSelectRaise: Builder3[S, S] =
      new Builder3[S, S](showF, _.raiseAction(_))

  }

  class Builder3[A, S](
      showF: S => String,
      onSelectF: (RaiseHandler[Any, A], S) => ZIO[Scope, UIError, Unit],
  ) {

    final def decorate(decorator: Decorator): WidgetAS[A, State[S]] = {
      val props: Props = decorator.computed

      Widget.state[HorizontalRadio.State[S]].fix { state =>
        val current: S = state.renderTimeValue.selected

        span(
          O.HorizontalRadio,
          borderStyle.solid,
          borderColor := props._borderColor,
          borderWidth := props._externalBorderSize,
          borderRadius := props._borderRadius,
        )(
          Widget.foreach(state.renderTimeValue.elems) { case (isFirst, opt, isLast) =>
            val isSelected: Boolean = opt == current
            span(
              O.HorizontalRadio.Button,
              // const
              borderLeft := "none",
              borderTop := "none",
              borderBottom := "none",
              padding := props._padding.show,
              fontSize := props._fontSize,
              if isSelected then // is selected
                fragment(
                  color := props._selectedFGColor,
                  backgroundColor.dynamic := props._selectedBGColor,
                  backgroundColor.dynamic.hover := props.selectedHoverColor,
                )
              else // is not selected
                fragment(
                  color := props._notSelectedFGColor,
                  backgroundColor.dynamic := props._notSelectedBGColor,
                  backgroundColor.dynamic.hover := props.notSelectedHoverColor,
                )
              ,
              if isFirst then // is first
                fragment(
                  borderTopLeftRadius := props._borderRadius,
                  borderBottomLeftRadius := props._borderRadius,
                )
              else // is not first
                fragment(
                )
              ,
              if isLast then // is last
                fragment(
                  borderTopRightRadius := props._borderRadius,
                  borderBottomRightRadius := props._borderRadius,
                )
              else // is not last
                fragment(
                  borderRight.csss(props._internalBorderSize, "solid", props._borderColor),
                )
              ,
              //
              showF(opt),
              onClick.a[A].handle { rh => onSelectF(rh, opt) *> state.update(_.copy(selected = opt)) },
            )(if isSelected then props._selectedButtonMod else props._notSelectedButtonMod)
          },
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
        horizontalRadio: HorizontalRadio.Decorator,
        labelSpacing: Option[String],
        surroundingPadding: String,
    )
    object Props extends PropsCompanion {

      override protected lazy val initialProps: Props =
        Props(
          label = Label.Decorator.defaultStyling,
          horizontalRadio = HorizontalRadio.Decorator.defaultStyling,
          labelSpacing = Label.defaultInputSpacing.some,
          surroundingPadding = 10.px,
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

      final def horizontalRadio(f: HorizontalRadio.Decorator => HorizontalRadio.Decorator): Decorator = {
        val dec = f(HorizontalRadio.Decorator.empty)
        make(s"horizontalRadio { ${dec.name} }") { current => current.copy(horizontalRadio = current.horizontalRadio >> dec) }
      }

      final def describe(description: Widget): Decorator =
        make("describe") { current => current.copy(label = current.label.describe(description)) }

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
    ): FormS[HorizontalRadio.State[A], A] = {
      val props = decorator(Decorator.empty.label(_.label(label))).computed

      Form.makeWith(
        label,
        div(
          padding := props.surroundingPadding,
          width.fitContent,
          Label(props.label),
          Spacing.vertical.opt(props.labelSpacing),
          div(HorizontalRadio[A].show(show).decorate(props.horizontalRadio)),
        ),
      )(_.selected)
    }

    def apply[A](
        label: String,
        decorator: Decorator => Decorator,
    ): FormS[HorizontalRadio.State[A], A] =
      apply[A](label, _.toString, decorator)

    def apply[A](
        label: String,
    ): FormS[HorizontalRadio.State[A], A] =
      apply[A](label, _.toString, identity)

  }

}
