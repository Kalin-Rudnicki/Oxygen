package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object Button extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      _colors: StandardProps.DerivedColors,
      _padding: StandardProps.Padding,
      _borderRadius: String,
      _fontSize: String,
      _fontWeight: String,
      _cursor: String,
      _mod: NodeModifier,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(StandardProps.DerivedColors.empty, StandardProps.Padding("0", "0"), S.borderRadius._0, S.fontSize._1, "0", "pointer", NodeModifier.empty)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    /////// Size ///////////////////////////////////////////////////////////////

    private def makeSize(name: String)(padding: StandardProps.Padding, borderRadius: String, fontSize: String, fontWeight: String): Decorator =
      make(name) { _.copy(_padding = padding, _borderRadius = borderRadius, _fontSize = fontSize, _fontWeight = fontWeight) }

    final lazy val extraSmall: Decorator =
      makeSize("ExtraSmall")(StandardProps.Padding(S.spacing._2px, S.spacing._3), S.borderRadius._2, S.fontSize._2, S.fontWeight.semiBold)
    final lazy val small: Decorator =
      makeSize("Small")(StandardProps.Padding(S.spacing._1, S.spacing._4), S.borderRadius._4, S.fontSize._3, S.fontWeight.semiBold)
    final lazy val medium: Decorator =
      makeSize("Medium")(StandardProps.Padding(S.spacing._2, S.spacing._8), S.borderRadius._5, S.fontSize._4, S.fontWeight.bold)
    final lazy val large: Decorator =
      makeSize("Large")(StandardProps.Padding(S.spacing._3, S.spacing._10), S.borderRadius._5, S.fontSize._4, S.fontWeight.bold)
    final lazy val extraLarge: Decorator =
      makeSize("ExtraLarge")(StandardProps.Padding(S.spacing._4, S.spacing._14), S.borderRadius._7, S.fontSize._5, S.fontWeight.bold)

    /////// Color ///////////////////////////////////////////////////////////////

    final def modifyColors(name: String)(f: StandardProps.DerivedColors => StandardProps.DerivedColors): Decorator =
      make(name) { p => p.copy(_colors = f(p._colors)) }

    final def baseColor(color: CSSColor): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))
    final def baseColor(color: CSSVar): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))
    final def baseColor(color: String): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))

    private def baseStyleNamed(name: String, color: CSSVar): Decorator =
      make(name) { p => p.copy(_colors = p._colors.copy(_baseColor = color.getColorValue)) }

    final lazy val primary: Decorator = baseStyleNamed("Primary", S.color.primary)
    final lazy val positive: Decorator = baseStyleNamed("Positive", S.color.status.positive)
    final lazy val negative: Decorator = baseStyleNamed("Negative", S.color.status.negative)
    final lazy val alert: Decorator = baseStyleNamed("Alert", S.color.status.alert)
    final lazy val informational: Decorator = baseStyleNamed("Informational", S.color.status.informational)
    final lazy val destructive: Decorator = baseStyleNamed("Destructive", S.color.status.destructive)
    final lazy val brandPrimary1: Decorator = baseStyleNamed("BrandPrimary1", S.color.brand.primary1)
    final lazy val brandPrimary2: Decorator = baseStyleNamed("BrandPrimary2", S.color.brand.primary2)

    final lazy val info: Decorator = informational

    /////// Style ///////////////////////////////////////////////////////////////

    private def makeStyle(
        name: String,
        color: StandardProps.DerivedColors.Group,
        backgroundColor: StandardProps.DerivedColors.Group,
        borderColor: StandardProps.DerivedColors.Group,
        cursor: Specified[String] = ___,
    ): Decorator =
      make(name) { p => p.copy(_colors = p._colors.mod(modColor = _ => color, modBackgroundColor = _ => backgroundColor, modBorderColor = _ => borderColor), _cursor = cursor.getOrElse(p._cursor)) }

    final lazy val standard: Decorator =
      makeStyle(
        name = "Standard",
        color = StandardProps.DerivedColors.Group(
          _base = ColorTransform.const(S.color.fg.inverse),
        ),
        backgroundColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.none,
          _hover = ColorTransform.darken(15.0),
          _active = ColorTransform.darken(30.0),
        ),
        borderColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.darken(50.0),
        ),
      )
    final lazy val subtle: Decorator =
      makeStyle(
        name = "Subtle",
        color = StandardProps.DerivedColors.Group(
          _base = ColorTransform.none,
        ),
        backgroundColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.darken(70.0) >>> ColorTransform.setOpacity(15.0),
          _hover = ColorTransform.darken(50.0) >>> ColorTransform.setOpacity(30.0),
          _active = ColorTransform.darken(75.0) >>> ColorTransform.setOpacity(40.0),
        ),
        borderColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.none,
        ),
      )
    final lazy val minimal: Decorator =
      makeStyle(
        name = "Minimal",
        color = StandardProps.DerivedColors.Group(
          _base = ColorTransform.none,
        ),
        backgroundColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.darken(70.0) >>> ColorTransform.setOpacity(15.0),
          _hover = ColorTransform.darken(50.0) >>> ColorTransform.setOpacity(30.0),
          _active = ColorTransform.darken(75.0) >>> ColorTransform.setOpacity(40.0),
        ),
        borderColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.transparent,
          _hover = ColorTransform.none,
        ),
      )
    final lazy val disabled: Decorator =
      makeStyle(
        name = "Disabled",
        color = StandardProps.DerivedColors.Group(
          _base = ColorTransform.const(S.color.fg.subtle),
        ),
        backgroundColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.const("#0004"),
        ),
        borderColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.transparent,
        ),
        cursor = "not-allowed",
      )
    final lazy val disabledProgress: Decorator =
      makeStyle(
        name = "Disabled (Progress)",
        color = StandardProps.DerivedColors.Group(
          _base = ColorTransform.const(S.color.fg.subtle),
        ),
        backgroundColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.const("#0004"),
        ),
        borderColor = StandardProps.DerivedColors.Group(
          _base = ColorTransform.transparent,
        ),
        cursor = "progress",
      )

    final def disabledWhen(condition: Boolean): Decorator = if (condition) this.disabled else Decorator(genericDecorator)
    final def disabledProgressWhen(condition: Boolean): Decorator = if (condition) this.disabledProgress else Decorator(genericDecorator)

    /////// Misc ///////////////////////////////////////////////////////////////

    final def padding(topBottom: String, leftRight: String): Decorator = make("custom(padding)") { _.copy(_padding = StandardProps.Padding(topBottom, leftRight)) }
    final def borderRadius(radius: String): Decorator = make("custom(borderRadius)") { _.copy(_borderRadius = radius) }
    final def fontSize(size: String): Decorator = make("custom(fontSize)") { _.copy(_fontSize = size) }
    final def fontWeight(weight: String): Decorator = make("custom(fontWeight)") { _.copy(_fontWeight = weight) }
    final def cursor(cursor: String): Decorator = make("custom(cursor)") { _.copy(_cursor = cursor) }

    // TODO (KR) : some ability to wrap the element?

    final lazy val mod: FocusNodeModifier = focusNodeModifier("mod")(_._mod)

    lazy val defaultFG: Decorator = modifyColors("Default FG") { _.setColor(S.color.fg.default) }
    lazy val inverseFG: Decorator = modifyColors("Inverse FG") { _.setColor(S.color.fg.inverse) }

  }

  final class Decorator private[Button] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.primary.standard.medium

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(
      decorator: Decorator,
  ): Node = {
    val props: Props = decorator.computed

    button(
      O.Button,
      borderStyle := "solid",
      borderWidth := 1.px,
      padding := props._padding.show,
      borderRadius := props._borderRadius,
      fontSize := props._fontSize,
      fontWeight := props._fontWeight,
      cursor := props._cursor,
      props._colors.styles,
    )(props._mod)
  }

  def apply(
      decorator: Decorator => Decorator,
  ): Node =
    Button(decorator(Decorator.defaultStyling))

  def apply(): Node =
    default

  lazy val default: Node = Button(Decorator.defaultStyling)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object form extends Decorable {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Props
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Props(
        button: Button.Decorator,
        surroundingPadding: String,
    )
    object Props extends PropsCompanion {

      override protected lazy val initialProps: Props =
        Props(
          button = Button.Decorator.defaultStyling,
          surroundingPadding = css(10.px, 35.px),
        )

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Decorator
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    trait DecoratorBuilder extends DecoratorBuilder0 {

      final def button(f: Button.Decorator => Button.Decorator): Decorator = {
        val dec = f(Button.Decorator.empty)
        make(s"button { ${dec.name} }") { current => current.copy(button = current.button >> dec) }
      }

    }

    final class Decorator private[form] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
    object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

      override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

      override lazy val defaultStyling: Decorator = empty

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Widget
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    def apply(
        buttonMainText: String,
        decorator: Decorator => Decorator = identity,
    ): SubmitForm[Unit] = {
      val props = decorator(Decorator.empty.button(_.mod(buttonMainText))).computed

      Form.unit(
        div(
          padding := props.surroundingPadding,
          Button(props.button)(
            onClick.action(Form.Submit),
          ),
        ),
      )
    }
  }

}
