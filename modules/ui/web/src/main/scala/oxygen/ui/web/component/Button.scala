package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object Button {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props private[Button] (
      _colors: StandardProps.DerivedColors,
      _padding: StandardProps.Padding,
      _borderRadius: String,
      _fontSize: String,
      _fontWeight: String,
      _cursor: String,
      _mod: NodeModifier,
  )
  object Props {

    private[Button] lazy val initial: Props =
      Props(StandardProps.DerivedColors.empty, StandardProps.Padding("0", "0"), S.borderRadius._0, S.fontSize._1, "0", "pointer", NodeModifier.empty)

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class Decorator private[Button] (private[Button] val decorator: GenericDecorator[Props]) extends DecoratorBuilder {

    def name: String = decorator.show

    private[Button] lazy val computed: Props = decorator.decorate(Props.initial)

    def >>(that: Decorator): Decorator = Decorator { this.decorator >> that.decorator }
    def <<(that: Decorator): Decorator = Decorator { this.decorator << that.decorator }

  }
  object Decorator extends DecoratorBuilder {

    override private[Button] val decorator: GenericDecorator[Props] = GenericDecorator.empty

    def all[S[_]: SeqRead](decorators: S[Decorator]): Decorator =
      Decorator { decorators.newIterator.foldLeft(GenericDecorator.empty) { (a, b) => a >> b.decorator } }

    def all(decorators: Decorator*): Decorator =
      all(decorators)

    val identity: Decorator = Decorator { GenericDecorator.empty }
    val empty: Decorator = identity

    lazy val defaultStyling: Decorator = empty.primary.standard.medium

  }

  trait DecoratorBuilder {
    private[Button] val decorator: GenericDecorator[Props]

    private def wrap(dec: GenericDecorator[Props]): Decorator = Decorator { decorator >> dec }
    private def wrap(name: String)(f: Props => Props): Decorator = wrap { GenericDecorator(name)(f) }

    /////// Size ///////////////////////////////////////////////////////////////

    final lazy val extraSmall: Decorator =
      wrap("ExtraSmall") { _.copy(_padding = StandardProps.Padding(S.spacing._2px, S.spacing._3), _borderRadius = S.borderRadius._2, _fontSize = S.fontSize._2, _fontWeight = "600") }
    final lazy val small: Decorator =
      wrap("Small") { _.copy(_padding = StandardProps.Padding(S.spacing._1, S.spacing._4), _borderRadius = S.borderRadius._4, _fontSize = S.fontSize._3, _fontWeight = "600") }
    final lazy val medium: Decorator =
      wrap("Medium") { _.copy(_padding = StandardProps.Padding(S.spacing._2, S.spacing._8), _borderRadius = S.borderRadius._5, _fontSize = S.fontSize._4, _fontWeight = "700") }
    final lazy val large: Decorator =
      wrap("Large") { _.copy(_padding = StandardProps.Padding(S.spacing._3, S.spacing._10), _borderRadius = S.borderRadius._5, _fontSize = S.fontSize._4, _fontWeight = "700") }
    final lazy val extraLarge: Decorator =
      wrap("ExtraLarge") { _.copy(_padding = StandardProps.Padding(S.spacing._4, S.spacing._14), _borderRadius = S.borderRadius._7, _fontSize = S.fontSize._5, _fontWeight = "700") }

    /////// Color ///////////////////////////////////////////////////////////////

    final def modifyColors(name: String)(f: StandardProps.DerivedColors => StandardProps.DerivedColors): Decorator =
      wrap(name) { p => p.copy(_colors = f(p._colors)) }

    final def baseColor(color: CSSColor): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))
    final def baseColor(color: CSSVar): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))
    final def baseColor(color: String): Decorator = modifyColors(s"baseColor($color)")(_.baseColor(color))

    private def baseStyleNamed(name: String, color: CSSVar): Decorator =
      wrap(name) { p => p.copy(_colors = p._colors.copy(_baseColor = color.getColorValue)) }

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
      wrap(name) { p => p.copy(_colors = p._colors.mod(modColor = _ => color, modBackgroundColor = _ => backgroundColor, modBorderColor = _ => borderColor), _cursor = cursor.getOrElse(p._cursor)) }

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

    final def disabledWhen(condition: Boolean): Decorator = if (condition) this.disabled else Decorator(decorator)
    final def disabledProgressWhen(condition: Boolean): Decorator = if (condition) this.disabledProgress else Decorator(decorator)

    /////// Misc ///////////////////////////////////////////////////////////////

    final def padding(topBottom: String, leftRight: String): Decorator = wrap("custom(padding)") { _.copy(_padding = StandardProps.Padding(topBottom, leftRight)) }
    final def borderRadius(radius: String): Decorator = wrap("custom(borderRadius)") { _.copy(_borderRadius = radius) }
    final def fontSize(size: String): Decorator = wrap("custom(fontSize)") { _.copy(_fontSize = size) }
    final def fontWeight(weight: String): Decorator = wrap("custom(fontWeight)") { _.copy(_fontWeight = weight) }
    final def cursor(cursor: String): Decorator = wrap("custom(cursor)") { _.copy(_cursor = cursor) }

    // TODO (KR) : some ability to wrap the element?

    final def setCustomMod(mod: NodeModifier): Decorator = wrap("custom(setMod)") { _.copy(_mod = mod) }
    final def addCustomMod(mod: NodeModifier): Decorator = wrap("custom(addMod)") { p => p.copy(_mod = p._mod <> mod) }
    final def prepend(before: Widget*): Decorator = addCustomMod(NodeModifier.before(before*))
    final def append(after: Widget*): Decorator = addCustomMod(NodeModifier.after(after*))
    final def surround(before: Widget*)(after: Widget*): Decorator = addCustomMod(NodeModifier.surround(before*)(after*))

    lazy val defaultFG: Decorator = modifyColors("Default FG") { _.setColor(S.color.fg.default) }
    lazy val inverseFG: Decorator = modifyColors("Inverse FG") { _.setColor(S.color.fg.inverse) }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(
      mainText: String,
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
    )(
      mainText,
    )(props._mod)
  }

  def apply(
      mainText: String,
      decorator: Decorator => Decorator,
  ): Node =
    Button(mainText, decorator(Decorator.defaultStyling))

  def apply(
      mainText: String,
  ): Node =
    Button(mainText, Decorator.defaultStyling)

}
