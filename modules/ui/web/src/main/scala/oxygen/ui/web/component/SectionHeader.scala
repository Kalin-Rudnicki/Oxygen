package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

object SectionHeader extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      color: String,
      indent: String,
      padding: String,
      tag: String,
      mod: NodeModifier,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        color = "transparent",
        indent = "0",
        padding = "0",
        tag = "div",
        mod = NodeModifier.empty,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final lazy val section1: Decorator = make("section1") { _.copy(color = S.color.primary, indent = S.spacing._10, padding = css("0", S.spacing._10, S.spacing._1), tag = "h2") }
    final lazy val section2: Decorator = make("section2") { _.copy(color = S.color.fg.moderate, indent = S.spacing._7, padding = css("0", S.spacing._7, S.spacing._1), tag = "h3") }
    final lazy val section3: Decorator = make("section3") { _.copy(color = S.color.fg.minimal, indent = S.spacing._4, padding = css("0", S.spacing._4, S.spacing._1), tag = "h4") }

    private def makeColor(n: String, c: String): Decorator = make(n) { _.copy(color = c) }

    final def color(c: String): Decorator = makeColor(s"custom(color = $c)", c)
    final lazy val primary: Decorator = makeColor("Primary", S.color.primary)
    final lazy val positive: Decorator = makeColor("Positive", S.color.status.positive)
    final lazy val negative: Decorator = makeColor("Negative", S.color.status.negative)
    final lazy val alert: Decorator = makeColor("Alert", S.color.status.alert)
    final lazy val informational: Decorator = makeColor("Informational", S.color.status.informational)
    final lazy val brandPrimary1: Decorator = makeColor("BrandPrimary1", S.color.brand.primary1)
    final lazy val brandPrimary2: Decorator = makeColor("BrandPrimary2", S.color.brand.primary2)

    final lazy val mod: FocusNodeModifier = focusNodeModifier("mod")(_.mod)

  }

  final class Decorator private[SectionHeader] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(
      text: String,
      decorator: Decorator,
  ): Node = {
    val props = decorator.computed
    Widget.node(props.tag)(
      text,
      color := props.color,
      padding := props.padding,
      margin("0", "0", "0", props.indent),
      borderBottom(2.px, "solid", props.color),
      width.fitContent,
    )(props.mod)
  }

  def section1(text: String, decorator: Decorator => Decorator = identity): Node =
    apply(text, decorator(Decorator.section1))

  def section2(text: String, decorator: Decorator => Decorator = identity): Node =
    apply(text, decorator(Decorator.section2))

  def section3(text: String, decorator: Decorator => Decorator = identity): Node =
    apply(text, decorator(Decorator.section3))

}
