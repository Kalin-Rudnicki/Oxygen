package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

object InfoSection extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      color: String,
      backHighlight: Boolean, // if you want to use this, your color must be a 6-digit hex string, or a variable that points to one
  ) {

    lazy val backHighlightColor: String = CSSColor.eval(color).setOpacity(10.0)

  }
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        color = "transparent",
        backHighlight = false,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    private def makeColor(n: String, c: String): Decorator = make(n) { _.copy(color = c) }

    final def color(c: String): Decorator = makeColor(s"custom(color = $c)", c)
    final lazy val primary: Decorator = makeColor("Primary", S.color.primary)
    final lazy val positive: Decorator = makeColor("Positive", S.color.status.positive)
    final lazy val negative: Decorator = makeColor("Negative", S.color.status.negative)
    final lazy val alert: Decorator = makeColor("Alert", S.color.status.alert)
    final lazy val informational: Decorator = makeColor("Info", S.color.status.informational)
    final lazy val brandPrimary1: Decorator = makeColor("BrandPrimary1", S.color.brand.primary1)
    final lazy val brandPrimary2: Decorator = makeColor("BrandPrimary2", S.color.brand.primary2)

    final lazy val backHighlight: Decorator = make("backHighlight") { _.copy(backHighlight = true) }

  }

  final class Decorator private[InfoSection] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.informational

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(decorator: Decorator): Node = {
    val props: Props = decorator.computed
    p(
      margin(S.spacing._2, S.spacing._0),
      padding(S.spacing._2, S.spacing._4),
      borderLeft(2.px, "solid", props.color),
      Widget.when(props.backHighlight) {
        backgroundColor := props.backHighlightColor
      },
    )
  }

  def apply(decorator: Decorator => Decorator): Node =
    apply(decorator(Decorator.defaultStyling))

  def apply(): Node = default

  lazy val default: Node = apply(Decorator.defaultStyling)

}
