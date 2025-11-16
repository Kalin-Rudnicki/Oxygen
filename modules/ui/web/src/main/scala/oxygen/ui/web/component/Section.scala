package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

object Section extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      margin: String,
      padding: String,
      borderRadius: String,
      backgroundColor: String,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        margin = "0",
        padding = "0",
        borderRadius = "0",
        backgroundColor = "transparent",
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final lazy val section1: Decorator =
      make("section1") { _.copy(margin = css(S.spacing._2, S.spacing._14), padding = css(S.spacing._5, S.spacing._10), borderRadius = S.borderRadius._8, backgroundColor = S.color.bg.layerOne) }

    final lazy val section2: Decorator =
      make("section2") { _.copy(margin = css(S.spacing._2, S.spacing._0), padding = css(S.spacing._5, S.spacing._10), borderRadius = S.borderRadius._5, backgroundColor = S.color.bg.layerTwo) }

    final lazy val section3: Decorator =
      make("section3") { _.copy(margin = css(S.spacing._1, S.spacing._0), padding = css(S.spacing._3, S.spacing._6), borderRadius = S.borderRadius._3, backgroundColor = S.color.bg.layerThree) }

  }

  final class Decorator private[Section] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(decorator: Decorator): Node = {
    val props = decorator.computed

    div(
      margin := props.margin,
      padding := props.padding,
      borderRadius := props.borderRadius,
      backgroundColor := props.backgroundColor,
    )
  }

  def section1(decorator: Decorator => Decorator = identity): Node =
    apply(decorator(Decorator.section1))

  def section2(decorator: Decorator => Decorator = identity): Node =
    apply(decorator(Decorator.section2))

  def section3(decorator: Decorator => Decorator = identity): Node =
    apply(decorator(Decorator.section3))

}
