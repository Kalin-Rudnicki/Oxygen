package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object Label extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      labelText: String,
      description: Option[Widget],
      labelMarginLeft: String,
      descriptionMarginLeft: String,
      labelDescriptionSpacing: String,
      mod: NodeModifier,
      labelMod: NodeModifier,
      descriptionMod: NodeModifier,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        labelText = "<???>",
        description = None,
        labelMarginLeft = S.spacing._6,
        descriptionMarginLeft = S.spacing._3,
        labelDescriptionSpacing = S.spacing._1,
        mod = NodeModifier.empty,
        labelMod = NodeModifier.empty,
        descriptionMod = NodeModifier.empty,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    final def label(text: String): Decorator = make("labeled") { _.copy(labelText = text) }
    final def describe(description: Widget): Decorator = make("described") { _.copy(description = description.some) }

    final def labelMarginLeft(value: String): Decorator = make("labelMarginLeft") { _.copy(labelMarginLeft = value) }
    final def descriptionMarginLeft(value: String): Decorator = make("descriptionMarginLeft") { _.copy(descriptionMarginLeft = value) }

    final lazy val mod: FocusNodeModifier = focusNodeModifier("mod")(_.mod)
    final lazy val labelMod: FocusNodeModifier = focusNodeModifier("labelMod")(_.labelMod)
    final lazy val descriptionMod: FocusNodeModifier = focusNodeModifier("descriptionMod")(_.descriptionMod)

  }

  final class Decorator private[Label] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def apply(decorator: Decorator): Widget = {
    val props = decorator.computed

    div(
      O.Label,
      div(
        O.Label.LabelText,
        marginLeft := props.labelMarginLeft,
        fontWeight := S.fontWeight.semiBold,
        fontSize := S.fontSize._4,
        props.labelText,
      )(props.labelMod),
      Widget.foreach(props.description) { description =>
        div(
          O.Label.DescriptionText,
          whiteSpace.pre,
          marginTop := props.labelDescriptionSpacing,
          marginLeft := props.descriptionMarginLeft,
          description,
        )(props.descriptionMod)
      },
    )(props.mod)
  }

  def apply(decorator: Decorator => Decorator): Widget =
    Label(decorator(Decorator.defaultStyling))

  lazy val defaultInputSpacing: String = S.spacing._2

}
