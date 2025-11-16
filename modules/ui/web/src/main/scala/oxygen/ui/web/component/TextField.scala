package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}
import zio.*

object TextField extends Decorable {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Props(
      inputType: String,
      padding: String,
      borderRadius: String,
      fontSize: String,
      width: String,
      trimInput: Boolean,
      mod: NodeModifier,
  )
  object Props extends PropsCompanion {

    override protected lazy val initialProps: Props =
      Props(
        inputType = "text",
        padding = "0",
        fontSize = "0",
        borderRadius = "0",
        width = "0",
        trimInput = true,
        mod = NodeModifier.empty,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  trait DecoratorBuilder extends DecoratorBuilder0 {

    private def makeSize(
        size: String,
        padding: String,
        borderRadius: String,
        fontSize: String,
        width: String,
    ): Decorator =
      make(size)(_.copy(padding = padding, borderRadius = borderRadius, fontSize = fontSize, width = width))

    final lazy val extraSmall: Decorator = makeSize("ExtraSmall", css(S.spacing._2px, S.spacing._2), S.borderRadius._2, S.fontSize._3, 15.ch)
    final lazy val small: Decorator = makeSize("Small", css(S.spacing._1, S.spacing._3), S.borderRadius._3, S.fontSize._4, 20.ch)
    final lazy val medium: Decorator = makeSize("Medium", css(s"calc(${S.spacing._1} * 1.5)", S.spacing._3), S.borderRadius._3, S.fontSize._4, 25.ch)
    final lazy val large: Decorator = makeSize("Large", css(S.spacing._2, S.spacing._4), S.borderRadius._4, S.fontSize._5, 30.ch)
    final lazy val extraLarge: Decorator = makeSize("ExtraLarge", css(S.spacing._2, S.spacing._5), S.borderRadius._5, S.fontSize._5, 40.ch)

    final def inputType(tpe: String): Decorator = make(s"inputType($tpe)") { _.copy(inputType = tpe) }
    final lazy val text: Decorator = inputType("text")
    final lazy val password: Decorator = inputType("password")
    final lazy val email: Decorator = inputType("email")

    final def padding(topBottom: String, leftRight: String): Decorator = make("padding") { _.copy(padding = css(topBottom, leftRight)) }
    final def borderRadius(borderRadius: String): Decorator = make(s"borderRadius($borderRadius)") { _.copy(borderRadius = borderRadius) }
    final def fontSize(fontSize: String): Decorator = make(s"fontSize($fontSize)") { _.copy(fontSize = fontSize) }
    final def width(width: String): Decorator = make(s"width($width)") { _.copy(width = width) }

    final def trimInput: Decorator = make("trimInput") { _.copy(trimInput = true) }
    final def noTrimInput: Decorator = make("noTrimInput") { _.copy(trimInput = false) }

    final lazy val mod: FocusNodeModifier = focusNodeModifier("mod")(_.mod)

  }

  final class Decorator private[TextField] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
  object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

    override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

    override lazy val defaultStyling: Decorator = empty.medium

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  opaque type State = String
  object State {

    val empty: State = ""
    def initial(value: String): State = value

    extension (self: State) def text: String = self

  }

  def apply(
      decorator: Decorator,
  ): WidgetAS[Form.Submit, State] = {
    val props = decorator.computed
    input(
      `type` := props.inputType,
      width := props.width,
      padding := props.padding,
      borderRadius := props.borderRadius,
      fontFamily := S.fontStyle.default,
      border := "none",
      outline := "none",
      Widget.state[State].fix { state =>
        value := state.unsafeCurrentValue
      },
      onKeyUp.eas[Form.Submit, State].handle { (s, rh, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]

        def targetValue: String = target.value.asInstanceOf[String]
        // weird weird out-of-order dom events

        if (e.keyCode == KeyCode.Enter.keyCode) {
          // cant be in a ZIO
          e.preventDefault()

          s.update(_ => targetValue) *>
            rh.raiseAction(Form.Submit)
        } else
          s.update(_ => targetValue)
      },
      onChange.es[State].handle { (s, e) =>
        val target = e.target.asInstanceOf[scala.scalajs.js.Dynamic]

        def targetValue: String = target.value.asInstanceOf[String]
        // weird weird out-of-order dom events

        s.update(_ => targetValue)
      },
    )(props.mod)
  }

  def apply(
      decorator: Decorator => Decorator,
  ): WidgetAS[Form.Submit, State] =
    TextField(decorator(Decorator.defaultStyling))

  def apply(
  ): WidgetAS[Form.Submit, State] =
    TextField(Decorator.defaultStyling)

  /**
    * NOTE : [[fieldName]] is not added as a label, it is only used to label errors.
    */
  def rawForm[A: StringDecoder as dec](
      fieldName: String,
      decorator: Decorator,
  ): SubmitFormS[State, Option[A]] =
    Form.makeWithValidation(fieldName, TextField(decorator)) { rawValue =>
      val value: String = if (decorator.computed.trimInput) rawValue.trim else rawValue
      Option.when(value.nonEmpty)(value).traverse(dec.decodeSimple)
    }

  /**
    * NOTE : [[fieldName]] is not added as a label, it is only used to label errors.
    */
  def rawForm[A: StringDecoder as dec](
      fieldName: String,
      decorator: Decorator => Decorator,
  ): SubmitFormS[State, Option[A]] =
    rawForm[A](fieldName, decorator(Decorator.defaultStyling))

  /**
    * NOTE : [[fieldName]] is not added as a label, it is only used to label errors.
    */
  def rawForm[A: StringDecoder as dec](
      fieldName: String,
  ): SubmitFormS[State, Option[A]] =
    rawForm[A](fieldName, Decorator.defaultStyling)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  object form extends Decorable {

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Props
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    final case class Props(
        label: Label.Decorator,
        textField: TextField.Decorator,
        labelSpacing: Option[String],
        surroundingPadding: String,
        width: String,
    )
    object Props extends PropsCompanion {

      override protected lazy val initialProps: Props =
        Props(
          label = Label.Decorator.defaultStyling,
          textField = TextField.Decorator.defaultStyling,
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

      final def textField(f: TextField.Decorator => TextField.Decorator): Decorator = {
        val dec = f(TextField.Decorator.empty)
        make(s"textField { ${dec.name} }") { current => current.copy(textField = current.textField >> dec) }
      }

      final def describe(description: Widget): Decorator =
        make("describe") { current => current.copy(label = current.label.describe(description)) }

      final def width(width: String): Decorator =
        make(s"custom(width = $width)") { current => current.copy(width = width, textField = current.textField.width(100.pct)) }

      final lazy val text: Decorator = textField(_.text)
      final lazy val password: Decorator = textField(_.password)
      final lazy val email: Decorator = textField(_.email)

    }

    final class Decorator private[form] (protected val genericDecorator: GenericDecorator[Props]) extends DecoratorBuilder, DecoratorBuilderType
    object Decorator extends DecoratorBuilder, DecoratorBuilderCompanion {

      override protected def wrapGeneric(genericDecorator: GenericDecorator[Props]): Decorator = new Decorator(genericDecorator)

      override lazy val defaultStyling: Decorator = empty

    }

    //////////////////////////////////////////////////////////////////////////////////////////////////////
    //      Widget
    //////////////////////////////////////////////////////////////////////////////////////////////////////

    def apply[A: StringDecoder as dec](
        label: String,
        decorator: Decorator => Decorator = identity,
    ): SubmitFormS[TextField.State, Option[A]] = {
      val props = decorator(Decorator.empty.label(_.label(label))).computed

      TextField.rawForm[A](label, props.textField).map { (textFieldWidget, textFieldValue) =>
        (
          div(
            padding := props.surroundingPadding,
            width := props.width,
            Label(props.label),
            Spacing.vertical.opt(props.labelSpacing),
            textFieldWidget,
          ),
          textFieldValue,
        )
      }
    }

  }

}
