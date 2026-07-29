package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * HolyGrail-style text field (W2-T03). Overflow-safe: maxWidth 100%, box-sizing border-box.
  */
final case class TextField(
    private val _size: Size,
    private val _inputType: String,
    private val _width: String,
    trimInput: Boolean,
    private val _extra: Widget,
) extends PWidget.Deferred[Any, Form.Submit, TextField.State, TextField.State] {

  def size(s: Size): TextField = copy(_size = s)
  def inputType(t: String): TextField = copy(_inputType = t)
  def width(w: String): TextField = copy(_width = w)
  def trimInput(t: Boolean): TextField = copy(trimInput = t)

  def extraSmall: TextField = size(Size.ExtraSmall)
  def small: TextField = size(Size.Small)
  def medium: TextField = size(Size.Medium)
  def large: TextField = size(Size.Large)
  def extraLarge: TextField = size(Size.ExtraLarge)

  def text: TextField = inputType("text")
  def password: TextField = inputType("password")
  def email: TextField = inputType("email")

  def noTrimInput: TextField = trimInput(false)
  def withTrimInput: TextField = trimInput(true)

  def extra(mods: Widget*): TextField = copy(_extra = fragment(this._extra, Widget.fragment(mods)))

  override protected def build: PWidget[Any, Form.Submit, TextField.State, TextField.State] =
    TextField.render(this)

}
object TextField {

  private val autoWidth: String = "__auto__"

  opaque type State = String
  object State {
    val empty: State = ""
    def initial(value: String): State = value
    extension (self: State) def text: String = self
  }

  private def sizeTokens(size: Size): (String, String, String, String) =
    size match {
      case Size.ExtraSmall => (css(S.spacing._2px, S.spacing._2), S.borderRadius._2, S.fontSize._3, 15.ch)
      case Size.Small      => (css(S.spacing._1, S.spacing._3), S.borderRadius._3, S.fontSize._4, 20.ch)
      case Size.Medium     => (css(s"calc(${S.spacing._1} * 1.5)", S.spacing._3), S.borderRadius._3, S.fontSize._4, 25.ch)
      case Size.Large      => (css(S.spacing._2, S.spacing._4), S.borderRadius._4, S.fontSize._5, 30.ch)
      case Size.ExtraLarge => (css(S.spacing._2, S.spacing._5), S.borderRadius._5, S.fontSize._5, 40.ch)
    }

  private def render(field: TextField): WidgetAS[Form.Submit, State] = {
    val (pad, radius, fSize, defaultWidth) = sizeTokens(field._size)
    val w = if field._width == autoWidth then defaultWidth else field._width
    input(
      `type` := field._inputType,
      width := w,
      maxWidth := 100.pct,
      boxSizing.borderBox,
      padding := pad,
      borderRadius := radius,
      fontSize := fSize,
      fontFamily := S.fontStyle.default,
      // Visible field chrome: never `border: none` — that + layerOne bg makes inputs
      // disappear on Section.level1 (also layerOne). Match global input stylesheet.
      border := s"1px solid ${S.color.fg.subtle}",
      borderStyle.solid,
      backgroundColor := S.color.bg.layerTwo,
      color := S.color.fg.default,
      Widget.state[State].fix { state =>
        value := state.unsafeCurrentValue
      },
      // Enter must be handled on keydown — text inputs do not fire `change` on Enter,
      // and form submit is a keydown-time gesture.
      onKeyDown.eas[Form.Submit, State].handle { (s, rh, e) =>
        if KeyCode.isEnter(e) then {
          e.preventDefault()
          val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
          s.update(_ => targetValue) *> rh.raiseAction(Form.Submit)
        } else ZIO.unit
      },
      onKeyUp.es[State].handle { (s, e) =>
        val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
        s.update(_ => targetValue)
      },
      onChange.es[State].handle { (s, e) =>
        val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
        s.update(_ => targetValue)
      },
      field._extra,
    )
  }

  val empty: TextField =
    TextField(Size.Medium, "text", autoWidth, true, Widget.empty)

  def apply(): TextField = empty

  def apply(configure: TextField => TextField): TextField =
    configure(empty)

  def rawForm[A: StringDecoder as dec](
      fieldName: String,
      field: TextField = empty,
  ): SubmitFormS[State, Option[A]] =
    Form.makeWithValidation(fieldName, field) { rawValue =>
      val value: String = if field.trimInput then rawValue.trim else rawValue
      Option.when(value.nonEmpty)(value).traverse(dec.decodeSimple)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled text field form builder. Extends [[PForm.Deferred]] so composition
    * (`<*>`, `zoomOut`, `required`, `onSubmit`, …) works after fluent config.
    *
    * Holds a typed [[TextField]] child — use [[modField]] to drill without losing type info.
    *
    * {{{
    * TextField.form[Email]("Email").email.width(300.px).required.zoomOut[Page](_.email)
    * }}}
    */
  final case class form[+Value] private (
      private val _fieldName: String,
      private val _field: TextField,
      private val _label: Label,
      private val _surroundingPadding: String,
      private val _width: String,
      private val _labelSpacing: Option[String],
      private val _decode: String => Either[String, Value],
  ) extends PForm.Deferred.Stateful[Any, Form.Submit, TextField.State, Option[Value]] {

    override protected lazy val build: PForm[Any, Form.Submit, TextField.State, TextField.State, Option[Value]] =
      Form.makeWithValidation(_fieldName, _field) { rawValue =>
        val value: String = if _field.trimInput then rawValue.trim else rawValue
        Option.when(value.nonEmpty)(value).traverse(_decode)
      }.map { (fieldWidget, fieldValue) =>
        (
          div(
            padding := _surroundingPadding,
            // Qualify CSS attrs that clash with form chrome methods (`width`).
            oxygen.ui.web.create.width := _width,
            maxWidth := 100.pct,
            boxSizing.borderBox,
            _label,
            Spacing.vertical.opt(_labelSpacing),
            fieldWidget,
          ),
          fieldValue,
        )
      }

    /** Drill into the underlying [[TextField]] builder (typed child). */
    def modField(f: TextField => TextField): form[Value] = copy(_field = f(_field))

    /** Drill into the [[Label]] chrome. */
    def modLabel(f: Label => Label): form[Value] = copy(_label = f(_label))

    def field: TextField = _field
    def label: Label = _label

    /////// field shortcuts ///////////////////////////////////////////////////////////////

    def size(s: Size): form[Value] = modField(_.size(s))
    def inputType(t: String): form[Value] = modField(_.inputType(t))
    def text: form[Value] = modField(_.text)
    def password: form[Value] = modField(_.password)
    def email: form[Value] = modField(_.email)
    def extraSmall: form[Value] = modField(_.extraSmall)
    def small: form[Value] = modField(_.small)
    def medium: form[Value] = modField(_.medium)
    def large: form[Value] = modField(_.large)
    def extraLarge: form[Value] = modField(_.extraLarge)
    def trimInput: form[Value] = modField(_.withTrimInput)
    def noTrimInput: form[Value] = modField(_.noTrimInput)
    def fieldExtra(mods: Widget*): form[Value] = modField(_.extra(mods*))

    /////// chrome shortcuts ///////////////////////////////////////////////////////////////

    def describe(d: Widget): form[Value] = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form[Value] = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form[Value] = copy(_surroundingPadding = p)
    def width(w: String): form[Value] = copy(_width = w, _field = _field.width(100.pct))
    def labelSpacing(s: Option[String]): form[Value] = copy(_labelSpacing = s)
    def noLabelSpacing: form[Value] = labelSpacing(None)

  }
  object form {

    def apply[A: StringDecoder as dec](label: String): TextField.form[A] =
      new TextField.form[A](
        _fieldName = label,
        _field = TextField.empty,
        _label = Label(label),
        _surroundingPadding = 10.px,
        _width = "fit-content",
        _labelSpacing = Label.defaultInputSpacing.some,
        _decode = dec.decodeSimple,
      )

  }

}
