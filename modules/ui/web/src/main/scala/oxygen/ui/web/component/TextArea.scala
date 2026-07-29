package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * HolyGrail-style text area (W2-T04). Overflow-safe: maxWidth 100%, box-sizing border-box.
  */
final case class TextArea(
    private val _size: Size,
    private val _inputType: String,
    private val _width: String,
    private val _height: String,
    trimInput: Boolean,
    private val _extra: Widget,
) extends PWidget.Deferred[Any, Form.Submit, TextArea.State, TextArea.State] {

  def size(s: Size): TextArea = copy(_size = s)
  def inputType(t: String): TextArea = copy(_inputType = t)
  def width(w: String): TextArea = copy(_width = w)
  def height(h: String): TextArea = copy(_height = h)
  def trimInput(t: Boolean): TextArea = copy(trimInput = t)

  def extraSmall: TextArea = size(Size.ExtraSmall)
  def small: TextArea = size(Size.Small)
  def medium: TextArea = size(Size.Medium)
  def large: TextArea = size(Size.Large)
  def extraLarge: TextArea = size(Size.ExtraLarge)

  def text: TextArea = inputType("text")
  def password: TextArea = inputType("password")
  def email: TextArea = inputType("email")

  def noTrimInput: TextArea = trimInput(false)
  def withTrimInput: TextArea = trimInput(true)

  def extra(mods: Widget*): TextArea = copy(_extra = fragment(this._extra, Widget.fragment(mods)))

  override protected def build: PWidget[Any, Form.Submit, TextArea.State, TextArea.State] =
    TextArea.render(this)

}
object TextArea {

  private val autoWidth: String = "__auto__"

  opaque type State = String
  object State {
    val empty: State = ""
    def initial(value: String): State = value
    extension (self: State) def text: String = self
  }

  private def sizeTokens(size: Size): (String, String, String, String) =
    size match {
      case Size.ExtraSmall => (css(S.spacing._2px, S.spacing._2), S.borderRadius._2, S.fontSize._3, 30.ch)
      case Size.Small      => (css(S.spacing._1, S.spacing._3), S.borderRadius._3, S.fontSize._4, 40.ch)
      case Size.Medium     => (css(s"calc(${S.spacing._1} * 1.5)", S.spacing._3), S.borderRadius._3, S.fontSize._4, 50.ch)
      case Size.Large      => (css(S.spacing._2, S.spacing._4), S.borderRadius._4, S.fontSize._5, 60.ch)
      case Size.ExtraLarge => (css(S.spacing._2, S.spacing._5), S.borderRadius._5, S.fontSize._5, 75.ch)
    }

  private def render(field: TextArea): WidgetAS[Form.Submit, State] = {
    val (pad, radius, fSize, defaultWidth) = sizeTokens(field._size)
    val w = if field._width == autoWidth then defaultWidth else field._width
    textArea(
      `type` := field._inputType,
      width := w,
      maxWidth := 100.pct,
      boxSizing.borderBox,
      height := field._height,
      padding := pad,
      borderRadius := radius,
      fontSize := fSize,
      fontFamily := S.fontStyle.default,
      // Same as TextField: keep a visible border + inset bg so fields read on section cards.
      border := s"1px solid ${S.color.fg.subtle}",
      borderStyle.solid,
      backgroundColor := S.color.bg.layerTwo,
      color := S.color.fg.default,
      resize.vertical,
      Widget.state[State].fix { state =>
        value := state.unsafeCurrentValue
      },
      onKeyDown.eas[Form.Submit, State].handle { (s, rh, e) =>
        if KeyCode.isEnter(e) && e.ctrlKey then {
          e.preventDefault()
          val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLTextAreaElement].value
          s.update(_ => targetValue) *> rh.raiseAction(Form.Submit)
        } else ZIO.unit
      },
      onKeyUp.es[State].handle { (s, e) =>
        val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLTextAreaElement].value
        s.update(_ => targetValue)
      },
      onChange.es[State].handle { (s, e) =>
        val targetValue = e.target.asInstanceOf[org.scalajs.dom.HTMLTextAreaElement].value
        s.update(_ => targetValue)
      },
      field._extra,
    )
  }

  val empty: TextArea =
    TextArea(Size.Medium, "text", autoWidth, 4.rem, true, Widget.empty)

  def apply(): TextArea = empty

  def apply(configure: TextArea => TextArea): TextArea =
    configure(empty)

  def rawForm[A: StringDecoder as dec](
      fieldName: String,
      field: TextArea = empty,
  ): SubmitFormS[State, Option[A]] =
    Form.makeWithValidation(fieldName, field) { rawValue =>
      val value: String = if field.trimInput then rawValue.trim else rawValue
      Option.when(value.nonEmpty)(value).traverse(dec.decodeSimple)
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled text area form builder. See [[TextField.form]] for the pattern.
    * Ctrl+Enter on the field raises [[Form.Submit]].
    */
  final case class form[+Value] private (
      private val _fieldName: String,
      private val _field: TextArea,
      private val _label: Label,
      private val _surroundingPadding: String,
      private val _width: String,
      private val _labelSpacing: Option[String],
      private val _decode: String => Either[String, Value],
  ) extends PForm.Deferred.Stateful[Any, Form.Submit, TextArea.State, Option[Value]] {

    override protected lazy val build: PForm[Any, Form.Submit, TextArea.State, TextArea.State, Option[Value]] =
      Form.makeWithValidation(_fieldName, _field) { rawValue =>
        val value: String = if _field.trimInput then rawValue.trim else rawValue
        Option.when(value.nonEmpty)(value).traverse(_decode)
      }.map { (fieldWidget, fieldValue) =>
        (
          div(
            padding := _surroundingPadding,
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

    def modField(f: TextArea => TextArea): form[Value] = copy(_field = f(_field))
    def modLabel(f: Label => Label): form[Value] = copy(_label = f(_label))
    def field: TextArea = _field
    def label: Label = _label

    def size(s: Size): form[Value] = modField(_.size(s))
    def height(h: String): form[Value] = modField(_.height(h))
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

    def describe(d: Widget): form[Value] = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form[Value] = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form[Value] = copy(_surroundingPadding = p)
    def width(w: String): form[Value] = copy(_width = w, _field = _field.width(100.pct))
    def labelSpacing(s: Option[String]): form[Value] = copy(_labelSpacing = s)
    def noLabelSpacing: form[Value] = labelSpacing(None)

  }
  object form {

    def apply[A: StringDecoder as dec](label: String): TextArea.form[A] =
      new TextArea.form[A](
        _fieldName = label,
        _field = TextArea.empty,
        _label = Label(label),
        _surroundingPadding = 10.px,
        _width = "fit-content",
        _labelSpacing = Label.defaultInputSpacing.some,
        _decode = dec.decodeSimple,
      )

  }

}
