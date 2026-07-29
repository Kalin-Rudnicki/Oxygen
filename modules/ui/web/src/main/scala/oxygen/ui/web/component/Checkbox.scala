package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.ZIO

/**
  * W10-T02: checkbox builder — boolean state, optional label, form helper.
  */
final case class Checkbox(
    private val label: String,
    private val intent: Intent,
    private val disabled: Boolean,
) {

  def label(t: String): Checkbox = copy(label = t)
  def intent(i: Intent): Checkbox = copy(intent = i)
  def primary: Checkbox = intent(Intent.Primary)
  def positive: Checkbox = intent(Intent.Success)
  def negative: Checkbox = intent(Intent.Danger)
  def disabled(d: Boolean): Checkbox = copy(disabled = d)

  def boolean: WidgetS[Boolean] =
    Widget.state[Boolean].fix { st =>
      val on = st.renderTimeValue
      val accent = Checkbox.roleColor(intent)
      div(
        display.inlineFlex,
        alignItems.center,
        gap := S.spacing._3,
        maxWidth := 100.pct,
        cursor := (if disabled then "not-allowed" else "pointer"),
        opacity := (if disabled then "0.55" else "1"),
        // Whole control is chrome — double-click must not select the label.
        userSelect.none,
        onClick := (if disabled then ZIO.unit else st.update(!_)),
        // Fixed-size box: always mount the check glyph (hidden when off) so
        // toggle never changes layout or shifts the label.
        div(
          width := Checkbox.boxPx.px,
          height := Checkbox.boxPx.px,
          minWidth := Checkbox.boxPx.px,
          maxWidth := Checkbox.boxPx.px,
          minHeight := Checkbox.boxPx.px,
          maxHeight := Checkbox.boxPx.px,
          boxSizing.borderBox,
          flexShrink := "0",
          overflow.hidden,
          borderRadius := S.borderRadius._2,
          border(2.px, "solid", if on then accent else S.color.fg.subtle),
          backgroundColor := (if on then accent else S.color.bg.layerOne.toString),
          display.flex,
          alignItems.center,
          justifyContent.center,
          color := S.color.primary.on,
          userSelect.none,
          span(
            display.inlineFlex,
            // keep slot occupied; only visibility changes
            if on then visibility.visible else visibility.hidden,
            Icon.check.size(Checkbox.iconPx),
          ),
        ),
        // label (overflow-safe)
        span(
          O.WrapText,
          color := S.color.fg.default,
          fontSize := S.fontSize._3,
          lineHeight := "1.25",
          userSelect.none,
          label,
        ),
      )
    }

}
object Checkbox {

  /** Outer control size (border-box, includes 2px border). */
  private val boxPx: Int = 18

  /** Check glyph — fits inside content area without pushing layout. */
  private val iconPx: Int = 12

  val empty: Checkbox = Checkbox("", Intent.Primary, disabled = false)

  def apply(label: String = ""): Checkbox = empty.label(label)

  def boolean(label: String = "", configure: Checkbox => Checkbox = identity): WidgetS[Boolean] =
    configure(Checkbox(label)).boolean

  private def roleColor(intent: Intent): String =
    intent match {
      case Intent.Primary => S.color.primary.standard
      case Intent.Accent  => S.color.highlight.accent.standard
      case Intent.Neutral => S.color.fg.moderate
      case Intent.Success => S.color.status.positive.standard
      case Intent.Warning => S.color.status.alert.standard
      case Intent.Danger  => S.color.status.negative.standard
      case Intent.Info    => S.color.status.informational.standard
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Boolean checkbox form builder.
    *
    * {{{
    * Checkbox.form("Accept terms").primary.zoomOut[MyForm](_.accepted)
    * Checkbox.form("VIP").modCheckbox(_.negative)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _checkbox: Checkbox,
  ) extends PForm.Deferred[Any, Nothing, Boolean, Boolean, Boolean] {

    override protected lazy val build: PForm[Any, Nothing, Boolean, Boolean, Boolean] =
      Form.makeWith(List(_fieldName), _checkbox.boolean)(identity)

    def modCheckbox(f: Checkbox => Checkbox): form = copy(_checkbox = f(_checkbox))
    def checkbox: Checkbox = _checkbox

    def label(t: String): form = modCheckbox(_.label(t))
    def intent(i: Intent): form = modCheckbox(_.intent(i))
    def primary: form = modCheckbox(_.primary)
    def positive: form = modCheckbox(_.positive)
    def negative: form = modCheckbox(_.negative)
    def disabled(d: Boolean): form = modCheckbox(_.disabled(d))

  }
  object form {

    def apply(label: String = ""): Checkbox.form =
      new Checkbox.form(
        _fieldName = if label.nonEmpty then label else "checkbox",
        _checkbox = Checkbox(label),
      )

  }

}
