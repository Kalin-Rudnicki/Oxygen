package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * W10-T09: custom color picker — hex text + swatch presets; optional alpha 0–1.
  * Can drive a seed preview (caller binds state).
  */
object ColorPicker {

  final case class State(hex: String, alpha: Double = 1.0) {
    def clampedAlpha: Double = alpha.max(0.0).min(1.0)
    def normalizedHex: String = ColorPicker.normalizeHex(hex).getOrElse(hex)
    def withHex(h: String): State = copy(hex = h)
    def withAlpha(a: Double): State = copy(alpha = a)
  }
  object State {
    val black: State = State("#000000", 1.0)
    val white: State = State("#ffffff", 1.0)
    def of(hex: String, alpha: Double = 1.0): State = State(hex, alpha)
  }

  /** Accept #RGB or #RRGGBB → canonical #RRGGBB lowercase. */
  def normalizeHex(raw: String): Option[String] = {
    val t = raw.trim.toLowerCase
    val body = if t.startsWith("#") then t.drop(1) else t
    body match {
      case b if b.matches("[0-9a-f]{6}") => Some("#" + b)
      case b if b.matches("[0-9a-f]{3}") =>
        Some("#" + b.flatMap(c => s"$c$c"))
      case _ => None
    }
  }

  def isValidHex(raw: String): Boolean = normalizeHex(raw).isDefined

  private val presets: Seq[String] =
    Seq("#3b82f6", "#14b8a6", "#f97316", "#8b5cf6", "#ef4444", "#f59e0b", "#111111", "#ffffff", "#64748b")

  def widget: WidgetS[State] =
    Widget.state[State].fix { st =>
      val s = st.renderTimeValue
      val preview = normalizeHex(s.hex).getOrElse("#888888")
      div(
        display.flex,
        flexDirection.column,
        gap := S.spacing._3,
        padding := S.spacing._3,
        backgroundColor := S.color.bg.layerOne,
        border(1.px, "solid", S.color.bg.layerThree),
        borderRadius := S.borderRadius._4,
        maxWidth := 320.px,
        // swatch + hex
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._3,
          div(
            width := 40.px,
            height := 40.px,
            borderRadius := S.borderRadius._3,
            border(1.px, "solid", S.color.fg.subtle),
            backgroundColor := preview,
            opacity := s.clampedAlpha.toString,
          ),
          input(
            `type`.text,
            value := s.hex,
            onInput.e.handle { e =>
              val v = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value
              st.update(_.withHex(v))
            },
            onKeyDown.e.handle { e =>
              if KeyCode.isEnter(e) then {
                e.preventDefault()
                val el = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement]
                val normalized = normalizeHex(el.value).getOrElse(el.value)
                st.update(_.withHex(normalized)) *> ZIO.succeed { el.blur(); () }
              } else ZIO.unit
            },
            width := 12.ch,
            padding := S.spacing._2,
            border(1.px, "solid", S.color.fg.subtle),
            borderRadius := S.borderRadius._2,
            backgroundColor := S.color.bg.layerTwo,
            color := S.color.fg.default,
          ),
          span(
            fontSize := S.fontSize._1,
            color := (if isValidHex(s.hex) then S.color.status.positive.standard else S.color.status.negative.standard),
            if isValidHex(s.hex) then "valid" else "invalid",
          ),
        ),
        // presets
        div(
          display.flex,
          flexWrap.wrap,
          gap := S.spacing._2,
          Widget.foreach(presets) { p =>
            button(
              O.Button,
              width := 28.px,
              height := 28.px,
              borderRadius := S.borderRadius._2,
              border(1.px, "solid", S.color.fg.subtle),
              backgroundColor := p,
              cursor.pointer,
              onClick := st.update(_.withHex(p)),
            )
          },
        ),
        // alpha
        div(
          display.flex,
          alignItems.center,
          gap := S.spacing._2,
          span("Alpha", fontSize := S.fontSize._2, color := S.color.fg.moderate),
          Widget.foreach(List(0.0, 0.25, 0.5, 0.75, 1.0)) { a =>
            Button(f"$a%.2f")
              .extraSmall
              .subtle
              .content(onClick := st.update(_.withAlpha(a)))
          },
        ),
      )
    }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled color picker form. Value is full [[State]] (hex + alpha).
    *
    * {{{
    * ColorPicker.form("Brand").describe("Primary seed").zoomOut[Page](_.brand)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _label: Label,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred[Any, Nothing, ColorPicker.State, ColorPicker.State, ColorPicker.State] {

    override protected lazy val build: PForm[Any, Nothing, ColorPicker.State, ColorPicker.State, ColorPicker.State] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          width.fitContent,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          ColorPicker.widget,
        ),
      )(identity)

    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def label: Label = _label
    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form = copy(_labelSpacing = s)
    def noLabelSpacing: form = labelSpacing(None)

  }
  object form {

    def apply(label: String): ColorPicker.form =
      new ColorPicker.form(
        _fieldName = label,
        _label = Label(label),
        _surroundingPadding = 10.px,
        _labelSpacing = Some(Label.defaultInputSpacing),
      )

  }

}
