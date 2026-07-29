package oxygen.ui.web.component

import java.time.LocalTime
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.*

/**
  * W10-T07: custom time picker (not native).
  *
  * Compact dual spinboxes: typeable hour / minute with ▲/▼ steppers.
  * [[HourMode.H24]] — hours 00–23 (default).
  * [[HourMode.H12]] — hours 1–12 + AM/PM toggle (state still stores 0–23).
  *
  * Width is intrinsic (`inline-flex` + `width: fit-content`) so embedding under a
  * stretched column (e.g. DateTimePicker) cannot blow the control out full-width.
  */
final case class TimePicker(
    private val mode: TimePicker.HourMode,
) extends PWidget.Deferred[Any, Nothing, TimePicker.State, TimePicker.State] {

  def hourMode(m: TimePicker.HourMode): TimePicker = copy(mode = m)
  def h24: TimePicker = hourMode(TimePicker.HourMode.H24)
  def h12: TimePicker = hourMode(TimePicker.HourMode.H12)

  override protected def build: PWidget[Any, Nothing, TimePicker.State, TimePicker.State] =
    TimePicker.render(mode)

}
object TimePicker {

  enum HourMode {
    case H24
    case H12
  }

  final case class State(hour: Int, minute: Int) {
    def clamped: State = copy(hour = hour.max(0).min(23), minute = minute.max(0).min(59))
    def toLocalTime: LocalTime = {
      val c = clamped
      LocalTime.of(c.hour, c.minute)
    }
    def withHour(h: Int): State = copy(hour = h).clamped
    def withMinute(m: Int): State = copy(minute = m).clamped

    def bumpHour(delta: Int): State =
      copy(hour = Math.floorMod(hour + delta, 24), minute = minute).clamped

    def bumpMinute(delta: Int): State =
      copy(hour = hour, minute = Math.floorMod(minute + delta, 60)).clamped

    /** Toggle AM/PM while keeping clock-face hour (e.g. 3 PM ↔ 3 AM). */
    def toggleAmPm: State =
      if hour < 12 then copy(hour = hour + 12).clamped
      else copy(hour = hour - 12).clamped

    def isPm: Boolean = hour >= 12

    /** 1–12 clock face for 12-hour UI. */
    def hour12: Int = {
      val h = hour % 12
      if h == 0 then 12 else h
    }

    /** Set from 1–12 face + am/pm. */
    def withHour12(h12: Int, pm: Boolean): State = {
      val face = h12.max(1).min(12)
      val h24 =
        if face == 12 then (if pm then 12 else 0)
        else if pm then face + 12
        else face
      withHour(h24)
    }
  }
  object State {
    def of(t: LocalTime): State = State(t.getHour, t.getMinute)
    def noon: State = State(12, 0)
    def midnight: State = State(0, 0)

    /** Pure parse "H:mm" / "HH:mm" (24h) → Option[State]. */
    def parse(raw: String): Option[State] = {
      val parts = raw.trim.split(':')
      if parts.length != 2 then None
      else
        try {
          val h = parts(0).toInt
          val m = parts(1).toInt
          if h >= 0 && h <= 23 && m >= 0 && m <= 59 then Some(State(h, m)) else None
        } catch {
          case _: NumberFormatException => None
        }
    }

    def format(s: State, mode: HourMode = HourMode.H24): String = {
      val c = s.clamped
      mode match {
        case HourMode.H24 => f"${c.hour}%02d:${c.minute}%02d"
        case HourMode.H12 =>
          val suffix = if c.isPm then "PM" else "AM"
          f"${c.hour12}%d:${c.minute}%02d $suffix"
      }
    }
  }

  /** Minute step for ▲/▼ (typing still accepts any 0–59). */
  private val minuteStep: Int = 5

  val empty: TimePicker = TimePicker(HourMode.H24)

  def apply(): TimePicker = empty

  def apply(mode: HourMode): TimePicker = new TimePicker(mode)

  /** Convenience aliases (prefer [[empty]] / [[h24]] / [[h12]]). */
  def h24: TimePicker = empty.h24
  def h12: TimePicker = empty.h12

  private def render(mode: HourMode): WidgetS[State] =
    Widget.state[State].fix { st =>
      val s = st.renderTimeValue.clamped
      div(
        display.inlineFlex,
        alignItems.center,
        gap := S.spacing._2,
        width.fitContent,
        maxWidth := 100.pct,
        boxSizing.borderBox,
        padding := css(S.spacing._2, S.spacing._3),
        backgroundColor := S.color.bg.layerOne,
        border(1.px, "solid", S.color.bg.layerThree),
        borderRadius := S.borderRadius._3,
        flexShrink := "0",
        // decorative clock
        span(
          color := S.color.fg.subtle,
          display.inlineFlex,
          alignItems.center,
          flexShrink := "0",
          Icon.clock.sm,
        ),
        mode match {
          case HourMode.H24 =>
            spinbox(
              displayValue = f"${s.hour}%02d",
              ariaLabel = "Hour",
              max = 23,
              min = 0,
              onInc = st.update(_.bumpHour(1)),
              onDec = st.update(_.bumpHour(-1)),
              onCommit = n => st.update(_.withHour(n)),
            )
          case HourMode.H12 =>
            spinbox(
              displayValue = f"${s.hour12}%02d",
              ariaLabel = "Hour",
              max = 12,
              min = 1,
              onInc = st.update(_.bumpHour(1)),
              onDec = st.update(_.bumpHour(-1)),
              onCommit = n => st.update(cur => cur.withHour12(n, cur.isPm)),
            )
        },
        span(
          fontSize := S.fontSize._5,
          fontWeight := "600",
          color := S.color.fg.moderate,
          userSelect.none,
          flexShrink := "0",
          ":",
        ),
        spinbox(
          displayValue = f"${s.minute}%02d",
          ariaLabel = "Minute",
          max = 59,
          min = 0,
          onInc = st.update(_.bumpMinute(minuteStep)),
          onDec = st.update(_.bumpMinute(-minuteStep)),
          onCommit = n => st.update(_.withMinute(n)),
        ),
        mode match {
          case HourMode.H24 => Widget.empty
          case HourMode.H12 =>
            val wid: String = 6.ch

            // Fixed box so AM ↔ PM never reflows; wide enough that letters do not clip.
            button(
              O.Button,
              flexShrink := "0",
              width := wid,
              minWidth := wid,
              maxWidth := wid,
              padding := css(S.spacing._1, S.spacing._1),
              textAlign.center,
              fontSize := S.fontSize._2,
              fontWeight := "600",
              Widget.raw.css("font-variant-numeric", "tabular-nums"),
              letterSpacing := "0.02em",
              border(1.px, "solid", S.color.bg.layerThree),
              borderRadius := S.borderRadius._2,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
              cursor.pointer,
              boxSizing.borderBox,
              overflow.hidden,
              whiteSpace.nowrap,
              Widget.raw.htmlAttr("title", "Toggle AM/PM"),
              onClick := st.update(_.toggleAmPm),
              if s.isPm then "PM" else "AM",
            )
        },
      )
    }

  /**
    * Vertical spinbox: ▲ / editable digits / ▼.
    * Commit on Enter, blur, and change.
    */
  private def spinbox(
      displayValue: String,
      ariaLabel: String,
      max: Int,
      min: Int,
      onInc: UIO[Unit],
      onDec: UIO[Unit],
      onCommit: Int => UIO[Unit],
  ): Widget = {
    def commitRaw(raw: String): UIO[Unit] = {
      val t = raw.trim
      if t.isEmpty then ZIO.unit
      else
        try {
          val n = t.toInt
          if n >= min && n <= max then onCommit(n) else ZIO.unit
        } catch {
          case _: NumberFormatException => ZIO.unit
        }
    }

    val wid: String = 4.ch
    div(
      display.flex,
      flexDirection.column,
      alignItems.center,
      justifyContent.center,
      gap := S.spacing._1,
      flexShrink := "0",
      Button().iconOnly(Icon.chevronUp).extraSmall.subtle.content(onClick := onInc, fontSize := S.fontSize._5, width := wid),
      input(
        `type`.text,
        Widget.raw.htmlAttr("inputmode", "numeric"),
        Widget.raw.htmlAttr("aria-label", ariaLabel),
        Widget.raw.htmlAttr("maxlength", "2"),
        value := displayValue,
        width := wid,
        minWidth := wid,
        maxWidth := wid,
        padding := css(S.spacing._1, S.spacing._1),
        textAlign.center,
        fontSize := S.fontSize._5,
        fontWeight := "600",
        Widget.raw.css("font-variant-numeric", "tabular-nums"),
        fontFamily := S.fontStyle.default,
        border(1.px, "solid", S.color.bg.layerThree),
        borderRadius := S.borderRadius._2,
        backgroundColor := S.color.bg.layerTwo,
        color := S.color.fg.default,
        outline := "none",
        boxSizing.borderBox,
        onKeyDown.e.handle { e =>
          val el = e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement]
          if KeyCode.isEnter(e) then {
            e.preventDefault()
            commitRaw(el.value) *> ZIO.succeed { el.blur(); () }
          } else if KeyCode.Up.matches(e) then {
            e.preventDefault()
            onInc
          } else if KeyCode.Down.matches(e) then {
            e.preventDefault()
            onDec
          } else ZIO.unit
        },
        onBlur.e.handle { e =>
          commitRaw(e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value)
        },
        onChange.e.handle { e =>
          commitRaw(e.target.asInstanceOf[org.scalajs.dom.HTMLInputElement].value)
        },
      ),
      Button().iconOnly(Icon.chevronDown).extraSmall.subtle.content(onClick := onDec, fontSize := S.fontSize._5, width := wid),
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled time picker form. Value is [[LocalTime]] from state.
    *
    * {{{
    * TimePicker.form("Due time").h12.zoomOut[Page](_.due)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _label: Label,
      private val _mode: HourMode,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred.Stateful[Any, Nothing, TimePicker.State, LocalTime] {

    override protected lazy val build: PForm[Any, Nothing, TimePicker.State, TimePicker.State, LocalTime] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          width.fitContent,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          TimePicker(_mode),
        ),
      )(_.toLocalTime)

    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def label: Label = _label
    def mode(m: HourMode): form = copy(_mode = m)
    def h24: form = mode(HourMode.H24)
    def h12: form = mode(HourMode.H12)
    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form = copy(_labelSpacing = s)
    def noLabelSpacing: form = labelSpacing(None)

  }
  object form {

    def apply(label: String): TimePicker.form =
      new TimePicker.form(
        _fieldName = label,
        _label = Label(label),
        _mode = HourMode.H24,
        _surroundingPadding = 10.px,
        _labelSpacing = Some(Label.defaultInputSpacing),
      )

  }

}
