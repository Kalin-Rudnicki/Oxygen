package oxygen.ui.web.component

import java.time.{LocalDate, LocalDateTime, LocalTime}
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W10-T08: compose [[DatePicker]] + [[TimePicker]].
  *
  * Children keep their intrinsic widths (`align-items: flex-start`) so the time
  * control does not stretch to the date panel width.
  *
  * {{{
  *   DateTimePicker.empty.h12.date(_.lg.subtle).time(_.h12)
  * }}}
  */
final case class DateTimePicker(
    private val timePicker: TimePicker,
    private val datePicker: DatePicker,
) extends PWidget.Deferred.Stateful[Any, Nothing, DateTimePicker.State] {

  def hourMode(m: TimePicker.HourMode): DateTimePicker = time(_.hourMode(m))
  def h24: DateTimePicker = hourMode(TimePicker.HourMode.H24)
  def h12: DateTimePicker = hourMode(TimePicker.HourMode.H12)

  /** Configure the embedded [[TimePicker]]. */
  def time(f: TimePicker => TimePicker): DateTimePicker = copy(timePicker = f(timePicker))

  /** Configure the embedded [[DatePicker]] (width, padding, chrome, …). */
  def date(f: DatePicker => DatePicker): DateTimePicker = copy(datePicker = f(datePicker))

  override protected def build: PWidget.Stateful[Any, Nothing, DateTimePicker.State] =
    div(
      display.inlineFlex,
      flexDirection.column,
      alignItems.flexStart, // critical: don't stretch TimePicker to date width
      gap := S.spacing._4,
      width.fitContent,
      maxWidth := 100.pct,
      datePicker.zoomOut[DateTimePicker.State](_.date),
      timePicker.zoomOut[DateTimePicker.State](_.time),
    )

}
object DateTimePicker {

  final case class State(
      date: DatePicker.State,
      time: TimePicker.State,
  ) {
    def toLocalDateTime: Option[LocalDateTime] =
      date.selected.map(d => LocalDateTime.of(d, time.toLocalTime))

    def withDate(d: LocalDate): State =
      copy(date = date.select(d))

    def withTime(t: LocalTime): State =
      copy(time = TimePicker.State.of(t))
  }
  object State {
    def empty: State = State(DatePicker.State.empty(), TimePicker.State.noon)
    def now: State = {
      val n = LocalDateTime.now()
      State(DatePicker.State.of(n.toLocalDate), TimePicker.State.of(n.toLocalTime))
    }
  }

  val empty: DateTimePicker = DateTimePicker(TimePicker.empty, DatePicker.empty)

  def apply(): DateTimePicker = empty

  def apply(timeMode: TimePicker.HourMode): DateTimePicker =
    new DateTimePicker(TimePicker(timeMode), DatePicker.empty)

  def apply(timePicker: TimePicker, datePicker: DatePicker): DateTimePicker =
    new DateTimePicker(timePicker, datePicker)

  /** Prefer [[empty]] / [[h24]] / [[h12]]. */
  def widget: DateTimePicker = empty
  def widget(timeMode: TimePicker.HourMode): DateTimePicker = apply(timeMode)
  def widget24: DateTimePicker = empty.h24
  def widget12: DateTimePicker = empty.h12

  def h24: DateTimePicker = empty.h24
  def h12: DateTimePicker = empty.h12

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled date+time form. Value is `Option[LocalDateTime]` (None until a date is picked).
    *
    * {{{
    * DateTimePicker.form("Appointment").h12.date(_.compact).zoomOut[Page](_.when)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _label: Label,
      private val _timePicker: TimePicker,
      private val _datePicker: DatePicker,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
  ) extends PForm.Deferred[Any, Nothing, DateTimePicker.State, DateTimePicker.State, Option[LocalDateTime]] {

    override protected lazy val build: PForm[Any, Nothing, DateTimePicker.State, DateTimePicker.State, Option[LocalDateTime]] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          width.fitContent,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          DateTimePicker(_timePicker, _datePicker),
        ),
      )(_.toLocalDateTime)

    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def label: Label = _label
    def time(f: TimePicker => TimePicker): form = copy(_timePicker = f(_timePicker))
    def timeMode(m: TimePicker.HourMode): form = time(_.hourMode(m))
    def h24: form = timeMode(TimePicker.HourMode.H24)
    def h12: form = timeMode(TimePicker.HourMode.H12)
    def date(f: DatePicker => DatePicker): form = copy(_datePicker = f(_datePicker))
    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form = copy(_labelSpacing = s)
    def noLabelSpacing: form = labelSpacing(None)

  }
  object form {

    def apply(label: String): DateTimePicker.form =
      new DateTimePicker.form(
        _fieldName = label,
        _label = Label(label),
        _timePicker = TimePicker.empty,
        _datePicker = DatePicker.empty,
        _surroundingPadding = 10.px,
        _labelSpacing = Some(Label.defaultInputSpacing),
      )

  }

}
