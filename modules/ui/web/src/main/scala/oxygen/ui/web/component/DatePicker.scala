package oxygen.ui.web.component

import java.time.{LocalDate, YearMonth}
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * W10-T06: custom date picker (not native input). Pure calendar model + builder UI.
  *
  * Header (flex):
  * {{{
  *   [year« month‹]  |  Month (grow)  |  Year  |  [month› year»]
  *   shrink              grow            shrink     shrink
  * }}}
  * Week starts on Sunday. Click year → year grid for fast jumps.
  *
  * Style via fluent builders:
  * {{{
  *   DatePicker.empty.width(18.rem).compact.borderRadius(S.borderRadius._2)
  * }}}
  */
final case class DatePicker(
    private val _width: String,
    private val _maxWidth: String,
    private val _minWidth: String,
    private val _padding: String,
    private val _backgroundColor: String,
    private val _borderColor: String,
    private val _borderWidth: String,
    private val _borderRadius: String,
    private val _extra: Widget,
    // TODO (KR) : optional min/max date
) extends PWidget.Deferred.Stateful[Any, Nothing, DatePicker.State] {

  def width(w: String): DatePicker = copy(_width = w)
  def maxWidth(w: String): DatePicker = copy(_maxWidth = w)
  def minWidth(w: String): DatePicker = copy(_minWidth = w)
  def padding(p: String): DatePicker = copy(_padding = p)
  def pad(p: String): DatePicker = padding(p)
  def backgroundColor(c: String): DatePicker = copy(_backgroundColor = c)
  def borderColor(c: String): DatePicker = copy(_borderColor = c)
  def borderWidth(w: String): DatePicker = copy(_borderWidth = w)
  def borderRadius(r: String): DatePicker = copy(_borderRadius = r)

  /** Append arbitrary node modifiers (classes, attrs, CSS). */
  def extra(mods: Widget*): DatePicker = copy(_extra = fragment(_extra, Widget.fragment(mods)))

  /** Narrow calendar (~16rem). */
  def sm: DatePicker = width(16.rem).minWidth(14.rem)

  /** Default calendar (~20rem). */
  def md: DatePicker = width(20.rem).minWidth(16.rem)

  /** Wide calendar (~24rem). */
  def lg: DatePicker = width(24.rem).minWidth(18.rem)

  /** Full available width (still capped by [[maxWidth]]). */
  def fullWidth: DatePicker = width(100.pct)

  /** Tight panel padding. */
  def compact: DatePicker = padding(S.spacing._2)

  /** Default panel padding. */
  def comfortable: DatePicker = padding(S.spacing._3)

  /** Roomier panel padding. */
  def spacious: DatePicker = padding(S.spacing._4)

  /** Flat: no border chrome. */
  def borderless: DatePicker = borderWidth(0.px).borderColor(S.color.bg.transparent)

  /** Subtle surface (layerTwo fill). */
  def subtle: DatePicker = backgroundColor(S.color.bg.layerTwo)

  override protected def build: PWidget.Stateful[Any, Nothing, DatePicker.State] =
    DatePicker.render(this)

}
object DatePicker {

  enum View {
    case Calendar
    case YearGrid
  }

  final case class State(
      cursor: YearMonth,
      selected: Option[LocalDate],
      view: View = View.Calendar,
  ) {
    def select(d: LocalDate): State = copy(selected = Some(d), cursor = YearMonth.from(d), view = View.Calendar)
    def clear: State = copy(selected = None)
    def prevMonth: State = copy(cursor = cursor.minusMonths(1))
    def nextMonth: State = copy(cursor = cursor.plusMonths(1))
    def prevYear: State = copy(cursor = cursor.minusYears(1))
    def nextYear: State = copy(cursor = cursor.plusYears(1))
    def prevYearPage: State = copy(cursor = cursor.minusYears(12))
    def nextYearPage: State = copy(cursor = cursor.plusYears(12))
    def showYearGrid: State = copy(view = View.YearGrid)
    def showCalendar: State = copy(view = View.Calendar)
    def pickYear(y: Int): State = copy(cursor = cursor.withYear(y), view = View.Calendar)
  }
  object State {
    def today: State = {
      val t = LocalDate.now()
      State(YearMonth.from(t), Some(t))
    }
    def empty(cursor: YearMonth = YearMonth.now()): State =
      State(cursor, None)
    def of(d: LocalDate): State =
      State(YearMonth.from(d), Some(d))
  }

  /**
    * Days for a Sunday-first month grid (padding cells as None). Pure — unit tested.
    * Java DayOfWeek: Mon=1 … Sun=7 → lead pads = `getValue % 7` (Sun→0, Mon→1, … Sat→6).
    */
  def monthCells(ym: YearMonth): Vector[Option[LocalDate]] = {
    val first = ym.atDay(1)
    val lead = first.getDayOfWeek.getValue % 7
    val daysInMonth = ym.lengthOfMonth()
    val cells = scala.collection.mutable.ArrayBuffer.empty[Option[LocalDate]]
    var i = 0
    while i < lead do {
      cells += None
      i += 1
    }
    var d = 1
    while d <= daysInMonth do {
      cells += Some(ym.atDay(d))
      d += 1
    }
    while cells.size % 7 != 0 do cells += None
    cells.toVector
  }

  /** 12-year page anchored to a multiple of 12. Pure — unit tested. */
  def yearPage(year: Int): Vector[Int] = {
    val start = Math.floorDiv(year, 12) * 12
    (0 until 12).map(start + _).toVector
  }

  private val dowLabels: Seq[String] = Seq("Su", "Mo", "Tu", "We", "Th", "Fr", "Sa")

  /** Square nav button size (grid tracks match this). */
  private val navCell: String = 2.rem

  /** Default panel width — fits 7 day cells + padding; month label uses min-width in header. */
  private val defaultWidth: String = 22.rem

  val empty: DatePicker = DatePicker(
    _width = defaultWidth,
    _maxWidth = 100.pct,
    _minWidth = 0.px,
    _padding = S.spacing._3,
    _backgroundColor = S.color.bg.layerOne,
    _borderColor = S.color.bg.layerThree,
    _borderWidth = 1.px,
    _borderRadius = S.borderRadius._4,
    _extra = Widget.empty,
  )

  def apply(): DatePicker = empty

  /** Alias for [[empty]] (pre-Deferred call sites). */
  def widget: DatePicker = empty

  private def render(cfg: DatePicker): WidgetS[State] =
    Widget.state[State].fix { st =>
      val s = st.renderTimeValue
      div(
        display.inlineBlock,
        width := cfg._width,
        maxWidth := cfg._maxWidth,
        minWidth := cfg._minWidth,
        boxSizing.borderBox,
        padding := cfg._padding,
        backgroundColor := cfg._backgroundColor,
        border(cfg._borderWidth, "solid", cfg._borderColor),
        borderRadius := cfg._borderRadius,
        userSelect.none,
        cfg._extra,
        s.view match {
          case View.Calendar => calendarBody(st, s)
          case View.YearGrid => yearGridBody(st, s)
        },
      )
    }

  /** Fixed-size nav cell — never flex-shrinks, never grows. */
  private def navBtn(icon: Icon, title: String, action: zio.UIO[Unit]): Widget =
    button(
      O.Button,
      width := navCell,
      height := navCell,
      minWidth := navCell,
      maxWidth := navCell,
      padding := 0.px,
      display.inlineFlex,
      alignItems.center,
      justifyContent.center,
      borderStyle := "none",
      borderWidth := 0.px,
      borderRadius := S.borderRadius._2,
      backgroundColor := S.color.bg.transparent,
      color := S.color.fg.moderate,
      cursor.pointer,
      Widget.raw.htmlAttr("title", title),
      onClick := action,
      icon.sm,
    )

  private def calendarBody(st: WidgetState[State], s: State): Widget = {
    val monthLabel: String = s.cursor.getMonth.toString.toLowerCase.capitalize
    val yearLabel: String = s.cursor.getYear.toString
    val cells: Vector[Option[LocalDate]] = monthCells(s.cursor) // this should probably be calculated on the State
    fragment(
      // flex top-bar: left arrows (shrink) · month (grow) · year (shrink) · right arrows (shrink)
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._1,
        marginBottom := S.spacing._3,
        width := 100.pct,
        minWidth := 0.px,
        // left arrows
        div(
          display.flex,
          alignItems.center,
          flexShrink := "0",
          gap := S.spacing._1,
          navBtn(Icon.chevronsLeft, "Previous year", st.update(_.prevYear)),
          navBtn(Icon.chevronLeft, "Previous month", st.update(_.prevMonth)),
        ),
        // month label — grows; min-width ≈ "September" so short months do not collapse the bar
        span(
          display.inlineBlock,
          flexGrow := "1",
          flexShrink := "0",
          minWidth := 12.ch,
          textAlign.center,
          fontWeight := "600",
          fontSize := S.fontSize._3,
          color := S.color.fg.default,
          whiteSpace.nowrap,
          overflow.hidden,
          textOverflow := "ellipsis",
          monthLabel,
        ),
        // year — shrink, clickable for year grid
        button(
          O.Button,
          flexShrink := "0",
          fontWeight := "600",
          fontSize := S.fontSize._3,
          color := S.color.primary.standard,
          backgroundColor := S.color.bg.transparent,
          borderStyle := "none",
          borderWidth := 0.px,
          padding := css(S.spacing._1, S.spacing._2),
          borderRadius := S.borderRadius._2,
          cursor.pointer,
          whiteSpace.nowrap,
          Widget.raw.htmlAttr("title", "Pick year"),
          onClick := st.update(_.showYearGrid),
          yearLabel,
        ),
        // right arrows
        div(
          display.flex,
          alignItems.center,
          flexShrink := "0",
          gap := S.spacing._1,
          navBtn(Icon.chevronRight, "Next month", st.update(_.nextMonth)),
          navBtn(Icon.chevronsRight, "Next year", st.update(_.nextYear)),
        ),
      ),
      // DOW
      div(
        display.grid,
        Widget.raw.css("grid-template-columns", "repeat(7, 1fr)"),
        gap := S.spacing._1,
        marginBottom := S.spacing._1,
        Widget.foreach(dowLabels) { lab =>
          span(
            textAlign.center,
            fontSize := S.fontSize._1,
            color := S.color.fg.subtle,
            lab,
          )
        },
      ),
      // days
      div(
        display.grid,
        Widget.raw.css("grid-template-columns", "repeat(7, 1fr)"),
        gap := S.spacing._1,
        Widget.foreach(cells) {
          case None =>
            div(height := 2.rem)
          case Some(day) =>
            val sel = s.selected.contains(day)
            button(
              O.Button,
              height := 2.rem,
              width := 100.pct,
              borderStyle := "solid",
              borderWidth := 1.px,
              borderColor := (if sel then S.color.primary.standard else S.color.bg.transparent),
              borderRadius := S.borderRadius._2,
              backgroundColor := (if sel then S.color.primary.standard else S.color.bg.transparent),
              color := (if sel then S.color.primary.on else S.color.fg.default),
              fontSize := S.fontSize._2,
              cursor.pointer,
              onClick := st.update(_.select(day)),
              day.getDayOfMonth.toString,
            )
        },
      ),
    )
  }

  private def yearGridBody(st: WidgetState[State], s: State): Widget = {
    val years = yearPage(s.cursor.getYear)
    val rangeLabel = s"${years.head} – ${years.last}"
    fragment(
      div(
        display.grid,
        Widget.raw.css("grid-template-columns", s"$navCell 1fr $navCell"),
        alignItems.center,
        columnGap := S.spacing._1,
        marginBottom := S.spacing._3,
        width := 100.pct,
        navBtn(Icon.chevronsLeft, "Previous years", st.update(_.prevYearPage)),
        button(
          O.Button,
          fontWeight := "600",
          color := S.color.fg.default,
          backgroundColor := S.color.bg.transparent,
          borderStyle := "none",
          borderWidth := 0.px,
          padding := css(S.spacing._1, S.spacing._2),
          borderRadius := S.borderRadius._2,
          cursor.pointer,
          fontSize := S.fontSize._3,
          textAlign.center,
          Widget.raw.htmlAttr("title", "Back to calendar"),
          onClick := st.update(_.showCalendar),
          rangeLabel,
        ),
        navBtn(Icon.chevronsRight, "Next years", st.update(_.nextYearPage)),
      ),
      div(
        display.grid,
        Widget.raw.css("grid-template-columns", "repeat(3, 1fr)"),
        gap := S.spacing._2,
        Widget.foreach(years) { y =>
          val on = s.cursor.getYear == y
          button(
            O.Button,
            height := 2.5.rem,
            width := 100.pct,
            borderStyle := "solid",
            borderWidth := 1.px,
            borderColor := (if on then S.color.primary.standard else S.color.bg.layerThree),
            borderRadius := S.borderRadius._2,
            backgroundColor := (if on then S.color.primary.standard else S.color.bg.transparent),
            color := (if on then S.color.primary.on else S.color.fg.default),
            fontSize := S.fontSize._3,
            fontWeight := (if on then "600" else "400"),
            cursor.pointer,
            onClick := st.update(_.pickYear(y)),
            y.toString,
          )
        },
      ),
    )
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Form (labeled, composable Deferred builder)
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Labeled date picker form. Value is `selected: Option[LocalDate]`.
    *
    * {{{
    * DatePicker.form("Start date").describe("Optional").picker(_.lg.subtle).zoomOut[Page](_.start)
    * }}}
    */
  final case class form private (
      private val _fieldName: String,
      private val _label: Label,
      private val _surroundingPadding: String,
      private val _labelSpacing: Option[String],
      private val _picker: DatePicker,
  ) extends PForm.Deferred[Any, Nothing, DatePicker.State, DatePicker.State, Option[LocalDate]] {

    override protected lazy val build: PForm[Any, Nothing, DatePicker.State, DatePicker.State, Option[LocalDate]] =
      Form.makeWith(
        _fieldName,
        div(
          padding := _surroundingPadding,
          width.fitContent,
          maxWidth := 100.pct,
          boxSizing.borderBox,
          _label,
          Spacing.vertical.opt(_labelSpacing),
          _picker,
        ),
      )(_.selected)

    def modLabel(f: Label => Label): form = copy(_label = f(_label))
    def label: Label = _label
    def describe(d: Widget): form = modLabel(_.describe(d))
    def labelMod(mods: Widget*): form = modLabel(_.mod(mods*))
    def surroundingPadding(p: String): form = copy(_surroundingPadding = p)
    def labelSpacing(s: Option[String]): form = copy(_labelSpacing = s)
    def noLabelSpacing: form = labelSpacing(None)

    /** Configure embedded [[DatePicker]] (width, padding, chrome, …). Prefer this over CSS-named helpers to avoid attr name clashes. */
    def picker(f: DatePicker => DatePicker): form = copy(_picker = f(_picker))
    def compact: form = picker(_.compact)
    def comfortable: form = picker(_.comfortable)
    def spacious: form = picker(_.spacious)
    def sm: form = picker(_.sm)
    def md: form = picker(_.md)
    def lg: form = picker(_.lg)
    def fullWidth: form = picker(_.fullWidth)
    def subtle: form = picker(_.subtle)
    def borderless: form = picker(_.borderless)

  }
  object form {

    def apply(label: String): DatePicker.form =
      new DatePicker.form(
        _fieldName = label,
        _label = Label(label),
        _surroundingPadding = 10.px,
        _labelSpacing = Some(Label.defaultInputSpacing),
        _picker = DatePicker.empty,
      )

  }

}
