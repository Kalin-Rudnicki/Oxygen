package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Deferred data table (W2-T10).
  *
  * Builds `div.table-shell > table.table` so corner radius + outer edge live on the shell,
  * while `border-collapse` stays on `<table>`. [[apply]] appends to the **table** (thead/tbody/…).
  * [[shell]] appends widgets/attrs to the outer shell div.
  *
  * {{{
  * Table.basic(
  *   Table.header("Name", "Status"),
  *   Table.body(
  *     Table.row("alpha", "ok"),
  *     Table.row("beta", "pending"),
  *   ),
  * )
  *
  * Table.basic.radius(S.borderRadius._5).shell(boxShadow := "…")(…)
  * }}}
  */
final case class Table[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val header: Table.Defaults,
    private val cell: Table.Defaults,
    private val borders: Table.Borders,
    /** Internal grid (row rules / cell grid). */
    private val borderColor: String,
    private val borderWidth: String,
    private val _radius: String,
    private val _outerBorder: Boolean,
    /** Shell perimeter — independent of [[borderColor]] / [[borderWidth]]. */
    private val _outerBorderColor: String,
    private val _outerBorderWidth: String,
    private val _shell: Growable[PWidget[Env, Action, StateGet, StateSet]],
    private val _children: Growable[PWidget[Env, Action, StateGet, StateSet]],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def leftAlignHeaders: Table[Env, Action, StateGet, StateSet] = copy(header = header.copy(alignment = "left"))
  def centerAlignHeaders: Table[Env, Action, StateGet, StateSet] = copy(header = header.copy(alignment = "center"))
  def rightAlignHeaders: Table[Env, Action, StateGet, StateSet] = copy(header = header.copy(alignment = "right"))
  def leftAlignCells: Table[Env, Action, StateGet, StateSet] = copy(cell = cell.copy(alignment = "left"))
  def centerAlignCells: Table[Env, Action, StateGet, StateSet] = copy(cell = cell.copy(alignment = "center"))
  def rightAlignCells: Table[Env, Action, StateGet, StateSet] = copy(cell = cell.copy(alignment = "right"))

  def padHeaders(topBottom: String, leftRight: String): Table[Env, Action, StateGet, StateSet] =
    copy(header = header.copy(padding = StandardProps.Padding(topBottom, leftRight)))
  def padCells(topBottom: String, leftRight: String): Table[Env, Action, StateGet, StateSet] =
    copy(cell = cell.copy(padding = StandardProps.Padding(topBottom, leftRight)))

  def styleHeaders(bg: String, fg: String): Table[Env, Action, StateGet, StateSet] = copy(header = header.copy(fg = fg, bg = bg))
  def styleCells(bg: String, fg: String): Table[Env, Action, StateGet, StateSet] = copy(cell = cell.copy(fg = fg, bg = bg))

  def primaryHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.primary.standard, S.color.primary.on)
  def positiveHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.status.positive.standard, S.color.status.positive.on)
  def negativeHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.status.negative.standard, S.color.status.negative.on)
  def alertHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.status.alert.standard, S.color.status.alert.on)
  def informationalHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.status.informational.standard, S.color.status.informational.on)
  def brandPrimary1Headers: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.brand.primary1, S.color.fg.default)
  def brandPrimary2Headers: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.brand.primary2, S.color.fg.default)
  def defaultHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.bg.default, S.color.fg.default)
  def baseHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.bg.base, S.color.fg.default)
  def layerOneHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.bg.layerOne, S.color.fg.default)
  def layerTwoHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.bg.layerTwo, S.color.fg.default)
  def layerThreeHeaders: Table[Env, Action, StateGet, StateSet] = styleHeaders(S.color.bg.layerThree, S.color.fg.default)

  def primaryCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.primary.standard, S.color.primary.on)
  def positiveCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.status.positive.standard, S.color.status.positive.on)
  def negativeCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.status.negative.standard, S.color.status.negative.on)
  def alertCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.status.alert.standard, S.color.status.alert.on)
  def informationalCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.status.informational.standard, S.color.status.informational.on)
  def brandPrimary1Cells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.brand.primary1, S.color.fg.default)
  def brandPrimary2Cells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.brand.primary2, S.color.fg.default)
  def defaultCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.bg.default, S.color.fg.default)
  def baseCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.bg.base, S.color.fg.default)
  def layerOneCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.bg.layerOne, S.color.fg.default)
  def layerTwoCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.bg.layerTwo, S.color.fg.default)
  def layerThreeCells: Table[Env, Action, StateGet, StateSet] = styleCells(S.color.bg.layerThree, S.color.fg.default)

  def rowBorders: Table[Env, Action, StateGet, StateSet] = copy(borders = Table.Borders.Rows)
  def cellBorders: Table[Env, Action, StateGet, StateSet] = copy(borders = Table.Borders.Cells)

  /** Internal grid stroke only (row rules / cell grid) — does not change the shell perimeter. */
  def borders(color: String, width: String): Table[Env, Action, StateGet, StateSet] =
    copy(borderColor = color, borderWidth = width)

  /** Outer shell corner radius (applied on the clip wrapper, not the collapsed table). */
  def radius(r: String): Table[Env, Action, StateGet, StateSet] = copy(_radius = r)
  def noRadius: Table[Env, Action, StateGet, StateSet] = radius("0")
  def radiusSm: Table[Env, Action, StateGet, StateSet] = radius(S.borderRadius._2)
  def radiusMd: Table[Env, Action, StateGet, StateSet] = radius(S.borderRadius._3)
  def radiusLg: Table[Env, Action, StateGet, StateSet] = radius(S.borderRadius._5)
  def radiusXl: Table[Env, Action, StateGet, StateSet] = radius(S.borderRadius._7)

  /**
    * Draw the perimeter on the **shell** (follows radius). Default on.
    * Color/width are independent of [[borders]] so you can e.g.
    * `.borders("blue", 3.px).outerBorder("red", 3.px)`.
    * When on, cell outer edges are suppressed so they don't double the shell stroke.
    */
  def outerBorder: Table[Env, Action, StateGet, StateSet] = copy(_outerBorder = true)
  def noOuterBorder: Table[Env, Action, StateGet, StateSet] = copy(_outerBorder = false)
  def outerBorder(enabled: Boolean): Table[Env, Action, StateGet, StateSet] = copy(_outerBorder = enabled)
  def outerBorder(color: String, width: String): Table[Env, Action, StateGet, StateSet] =
    copy(_outerBorder = true, _outerBorderColor = color, _outerBorderWidth = width)

  /** Soft, card-like data table (recommended default). */
  def quiet: Table[Env, Action, StateGet, StateSet] =
    styleHeaders(S.color.bg.layerTwo, S.color.fg.moderate)
      .styleCells(S.color.bg.layerOne, S.color.fg.default)
      .padHeaders(S.spacing._3, S.spacing._4)
      .padCells(S.spacing._3, S.spacing._4)
      .borders(S.color.bg.layerThree, 1.px)
      .leftAlignHeaders
      .leftAlignCells

  /** Strong brand header for emphasis / marketing tables. */
  def branded: Table[Env, Action, StateGet, StateSet] =
    primaryHeaders
      .styleCells(S.color.bg.layerOne, S.color.fg.default)
      .padHeaders(S.spacing._3, S.spacing._4)
      .padCells(S.spacing._3, S.spacing._4)
      .borders(S.color.bg.layerThree, 1.px)
      .leftAlignHeaders
      .leftAlignCells

  /**
    * Append widgets/attrs to the **outer shell** div (class, style, shadow, data-*, …).
    * Table body content still goes through [[apply]].
    */
  def shell[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      mods: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): Table[Env2, Action2, StateGet2, StateSet2] =
    if mods.isEmpty then this
    else copy(_shell = _shell ++ Growable.many(mods))

  /** Append table body children (thead / tbody / tr / …). */
  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
  ): Table[Env2, Action2, StateGet2, StateSet2] =
    if addChildren.isEmpty then this
    else copy(_children = _children ++ Growable.many(addChildren))

  def appendChildren[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Growable[PWidget[Env2, Action2, StateGet2, StateSet2]],
  ): Table[Env2, Action2, StateGet2, StateSet2] =
    copy(_children = _children ++ addChildren)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    // Shell: radius + optional perimeter. Collapse stays on <table>.
    // OuterBorder class drives CSS that zeros cell outer edges (avoids double stroke).
    val shellChrome: Widget =
      if _outerBorder then
        fragment(
          O.Table.Shell.OuterBorder,
          borderStyle.solid,
          oxygen.ui.web.create.borderWidth := _outerBorderWidth,
          oxygen.ui.web.create.borderColor := _outerBorderColor,
        )
      else
        fragment(
          borderStyle.none,
          oxygen.ui.web.create.borderWidth := 0.px,
        )

    div(
      O.Table.Shell,
      shellChrome,
      width := 100.pct,
      boxSizing.borderBox,
      borderRadius := _radius,
      overflow.hidden,
    ).appendChildren(_shell).apply(
      table(
        O.Table,
        borders match {
          case Table.Borders.Rows  => O.Table.RowBorders
          case Table.Borders.Cells => O.Table.CellBorders
        },
        O.Table.defaultBorderColor := borderColor,
        O.Table.defaultBorderWidth := borderWidth,
        O.Table.HeaderCellVars.fgColor := header.fg,
        O.Table.HeaderCellVars.bgColor := header.bg,
        O.Table.HeaderCellVars.padding := header.padding.show,
        O.Table.HeaderCellVars.alignment := header.alignment,
        O.Table.CellCellVars.fgColor := cell.fg,
        O.Table.CellCellVars.bgColor := cell.bg,
        O.Table.CellCellVars.padding := cell.padding.show,
        O.Table.CellCellVars.alignment := cell.alignment,
        width := 100.pct,
        borderCollapse.collapse,
        margin := 0.px,
      ).appendChildren(_children),
    )
  }

}
object Table extends WidgetTypes[Table] {

  final case class Defaults(
      fg: String,
      bg: String,
      padding: StandardProps.Padding,
      alignment: String,
  )

  enum Borders {
    case Rows
    case Cells
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Factories
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** Quiet defaults (soft header, row rules, rounded shell). */
  val empty: Table.Const =
    Table(
      header = Defaults(
        fg = S.color.fg.moderate,
        bg = S.color.bg.layerTwo,
        padding = StandardProps.Padding(S.spacing._3, S.spacing._4),
        alignment = "left",
      ),
      cell = Defaults(
        fg = S.color.fg.default,
        bg = S.color.bg.layerOne,
        padding = StandardProps.Padding(S.spacing._3, S.spacing._4),
        alignment = "left",
      ),
      borders = Borders.Rows,
      borderColor = S.color.bg.layerThree,
      borderWidth = 1.px,
      _radius = S.borderRadius._3,
      _outerBorder = true,
      _outerBorderColor = S.color.bg.layerThree,
      _outerBorderWidth = 1.px,
      _shell = Growable.empty,
      _children = Growable.empty,
    )

  def apply(): Table.Const = empty

  def apply(configure: Table.Const => Table.Const): Table.Const =
    configure(empty)

  /** Alias for [[empty]] — `Table.basic(thead…, tbody…)` stays the usual call site. */
  lazy val basic: Table.Const = empty

  def basic(configure: Table.Const => Table.Const): Table.Const =
    configure(empty)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Content helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /** `thead` with one header row of label strings. */
  def header(labels: String*): Node =
    thead(tr(labels.map(l => th(l))*))

  /** `thead` with one header row of arbitrary cell widgets (wrapped in `th`). */
  def headerCells(cells: Widget*): Node =
    thead(tr(cells.map(c => th(c))*))

  /** Body `tr` of string cells. */
  def row(cells: String*): Node =
    tr(cells.map(c => td(c))*)

  /** Body `tr` of arbitrary widgets (wrapped in `td`). */
  def rowCells(cells: Widget*): Node =
    tr(cells.map(c => td(c))*)

  /** `tbody` of row widgets (`tr` / [[row]] / …). */
  def body(rows: Widget*): Node =
    tbody(rows*)

  /** Map items → body rows. */
  def bodyRows[A](items: Iterable[A])(cells: A => Seq[Widget]): Node =
    tbody(items.iterator.map(a => rowCells(cells(a)*)).toSeq*)

  /**
    * Full quiet table from header labels + string rows.
    *
    * {{{
    * Table.of("Name", "Role")(
    *   Seq(Seq("Ada", "Admin"), Seq("Bob", "User")),
    * )
    * }}}
    */
  def of(headers: String*)(rows: Seq[Seq[String]]): Table.Const =
    empty(
      header(headers*),
      body(rows.map(r => row(r*))*),
    )

  /**
    * Full quiet table from headers + mapped items.
    *
    * {{{
    * Table.ofData("Id", "Label")(items)(i => Seq(Widget.text(i.id), Widget.text(i.label)))
    * }}}
    */
  def ofData[A](headers: String*)(items: Iterable[A])(cells: A => Seq[Widget]): Table.Const =
    empty(
      header(headers*),
      bodyRows(items)(cells),
    )

}
