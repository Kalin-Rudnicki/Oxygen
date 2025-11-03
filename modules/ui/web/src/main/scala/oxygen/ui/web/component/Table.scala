package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.create.{*, given}

object Table {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Props
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Defaults(
      _defaultFGColor: String,
      _defaultBGColor: String,
      _defaultPadding: StandardProps.Padding,
      _defaultAlignment: String,
  )

  enum Borders {
    case Rows
    case Cells
  }

  final case class Props(
      _header: Defaults,
      _cell: Defaults,
      _borders: Borders,
      _borderColor: String,
      _borderWidth: String,
  )
  object Props {

    private[Table] lazy val initial: Props =
      Props(
        _header = Defaults(
          _defaultFGColor = "transparent",
          _defaultBGColor = "transparent",
          _defaultPadding = StandardProps.Padding(S.spacing._1, S.spacing._5),
          _defaultAlignment = "center",
        ),
        _cell = Defaults(
          _defaultFGColor = "transparent",
          _defaultBGColor = "transparent",
          _defaultPadding = StandardProps.Padding(S.spacing._2px, S.spacing._3),
          _defaultAlignment = "left",
        ),
        _borders = Borders.Rows,
        _borderColor = S.color.fg.default,
        _borderWidth = 1.px,
      )

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Decorator
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class Decorator private[Table] (private[Table] val decorator: GenericDecorator[Props]) extends DecoratorBuilder {

    def name: String = decorator.show

    private[Table] lazy val computed: Props = decorator.decorate(Props.initial)

    def >>(that: Decorator): Decorator = Decorator { this.decorator >> that.decorator }
    def <<(that: Decorator): Decorator = Decorator { this.decorator << that.decorator }

  }
  object Decorator extends DecoratorBuilder {

    override private[Table] val decorator: GenericDecorator[Props] = GenericDecorator.empty

    def all[S[_]: SeqRead](decorators: S[Decorator]): Decorator =
      Decorator { decorators.newIterator.foldLeft(GenericDecorator.empty) { (a, b) => a >> b.decorator } }

    def all(decorators: Decorator*): Decorator =
      all(decorators)

    val identity: Decorator = Decorator { GenericDecorator.empty }
    val empty: Decorator = identity

    lazy val defaultStyling: Decorator = empty.primaryHeaders.baseCells

  }

  trait DecoratorBuilder {
    private[Table] val decorator: GenericDecorator[Props]

    private def wrap(dec: GenericDecorator[Props]): Decorator = Decorator { decorator >> dec }
    private def wrap(name: String)(f: Props => Props): Decorator = wrap { GenericDecorator(name)(f) }

    private def modHeaders(name: String)(f: Defaults => Defaults): Decorator = wrap(name) { p => p.copy(_header = f(p._header)) }
    private def modCells(name: String)(f: Defaults => Defaults): Decorator = wrap(name) { p => p.copy(_cell = f(p._cell)) }

    /////// Alignment ///////////////////////////////////////////////////////////////

    final lazy val leftAlignHeaders: Decorator = modHeaders("LeftAlignHeaders") { _.copy(_defaultAlignment = "left") }
    final lazy val centerAlignHeaders: Decorator = modHeaders("CenterAlignHeaders") { _.copy(_defaultAlignment = "center") }
    final lazy val rightAlignHeaders: Decorator = modHeaders("RightAlignHeaders") { _.copy(_defaultAlignment = "right") }

    final lazy val leftAlignCells: Decorator = modCells("LeftAlignCells") { _.copy(_defaultAlignment = "left") }
    final lazy val centerAlignCells: Decorator = modCells("CenterAlignCells") { _.copy(_defaultAlignment = "center") }
    final lazy val rightAlignCells: Decorator = modCells("RightAlignCells") { _.copy(_defaultAlignment = "right") }

    /////// Padding ///////////////////////////////////////////////////////////////

    final def padHeaders(topBottom: String, leftRight: String): Decorator = modHeaders("custom(padHeaders)") { _.copy(_defaultPadding = StandardProps.Padding(topBottom, leftRight)) }
    final def padCells(topBottom: String, leftRight: String): Decorator = modCells("custom(padCells)") { _.copy(_defaultPadding = StandardProps.Padding(topBottom, leftRight)) }

    /////// Padding ///////////////////////////////////////////////////////////////

    private def makeHeaderStyles(name: String, bgColor: String, fgColor: String): Decorator =
      modHeaders(s"HeaderColors($name)") { _.copy(_defaultFGColor = fgColor, _defaultBGColor = bgColor) }
    private def makeCellStyles(name: String, bgColor: String, fgColor: String): Decorator =
      modCells(s"CellColors($name)") { _.copy(_defaultFGColor = fgColor, _defaultBGColor = bgColor) }

    final lazy val primaryHeaders: Decorator = makeHeaderStyles("Primary", S.color.primary, S.color.fg.inverse)
    final lazy val positiveHeaders: Decorator = makeHeaderStyles("Positive", S.color.status.positive, S.color.fg.inverse)
    final lazy val negativeHeaders: Decorator = makeHeaderStyles("Negative", S.color.status.negative, S.color.fg.inverse)
    final lazy val alertHeaders: Decorator = makeHeaderStyles("Alert", S.color.status.alert, S.color.fg.inverse)
    final lazy val informationalHeaders: Decorator = makeHeaderStyles("Informational", S.color.status.informational, S.color.fg.inverse)
    final lazy val brandPrimary1Headers: Decorator = makeHeaderStyles("BrandPrimary1", S.color.brand.primary1, S.color.fg.default)
    final lazy val brandPrimary2Headers: Decorator = makeHeaderStyles("BrandPrimary2", S.color.brand.primary2, S.color.fg.default)
    final lazy val defaultHeaders: Decorator = makeHeaderStyles("Default", S.color.bg.default, S.color.fg.default)
    final lazy val baseHeaders: Decorator = makeHeaderStyles("Base", S.color.bg.base, S.color.fg.default)
    final lazy val layerOneHeaders: Decorator = makeHeaderStyles("Layer1", S.color.bg.layerOne, S.color.fg.default)
    final lazy val layerTwoHeaders: Decorator = makeHeaderStyles("Layer2", S.color.bg.layerTwo, S.color.fg.default)
    final lazy val layerThreeHeaders: Decorator = makeHeaderStyles("Layer3", S.color.bg.layerThree, S.color.fg.default)

    final def styleHeaders(bgColor: String, fgColor: String): Decorator =
      makeHeaderStyles("custom", bgColor, fgColor)

    final lazy val primaryCells: Decorator = makeCellStyles("Primary", S.color.primary, S.color.fg.inverse)
    final lazy val positiveCells: Decorator = makeCellStyles("Positive", S.color.status.positive, S.color.fg.inverse)
    final lazy val negativeCells: Decorator = makeCellStyles("Negative", S.color.status.negative, S.color.fg.inverse)
    final lazy val alertCells: Decorator = makeCellStyles("Alert", S.color.status.alert, S.color.fg.inverse)
    final lazy val informationalCells: Decorator = makeCellStyles("Informational", S.color.status.informational, S.color.fg.inverse)
    final lazy val brandPrimary1Cells: Decorator = makeCellStyles("BrandPrimary1", S.color.brand.primary1, S.color.fg.default)
    final lazy val brandPrimary2Cells: Decorator = makeCellStyles("BrandPrimary2", S.color.brand.primary2, S.color.fg.default)
    final lazy val defaultCells: Decorator = makeCellStyles("Default", S.color.bg.default, S.color.fg.default)
    final lazy val baseCells: Decorator = makeCellStyles("Base", S.color.bg.base, S.color.fg.default)
    final lazy val layerOneCells: Decorator = makeCellStyles("Layer1", S.color.bg.layerOne, S.color.fg.default)
    final lazy val layerTwoCells: Decorator = makeCellStyles("Layer2", S.color.bg.layerTwo, S.color.fg.default)
    final lazy val layerThreeCells: Decorator = makeCellStyles("Layer3", S.color.bg.layerThree, S.color.fg.default)

    final def styleCells(bgColor: String, fgColor: String): Decorator =
      makeCellStyles("custom", bgColor, fgColor)

    /////// Borders ///////////////////////////////////////////////////////////////

    final lazy val rowBorders: Decorator = wrap("Borders(Rows)") { _.copy(_borders = Borders.Rows) }
    final lazy val cellBorders: Decorator = wrap("Borders(Cells)") { _.copy(_borders = Borders.Cells) }

    final def borders(color: String, width: String): Decorator = wrap("custom(borders)") { _.copy(_borderWidth = width, _borderColor = color) }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Widget
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def basic(decorator: Decorator): Node = {
    val props: Props = decorator.computed

    table(
      O.Table,
      props._borders match {
        case Borders.Rows  => O.Table.RowBorders
        case Borders.Cells => O.Table.CellBorders
      },
      O.Table.defaultBorderColor := props._borderColor,
      O.Table.defaultBorderWidth := props._borderWidth,
      O.Table.HeaderCellVars.fgColor := props._header._defaultFGColor,
      O.Table.HeaderCellVars.bgColor := props._header._defaultBGColor,
      O.Table.HeaderCellVars.padding := props._header._defaultPadding.show,
      O.Table.HeaderCellVars.alignment := props._header._defaultAlignment,
      O.Table.CellCellVars.fgColor := props._cell._defaultFGColor,
      O.Table.CellCellVars.bgColor := props._cell._defaultBGColor,
      O.Table.CellCellVars.padding := props._cell._defaultPadding.show,
      O.Table.CellCellVars.alignment := props._cell._defaultAlignment,
    )
  }

  def basic(decorator: Decorator => Decorator): Node =
    basic(decorator(Decorator.defaultStyling))

  lazy val basic: Node =
    basic(Decorator.defaultStyling)

}
