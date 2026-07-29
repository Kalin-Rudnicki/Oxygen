package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Section header (W2) — pure CSS vars, no Decorator.
  *
  * Header text is a field; the component itself is the widget (no `apply(text)` builder).
  * Prefer [[text]] / [[section1]] / [[section2]] / [[section3]].
  */
final case class SectionHeader(
    private val _color: String,
    private val _indent: String,
    private val _padding: String,
    private val _tag: String,
    private val _extra: Widget,
    private val _id: Option[String],
    private val _text: String,
) extends PWidget.Deferred[Any, Nothing, Any, Nothing] {

  def color(c: String): SectionHeader = copy(_color = c)
  def primary: SectionHeader = color(S.color.primary)
  def positive: SectionHeader = color(S.color.status.positive)
  def negative: SectionHeader = color(S.color.status.negative)
  def alert: SectionHeader = color(S.color.status.alert)
  def informational: SectionHeader = color(S.color.status.informational)
  def brandPrimary1: SectionHeader = color(S.color.brand.primary1)
  def brandPrimary2: SectionHeader = color(S.color.brand.primary2)

  def extra(mods: Widget*): SectionHeader = copy(_extra = fragment(this._extra, Widget.fragment(mods)))

  /** HTML `id` on the heading element (anchors / scroll targets). */
  def withId(anchorId: String): SectionHeader = copy(_id = Some(AnchorId.normalize(anchorId)).filter(_.nonEmpty))

  def text(t: String): SectionHeader = copy(_text = t)

  override protected def build: PWidget[Any, Nothing, Any, Nothing] = {
    import oxygen.ui.web.create.{color as colorAttr, padding as paddingAttr, margin as marginAttr, id as idAttr}
    Widget.node(_tag)(
      _text,
      colorAttr := _color,
      paddingAttr := _padding,
      marginAttr("0", "0", "0", _indent),
      borderBottom(2.px, "solid", _color),
      width.fitContent,
      _id.map(x => idAttr := x).getOrElse(Widget.empty),
      _extra,
    )
  }

}
object SectionHeader {

  val empty: SectionHeader =
    SectionHeader("transparent", "0", "0", "div", Widget.empty, None, "")

  val level1: SectionHeader =
    SectionHeader(S.color.primary, S.spacing._10, css("0", S.spacing._10, S.spacing._1), "h2", Widget.empty, None, "")

  val level2: SectionHeader =
    SectionHeader(S.color.fg.moderate, S.spacing._7, css("0", S.spacing._7, S.spacing._1), "h3", Widget.empty, None, "")

  val level3: SectionHeader =
    SectionHeader(S.color.fg.minimal, S.spacing._4, css("0", S.spacing._4, S.spacing._1), "h4", Widget.empty, None, "")

  def section1(text: String, configure: SectionHeader => SectionHeader = identity): SectionHeader =
    configure(level1).text(text)

  def section2(text: String, configure: SectionHeader => SectionHeader = identity): SectionHeader =
    configure(level2).text(text)

  def section3(text: String, configure: SectionHeader => SectionHeader = identity): SectionHeader =
    configure(level3).text(text)

}
