package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Section + header composite (W2).
  */
final case class SectionWithHeader[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val header: SectionHeader,
    private val section: Section.Const,
    private val postHeaderSpacing: String,
    private val _headerText: String,
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def header(f: SectionHeader => SectionHeader): SectionWithHeader[Env, Action, StateGet, StateSet] =
    copy(header = f(header))
  def section(f: Section.Const => Section.Const): SectionWithHeader[Env, Action, StateGet, StateSet] =
    copy(section = f(section))
  def primary: SectionWithHeader[Env, Action, StateGet, StateSet] = header(_.primary)
  def positive: SectionWithHeader[Env, Action, StateGet, StateSet] = header(_.positive)
  def negative: SectionWithHeader[Env, Action, StateGet, StateSet] = header(_.negative)
  def alert: SectionWithHeader[Env, Action, StateGet, StateSet] = header(_.alert)
  def informational: SectionWithHeader[Env, Action, StateGet, StateSet] = header(_.informational)

  /** Anchor id on the outer section shell (preferred for page `#fragment` targets). */
  def withId(anchorId: String): SectionWithHeader[Env, Action, StateGet, StateSet] =
    section(_.withId(anchorId))

  def headerText(t: String): SectionWithHeader[Env, Action, StateGet, StateSet] =
    copy(_headerText = t)

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): SectionWithHeader[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = fragment(_content, Widget.fragment(children)))

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    section(
      header.extra(marginBottom := postHeaderSpacing).text(_headerText),
      _content,
    )

}
object SectionWithHeader extends WidgetTypes[SectionWithHeader] {

  val level1: SectionWithHeader.Const =
    SectionWithHeader(SectionHeader.level1, Section.level1, S.spacing._5, "", Widget.empty)

  val level2: SectionWithHeader.Const =
    SectionWithHeader(SectionHeader.level2, Section.level2, S.spacing._5, "", Widget.empty)

  val level3: SectionWithHeader.Const =
    SectionWithHeader(SectionHeader.level3, Section.level3, S.spacing._3, "", Widget.empty)

  def section1(headerText: String, configure: SectionWithHeader.Const => SectionWithHeader.Const = identity): SectionWithHeader.Const =
    configure(level1).headerText(headerText)

  def section2(headerText: String, configure: SectionWithHeader.Const => SectionWithHeader.Const = identity): SectionWithHeader.Const =
    configure(level2).headerText(headerText)

  def section3(headerText: String, configure: SectionWithHeader.Const => SectionWithHeader.Const = identity): SectionWithHeader.Const =
    configure(level3).headerText(headerText)

}
