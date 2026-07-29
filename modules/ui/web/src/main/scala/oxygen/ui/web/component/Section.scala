package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Layout section chrome (W2) — pure CSS vars, no Decorator.
  *
  * Anchors (W7-T04): use [[withId]] so `PageURL#fragment` / `Window.scroll.toId` can target the section.
  * {{{
  *   Section.level1.withId("overview")(h2("Overview"), body)
  *   Section.level1.withId(AnchorId.slug("My section"))(...)
  * }}}
  */
final case class Section[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _margin: String,
    private val _padding: String,
    private val _borderRadius: String,
    private val _backgroundColor: String,
    private val _id: Option[String],
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def withMargin(m: String): Section[Env, Action, StateGet, StateSet] = copy(_margin = m)
  def withPadding(p: String): Section[Env, Action, StateGet, StateSet] = copy(_padding = p)
  def withBorderRadius(r: String): Section[Env, Action, StateGet, StateSet] = copy(_borderRadius = r)
  def withBackgroundColor(c: String): Section[Env, Action, StateGet, StateSet] = copy(_backgroundColor = c)

  /** Set HTML `id` for deep links / in-page scroll (leading `#` optional). */
  def withId(anchorId: String): Section[Env, Action, StateGet, StateSet] =
    copy(_id = Some(AnchorId.normalize(anchorId)).filter(_.nonEmpty))

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): Section[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = fragment(_content, Widget.fragment(children)))

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    import oxygen.ui.web.create.{margin as marginAttr, padding as paddingAttr, borderRadius as borderRadiusAttr, backgroundColor as backgroundColorAttr, id as idAttr}
    div(
      marginAttr := _margin,
      paddingAttr := _padding,
      borderRadiusAttr := _borderRadius,
      backgroundColorAttr := _backgroundColor,
      _id.map(i => idAttr := i).getOrElse(Widget.empty),
      _content,
    )
  }

}
object Section extends WidgetTypes[Section] {

  val empty: Section.Const =
    Section("0", "0", "0", "transparent", None, Widget.empty)

  val level1: Section.Const =
    Section(
      _margin = css(S.spacing._2, S.spacing._14),
      _padding = css(S.spacing._5, S.spacing._10),
      _borderRadius = S.borderRadius._8,
      _backgroundColor = S.color.bg.layerOne,
      _id = None,
      _content = Widget.empty,
    )

  val level2: Section.Const =
    Section(
      _margin = css(S.spacing._2, S.spacing._0),
      _padding = css(S.spacing._5, S.spacing._10),
      _borderRadius = S.borderRadius._5,
      _backgroundColor = S.color.bg.layerTwo,
      _id = None,
      _content = Widget.empty,
    )

  val level3: Section.Const =
    Section(
      _margin = css(S.spacing._1, S.spacing._0),
      _padding = css(S.spacing._3, S.spacing._6),
      _borderRadius = S.borderRadius._3,
      _backgroundColor = S.color.bg.layerThree,
      _id = None,
      _content = Widget.empty,
    )

  def section1(configure: Section.Const => Section.Const = identity): Section.Const =
    configure(level1)

  def section2(configure: Section.Const => Section.Const = identity): Section.Const =
    configure(level2)

  def section3(configure: Section.Const => Section.Const = identity): Section.Const =
    configure(level3)

}
