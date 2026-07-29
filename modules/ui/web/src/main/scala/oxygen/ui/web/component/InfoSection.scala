package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Callout / info strip (W2). Uses pure vars; backHighlight uses subtle role token.
  */
final case class InfoSection[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _color: String,
    private val highlightBg: Option[String],
    private val _content: Widget.Polymorphic[Env, Action, StateGet, StateSet],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def color(c: String): InfoSection[Env, Action, StateGet, StateSet] = copy(_color = c)
  def primary: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.primary)
  def positive: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.status.positive)
  def negative: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.status.negative)
  def alert: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.status.alert)
  def informational: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.status.informational)
  def brandPrimary1: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.brand.primary1)
  def brandPrimary2: InfoSection[Env, Action, StateGet, StateSet] = color(S.color.brand.primary2)

  def backHighlight: InfoSection[Env, Action, StateGet, StateSet] =
    copy(highlightBg = Some(S.color.bg.layerOne.toString))

  def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      children: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2]*,
  ): InfoSection[Env2, Action2, StateGet2, StateSet2] =
    copy(_content = fragment(_content, Widget.fragment(children)))

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    p(
      margin(S.spacing._2, S.spacing._0),
      padding(S.spacing._2, S.spacing._4),
      borderLeft(2.px, "solid", _color),
      Widget.foreach(highlightBg) { bg =>
        backgroundColor := bg
      },
      _content,
    )

}
object InfoSection extends WidgetTypes[InfoSection] {

  val empty: InfoSection.Const =
    InfoSection(S.color.status.informational, None, Widget.empty)

  def apply(): InfoSection.Const = empty

  def apply(configure: InfoSection.Const => InfoSection.Const): InfoSection.Const =
    configure(empty)

  lazy val default: InfoSection.Const = empty

}
