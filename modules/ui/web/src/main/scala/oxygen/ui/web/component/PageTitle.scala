package oxygen.ui.web.component

import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}

/**
  * Page-level title block for content under shell chrome (TopBar / layout.HolyGrail).
  *
  * Global `h1` no longer carries a large left offset; use this so every page gets
  * consistent top/side spacing and optional details under the heading.
  *
  * {{{
  * PageTitle("Public IdP clients")
  * PageTitle("Account").details("Manage billing and linked identities")
  * }}}
  */
final case class PageTitle[-Env, +Action, -StateGet, +StateSet <: StateGet](
    private val _title: Widget.Polymorphic[Env, Action, StateGet, StateSet],
    private val _details: Option[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
    private val _margin: String,
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

  def title[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      t: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): PageTitle[Env2, Action2, StateGet2, StateSet2] =
    copy(_title = t)
  def title(t: String): PageTitle[Env, Action, StateGet, StateSet] = title(span(t))

  def details[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      d: Widget.Polymorphic[Env2, Action2, StateGet2, StateSet2],
  ): PageTitle[Env2, Action2, StateGet2, StateSet2] =
    copy(_details = Some(d))
  def details(d: String): PageTitle[Env, Action, StateGet, StateSet] = details(span(d))
  def noDetails: PageTitle[Env, Action, StateGet, StateSet] = copy(_details = None)

  /** Outer margin (CSS shorthand). Defaults match Section.level1 horizontal inset. */
  def margin(m: String): PageTitle[Env, Action, StateGet, StateSet] = copy(_margin = m)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] = {
    // Qualify CSS attrs that clash with builder methods (`margin`).
    import oxygen.ui.web.create.{margin as marginAttr, padding as paddingAttr}
    div(
      marginAttr := _margin,
      h1(
        marginAttr := "0",
        paddingAttr := "0",
        fontSize := "1.75rem",
        fontWeight := S.fontWeight.bold,
        color := S.color.fg.default,
        lineHeight := "1.25",
        _title,
      ),
      _details.fold(Widget.empty) { d =>
        div(
          marginTop := S.spacing._2,
          fontSize := S.fontSize._3,
          color := S.color.fg.moderate,
          lineHeight := "1.45",
          d,
        )
      },
    )
  }

}
object PageTitle extends WidgetTypes[PageTitle] {

  /**
    * Default margin (top / right / bottom / left).
    * More air under the TopBar and before the first section; stronger left inset
    * so the title sits clearly inside the content column (not flush under chrome).
    */
  private val defaultMargin: String =
    css(
      s"calc(${S.spacing._5} * 2)", // top  (~2× prior _5)
      S.spacing._14, // right (match Section.level1 horizontal inset)
      s"calc(${S.spacing._3} * 2)", // bottom (~2× prior _3)
      s"calc(${S.spacing._14} * 3)", // left  (~3× prior _14)
    )

  val empty: PageTitle.Const =
    PageTitle(Widget.empty, None, defaultMargin)

  def apply(title: String): PageTitle.Const =
    empty.title(title)

  def apply[Env, Action, StateGet, StateSet <: StateGet](
      title: Widget.Polymorphic[Env, Action, StateGet, StateSet],
  ): PageTitle[Env, Action, StateGet, StateSet] =
    empty.title(title)

  def apply(title: String, details: String): PageTitle.Const =
    empty.title(title).details(details)

}
