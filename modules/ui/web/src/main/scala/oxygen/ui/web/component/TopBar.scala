package oxygen.ui.web.component

import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.create.{*, given}
import zio.http.URL

final class TopBar[-Env, +Action, -StateGet, +StateSet <: StateGet] private (
    private val _left: Seq[TopBar.Item[Env, Action, StateGet, StateSet]],
    private val _right: Seq[TopBar.Item[Env, Action, StateGet, StateSet]],
) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {
  import TopBar.*

  def left[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: TopBar.Item[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left ++ addChildren, _right = _right)

  def leftItems[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: (TopBar.Item.Const => TopBar.Item[Env2, Action2, StateGet2, StateSet2])*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left ++ addChildren.map(_(TopBar.Item.empty)), _right = _right)

  def leftOpt[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Option[TopBar.Item[Env2, Action2, StateGet2, StateSet2]]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left ++ addChildren.flatten, _right = _right)

  def right[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: TopBar.Item[Env2, Action2, StateGet2, StateSet2]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left, _right = _right ++ addChildren)

  def rightItems[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: (TopBar.Item.Const => TopBar.Item[Env2, Action2, StateGet2, StateSet2])*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left, _right = _right ++ addChildren.map(_(TopBar.Item.empty)))

  def rightOpt[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
      addChildren: Option[TopBar.Item[Env2, Action2, StateGet2, StateSet2]]*,
  ): TopBar[Env2, Action2, StateGet2, StateSet2] =
    new TopBar(_left = _left, _right = _right ++ addChildren.flatten)

  override protected def build: PWidget[Env, Action, StateGet, StateSet] =
    bar(
      shrinkSection(_left*),
      growSection,
      shrinkSection(_right*),
    )

}
object TopBar extends WidgetTypes[TopBar] {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  val empty: TopBar.Const = new TopBar(_left = Nil, _right = Nil)

  val item: TopBar.Item.Const = TopBar.Item.empty

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Item
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class Item[-Env, +Action, -StateGet, +StateSet <: StateGet] private (
      private val _children: Growable[Widget.Polymorphic[Env, Action, StateGet, StateSet]],
  ) extends PWidget.Deferred[Env, Action, StateGet, StateSet] {

    private lazy val _built: Widget.Polymorphic[Env, Action, StateGet, StateSet] = TopBar.itemWidget.appendChildren(_children)
    override protected def build: PWidget[Env, Action, StateGet, StateSet] = _built

    def apply[Env2 <: Env, Action2 >: Action, StateGet2 <: StateGet, StateSet2 >: StateSet <: StateGet2](
        addChildren: PWidget[Env2, Action2, StateGet2, StateSet2]*,
    ): TopBar.Item[Env2, Action2, StateGet2, StateSet2] =
      new Item(_children = _children ++ Growable.many(addChildren))

    def index: Item[Env, Action, StateGet, StateSet] =
      this.apply(
        color := S.color.brand.primary2.light,
        fontSize := S.fontSize._7,
        fontWeight := S.fontWeight.bold,
      )

    def onClickPush(nav: RoutablePage.Navigate): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.push(nav))
    def onClickPush(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickPush(page.navigate(params))
    def onClickPush(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickPush(page)(())
    def onClickPush(url: => URL): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.push(url))
    def onClickPush(url: String): Item[Env, Action, StateGet, StateSet] = this.onClickPush(unsafeUrl(url))

    def onClickReplace(nav: RoutablePage.Navigate): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.replace(nav))
    def onClickReplace(page: RoutablePage[?])(params: page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(page.navigate(params))
    def onClickReplace(page: RoutablePage[?])(using ev: Unit <:< page.PageParams): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(page)(())
    def onClickReplace(url: => URL): Item[Env, Action, StateGet, StateSet] = this.apply(onClick.replace(url))
    def onClickReplace(url: String): Item[Env, Action, StateGet, StateSet] = this.onClickReplace(unsafeUrl(url))

  }
  object Item extends WidgetTypes[TopBar.Item] {

    val empty: TopBar.Item.Const = Item(Growable.empty)

  }

  private def unsafeUrl(url: String): URL = URL.decode(url) match
    case Right(url)  => url
    case Left(error) => throw new RuntimeException(s"Invalid URL [$url]: $error")

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private val bar: Node =
    div(
      width := 100.pct,
      height := 100.pct,
      display.flex,
      backgroundColor := S.color.brand.primary1,
    )

  private val shrinkSection: Node =
    div(
      height := 100.pct,
      flexGrow := 0,
      flexShrink := 1,
    )

  private val growSection: Node =
    div(
      height := 100.pct,
      flexGrow := 1,
      flexShrink := 0,
    )

  private val itemWidget: Node =
    div(
      height := 100.pct,
      cursor.pointer,
      userSelect.none,
      display.inlineFlex,
      justifyContent.center,
      alignItems.center,
      padding := "0 1rem",
      fontSize := OxygenStyleVars.fontSize._5,
      backgroundColor.dynamic.hover := S.color.brand.primary1.dark.getColorValue.darken(15),
      backgroundColor.dynamic.hoverActive := S.color.brand.primary1.dark.getColorValue.darken(30),
    )

}
