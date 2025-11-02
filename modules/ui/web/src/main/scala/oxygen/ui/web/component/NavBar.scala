package oxygen.ui.web.component

import oxygen.ui.web.{NonRoutablePage, RoutablePage}
import oxygen.ui.web.create.{*, given}

final class NavBar[-Env, +Action, -StateGet, +StateSet <: StateGet](
    props: NavBar.Props,
    leftItems: Seq[NavBar.NavBarElemBuilder => Widget.Polymorphic[Env, Action, StateGet, StateSet]],
    rightItems: Seq[NavBar.NavBarElemBuilder => Widget.Polymorphic[Env, Action, StateGet, StateSet]],
) {

  private val navBarBase: Node =
    div(
      OxygenStyleSheet.NavBar,
      minHeight := props.navBarHeight,
      backgroundColor := props.navBarColor,
    )

  private val sectionBase: Node =
    div(OxygenStyleSheet.NavBar.Section)

  private val expandSectionBase: Node =
    sectionBase(OxygenStyleSheet.NavBar.Section.Expand)

  private val shrinkSectionBase: Node =
    sectionBase(OxygenStyleSheet.NavBar.Section.Shrink)

  private val leftElemBuilder: NavBar.NavBarElemBuilder = NavBar.NavBarElemBuilder(NavBar.NavBarElemBuilder.Side.Left, props)
  private val rightElemBuilder: NavBar.NavBarElemBuilder = NavBar.NavBarElemBuilder(NavBar.NavBarElemBuilder.Side.Right, props)

  // TODO (KR) : make `NavBarElemBuilder` return a wrapped widget which contains info about what type of elem the nav elem is
  //           : ex: if `_.simplePush` is used, or if its a menu item
  //           : this will be helpful for being able to display things differently on web/mobile
  val widget: Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    navBarBase(
      shrinkSectionBase(Widget.foreach(leftItems) { _(leftElemBuilder) }),
      expandSectionBase,
      shrinkSectionBase(Widget.foreach(rightItems) { _(rightElemBuilder) }),
    )

}
object NavBar {

  type Const = NavBar.Stateless[Any, Nothing]
  type Stateless[-Env, +Action] = NavBar[Env, Action, Any, Nothing]
  type Stateful[-Env, +Action, State] = NavBar[Env, Action, State, State]

  final case class Props(
      navBarHeight: String = 40.px,
      navBarColor: String = OxygenStyleVars.color.brand.primary1,
  )

  def make[Env, Action, StateGet, StateSet <: StateGet](props: NavBar.Props = NavBar.Props())(
      leftItems: (NavBarElemBuilder => Widget.Polymorphic[Env, Action, StateGet, StateSet])*,
  )(
      rightItems: (NavBarElemBuilder => Widget.Polymorphic[Env, Action, StateGet, StateSet])*,
  ): NavBar[Env, Action, StateGet, StateSet] =
    NavBar(props, leftItems, rightItems)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class NavBarElemBuilder(
      // TODO (KR) : use this for menus
      @scala.annotation.unused side: NavBarElemBuilder.Side,
      props: NavBar.Props,
  ) {

    private val base: Node =
      div(
        minHeight := props.navBarHeight,
        OxygenStyleSheet.NavBar.Section.Elem,
        backgroundColor.dynamic := props.navBarColor,
        backgroundColor.dynamic.hover := CSSColor.eval(props.navBarColor).darken(15.0),
        backgroundColor.dynamic.active := CSSColor.eval(props.navBarColor).darken(30.0),
        borderRadius.dynamic.active := 10.px,
      )

    // TODO (KR) : return a special repr that says what kind of nav bar item was created
    def apply[Env, Action, StateGet, StateSet <: StateGet](content: Widget.Polymorphic[Env, Action, StateGet, StateSet]*): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
      base(content*)

    def simplePush(label: String, other: Widget*)(page: RoutablePage[?])(params: page.Params): Widget =
      if (other.isEmpty) apply(label, onClick.push(page)(params))
      else apply(label, Widget.fragment(other), onClick.push(page)(params))

    def simpleReplace(label: String, other: Widget*)(page: RoutablePage[?])(params: page.Params): Widget =
      if (other.isEmpty) apply(label, onClick.replace(page)(params))
      else apply(label, Widget.fragment(other), onClick.replace(page)(params))

    def simpleRender[Env](label: String, other: Widget*)(page: NonRoutablePage[Env])(params: page.Params): WidgetE[Env] =
      if (other.isEmpty) apply(label, onClick.render(page)(params))
      else apply(label, Widget.fragment(other), onClick.render(page)(params))

    // TODO (KR) : simple menu

    // TODO (KR) : complex menu

  }
  object NavBarElemBuilder {

    enum Side { case Left, Right }

  }

}
