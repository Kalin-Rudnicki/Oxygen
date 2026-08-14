package oxygen.example.ui.page.showcase

import oxygen.example.ui.page.showcase.pages.*
import oxygen.example.ui.page as P
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.defaults.{ComponentsPage, StylesPage}
import oxygen.ui.web.layout.*
import zio.*

/**
  * Shared HolyGrail chrome for showcase demos (EX-T03).
  * Nav links match EX-T02 consolidated paths; current path is highlighted.
  *
  * Page objects live in [[oxygen.example.ui.page.showcase.pages]].
  */
object ShowcaseLayout {

  private def navItem(label: String, page: RoutablePage.NoParams[?], currentPath: Seq[String]): Widget = {
    val selected = page.path == currentPath
    SideBar.basicItem(selected = selected)(
      label,
      onClick.push(page.nav()),
      color := (if selected then S.color.primary.on else S.color.fg.default),
      width := 100.pct,
    )
  }

  def topBar: TopBar.Const =
    // Height left unset — HolyGrail.topHeight owns the row so TopBar does not fight it.
    TopBar.empty
      .brand
      .left(
        TopBar.item("Oxygen Showcase").onClickPush(ShowcaseHubPage.nav()),
        TopBar.item("App").onClickPush(P.index.IndexPage.nav()),
        TopBar.item("Styles").onClickPush(StylesPage.nav()),
        TopBar.item("Components").onClickPush(ComponentsPage.nav()),
      )

  def sideNav(currentPath: Seq[String]): SideBar.Const =
    SideBar().surface.middle(
      navItem("Hub", ShowcaseHubPage, currentPath),
      navItem("Shell", ShellPage, currentPath),
      navItem("Sign-in", SignInPage, currentPath),
      navItem("Register", RegisterPage, currentPath),
      navItem("Dashboard", DashboardPage, currentPath),
      navItem("Theme", ThemePage, currentPath),
      navItem("Icons", IconsPage, currentPath),
      navItem("Form validation", FormValidationPage, currentPath),
      navItem("Page lock", FormLockPage, currentPath),
      navItem("Choices", FormChoicesPage, currentPath),
      navItem("Date/time", FormDateTimePage, currentPath),
      navItem("Color field", FormColorPage, currentPath),
      navItem("Upload", FormUploadPage, currentPath),
      navItem("All form fields", FormAllPage, currentPath),
      navItem("Modal", ModalPage, currentPath),
      navItem("Drawer", DrawerPage, currentPath),
      navItem("Dropdown menu", DropdownMenuPage, currentPath),
      navItem("Tooltips", TooltipPage, currentPath),
      navItem("Table", TablePage, currentPath),
      navItem("Feed", FeedPage, currentPath),
      navItem("Sortable", SortablePage, currentPath),
      navItem("Tabs", TabsPage, currentPath),
      navItem("Wizard steps", WizardPage, currentPath),
      navItem("Busy", BusyPage, currentPath),
      navItem("Messages", MessagesPage, currentPath),
      navItem("Anchors", AnchorsPage, currentPath),
      navItem("Grid", GridPage, currentPath),
      navItem("Media queries", MediaQueryPage, currentPath),
      navItem("Kitchen sink", KitchenSinkPage, currentPath),
    )

  /** Layout chrome with the active nav item highlighted from [[current]]'s path. */
  def page[Env, Action, StateGet, StateSet <: StateGet](
      current: RoutablePage.NoParams[?],
      title: String,
  )(
      body: Widget.Polymorphic[Env, Action, StateGet, StateSet]*,
  ): Widget.Polymorphic[Env, Action, StateGet, StateSet] =
    HolyGrail.empty
      .topHeight(48.px)
      .leftWidth(220.px)
      .top(topBar)
      .modLeft(_ => sideNav(current.path))
      // Center is O.Scrollable by default on HolyGrail (shell stays fixed).
      // Page messages auto-included by HolyGrail (includePageMessages = true).
      .center(
        div(
          // Class targeted by HolyGrail.responsiveSheet below `md` for tighter phone padding.
          Widget.`class`("oxy-page-body"),
          padding := S.spacing._6,
          maxWidth := 1100.px,
          boxSizing.borderBox,
          width := 100.pct,
          h1(title, marginBottom := S.spacing._4),
          Widget.fragment(body*),
        ),
      )

  def note(text: String): Widget =
    p(color := S.color.fg.moderate, fontSize := S.fontSize._2, marginBottom := S.spacing._4, text)

  def todoBackend(text: String): Widget =
    InfoSection(_.alert.backHighlight)(
      s"TODO (backend): $text",
    )

  /** Base trait for simple showcase pages with Unit state. */
  trait SimplePage extends RoutablePage.NoParams[Any] { self =>
    override type PageState = Unit
    override def initialLoad(params: Unit): ZIO[zio.Scope, UIError, Unit] = zio.ZIO.unit
    override def postLoad(state: WidgetState[Unit], initialState: Unit): ZIO[zio.Scope, UIError, Unit] = zio.ZIO.unit
    def pageTitle: String
    def body: Widget
    override def title(state: Unit): String = pageTitle
    override protected def component(state: WidgetState[Unit], renderState: Unit): WidgetS[Unit] =
      ShowcaseLayout.page(self, pageTitle)(body)
  }

}
