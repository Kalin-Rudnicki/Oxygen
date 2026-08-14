package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object ResponsiveTopBarPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "responsive-topbar")
  override def pageTitle: String = "Responsive TopBar (overflow menu)"

  private def toast(msg: String) = PageMessages.add(PageMessage.info(msg))

  /**
    * A TopBar whose nav items (OXY-151) render inline at `>= md`, and auto-collapse into a single
    * "More" [[DropdownMenu]] (OXY-152) below `md`. The swap is pure CSS — resize the frame to see it.
    */
  private def demoBar: TopBar.Const =
    TopBar.empty
      .brand
      .barHeight(52.px)
      .left(
        TopBar.item.index("MyApp").onClickPush(ShowcaseHubPage.nav()),
      )
      .nav(
        TopBar.menuItem("Home").withIcon(Icon.home).onSelect(toast("Home")),
        TopBar.menuItem("Products").withIcon(Icon.grid).onSelect(toast("Products")),
        TopBar.menuItem("Pricing").withIcon(Icon.tag).onSelect(toast("Pricing")),
        TopBar.menuItem("Docs").withIcon(Icon.book).onSelect(toast("Docs")),
        TopBar.menuSeparator,
        TopBar.menuItem("About").onSelect(toast("About")),
        TopBar.menuItem("Enterprise (soon)").disabled,
      )
      .right(
        TopBar.item.dropdownWithIcon("responsive-user", Icon.user, "Jane")(
          TopBar.menuItem("Profile").withIcon(Icon.user).onSelect(toast("Profile")),
          TopBar.menuSeparator,
          TopBar.menuItem("Sign out").withIcon(Icon.logOut).onSelect(toast("Signed out")),
        ),
      )

  /** Same bar in a deliberately narrow frame so the overflow "More" menu is always shown. */
  private def narrowFrame: Widget =
    div(
      width := 360.px,
      maxWidth := 100.pct,
      border := s"1px solid ${S.color.fg.subtle}",
      borderRadius := S.borderRadius._3,
      overflow.visible,
      demoBar,
    )

  override def body: Widget =
    fragment(
      ShowcaseLayout.note(
        "Nav items are declared once via TopBar.nav(...). At >= md they render inline; below md they " +
          "auto-collapse into a single \"More\" dropdown (the reused OXY-152 DropdownMenu). No JS — the " +
          "swap is pure CSS @media, so it is SSR/hydration safe.",
      ),
      h3("Live bar (resize the window / frame to cross the md breakpoint)"),
      div(
        border := s"1px solid ${S.color.fg.subtle}",
        borderRadius := S.borderRadius._3,
        overflow.visible,
        marginBottom := S.spacing._6,
        demoBar,
      ),
      h3("Forced-narrow frame (always shows the collapsed \"More\" menu)"),
      p(color := S.color.fg.moderate, fontSize := S.fontSize._2, "The 360px wrapper is below md, so only the overflow menu is visible."),
      narrowFrame,
    )
}
