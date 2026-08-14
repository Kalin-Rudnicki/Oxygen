package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object DropdownMenuPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "overlays", "dropdown-menu")
  override def pageTitle: String = "Dropdown menu"

  private def toast(msg: String) = PageMessages.add(PageMessage.info(msg))

  /** A TopBar with left nav dropdown + right user-menu dropdown (disabled item + separator). */
  private def demoBar: TopBar.Const =
    TopBar.empty
      .surface
      .barHeight(48.px)
      .left(
        TopBar.item("Home").onClickPush(ShowcaseHubPage.nav()),
        TopBar.item.dropdown("showcase-products", "Products")(
          TopBar.menuItem("Overview").withIcon(Icon.grid).onSelect(toast("Products → Overview")),
          TopBar.menuItem("Pricing").withIcon(Icon.tag).onSelect(toast("Products → Pricing")),
          TopBar.menuSeparator,
          TopBar.menuItem("Enterprise (soon)").disabled,
        ),
      )
      .right(
        TopBar.item.dropdownWithIcon("showcase-user", Icon.user, "Jane")(
          TopBar.menuItem("Profile").withIcon(Icon.user).onSelect(toast("Profile")),
          TopBar.menuItem("Settings").withIcon(Icon.settings).onSelect(toast("Settings")),
          TopBar.menuSeparator,
          TopBar.menuItem("Sign out").withIcon(Icon.logOut).onSelect(toast("Signed out")),
        ),
      )

  /** A standalone DropdownMenu (reusable outside TopBar — e.g. SideBar / overflow menus). */
  private def standalone: Widget =
    DropdownMenu("showcase-standalone", Icon.moreHorizontal.md, span("Actions"))
      .items(
        DropdownMenu.item("Rename").withIcon(Icon.edit).onSelect(toast("Rename")),
        DropdownMenu.item("Duplicate").withIcon(Icon.copy).onSelect(toast("Duplicate")),
        DropdownMenu.separator,
        DropdownMenu.item("Delete").withIcon(Icon.trash).onSelect(toast("Delete")),
        DropdownMenu.item("Archived (soon)").disabled,
      )
      .trigger(
        padding := css(S.spacing._2, S.spacing._3),
        border := s"1px solid ${S.color.fg.subtle}",
        borderRadius := S.borderRadius._3,
        backgroundColor := S.color.bg.layerOne,
        color := S.color.fg.default,
      )

  override def body: Widget =
    fragment(
      ShowcaseLayout.note(
        "Click a trigger to open. Keyboard: Enter/Space/↓ open; ↑/↓ move (wrap); Home/End; " +
          "Enter/Space select; Esc closes and restores focus. Outside-click closes.",
      ),
      h3("In a TopBar"),
      div(
        border := s"1px solid ${S.color.fg.subtle}",
        borderRadius := S.borderRadius._3,
        overflow.visible,
        marginBottom := S.spacing._6,
        demoBar,
      ),
      h3("Standalone (reusable panel)"),
      p(color := S.color.fg.moderate, fontSize := S.fontSize._2, "Same component TopBar uses — drop it anywhere."),
      standalone,
    )
}
