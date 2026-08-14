package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object ShellPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "shell")
  override def pageTitle: String = "App shell (HolyGrail)"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("TopBar + SideBar + scrollable center. Resize viewport for responsive collapse."),
      p("This page is already inside the showcase HolyGrail shell."),
      div(height := S.spacing._3),
      // OXY-154: reusable ColorModePicker
      ColorModePicker(),
    )
}
