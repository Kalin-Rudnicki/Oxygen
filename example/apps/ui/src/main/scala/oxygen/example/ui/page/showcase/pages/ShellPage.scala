package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import oxygen.ui.web.service.ColorMode

object ShellPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "shell")
  override def pageTitle: String = "App shell (HolyGrail)"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("TopBar + SideBar + scrollable center. Resize viewport for responsive collapse."),
      p("This page is already inside the showcase HolyGrail shell."),
      div(height := S.spacing._3),
      Button("System theme").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.System)),
      span(display.inlineBlock, width := S.spacing._2),
      Button("Light").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.Light)),
      span(display.inlineBlock, width := S.spacing._2),
      Button("Dark").small.subtle.content(onClick := ColorMode.setAndPersist(ColorMode.Mode.Dark)),
    )
}
