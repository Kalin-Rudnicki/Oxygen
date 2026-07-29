package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object TooltipPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "overlays", "tooltip")
  override def pageTitle: String = "Tooltips"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("Hover or focus a control — tip centers below with elevated inverse chrome."),
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._4,
        flexWrap.wrap,
        // room so tips aren't flush against the next section
        paddingBottom := S.spacing._10,
        Tooltip("Save document")(Button("Save").leading(Icon.save).small),
        Tooltip("Danger zone")(Button("Delete").iconOnly(Icon.trash).small.negative.subtle),
        Tooltip("Settings")(Button().iconOnly(Icon.settings).small.subtle),
      ),
    )
}
