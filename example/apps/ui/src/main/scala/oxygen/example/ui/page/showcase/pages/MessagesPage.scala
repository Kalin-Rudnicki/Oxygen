package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object MessagesPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "feedback", "messages")
  override def pageTitle: String = "Page messages"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("Fire toasts via PageMessages (bottom corner mounted in layout)."),
      div(
        Button("Info").small.informational.content(onClick := PageMessages.add(PageMessage.info("Info toast"))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Success").small.positive.content(onClick := PageMessages.add(PageMessage.positive("Saved"))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Warning").small.alert.content(onClick := PageMessages.add(PageMessage.warning("Careful"))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Error").small.negative.content(onClick := PageMessages.add(PageMessage.error("Failed"))),
      ),
      Spacing.vertical._6,
      div(
        Button("Info").small.informational.content(onClick := PageMessages.add(PageMessage.info("Info toast, " * 50))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Success").small.positive.content(onClick := PageMessages.add(PageMessage.positive("Saved, " * 50))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Warning").small.alert.content(onClick := PageMessages.add(PageMessage.warning("Careful, " * 50))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Error").small.negative.content(onClick := PageMessages.add(PageMessage.error("Failed, " * 50))),
      ),
    )
}
