package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object MessagesPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "feedback", "messages")
  override def pageTitle: String = "Page messages"
  override def body: Widget = {
    // An unbroken run (no break opportunities) is the only case that actually exercises
    // `word-break: break-word` / `overflow-wrap: anywhere` on the toast text container; the
    // `"…, " * 50` repros below have a space/comma every few chars and soft-wrap without them.
    val unbrokenRun: String = "https://oxygen.example.com/very/long/path?token=" + ("a1b2c3d4e5" * 24)
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
      Spacing.vertical._6,
      ShowcaseLayout.note("Unbroken run (no spaces / long URL): verifies overflow-wrap: anywhere + word-break on the text container."),
      div(
        Button("Info").small.informational.content(onClick := PageMessages.add(PageMessage.info(unbrokenRun))),
        span(display.inlineBlock, width := S.spacing._2),
        Button("Error").small.negative.content(onClick := PageMessages.add(PageMessage.error(unbrokenRun))),
      ),
    )
  }
}
