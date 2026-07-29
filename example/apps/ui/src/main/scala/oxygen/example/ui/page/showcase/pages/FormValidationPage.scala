package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}

object FormValidationPage extends ShowcaseLayout.SimplePage {
  override val path: Seq[String] = Seq("showcase", "forms", "validation")
  override def pageTitle: String = "Form validation"
  override def body: Widget =
    fragment(
      ShowcaseLayout.note("Field anatomy: labels, long wrap labels, required-looking chrome."),
      Label("Very long field label that should wrap when the form column is narrow"),
      div(
        Label("Name"),
        div(height := S.spacing._1),
        input(
          `type`.text,
          padding := S.spacing._2,
          width := 28.ch,
          border(1.px, "solid", S.color.fg.subtle),
          borderRadius := S.borderRadius._3,
          backgroundColor := S.color.bg.layerTwo,
          color := S.color.fg.default,
        ),
      ),
      div(height := S.spacing._2),
      div(
        Label("Bio"),
        div(height := S.spacing._1),
        textArea(
          padding := S.spacing._2,
          width := 28.ch,
          height := 6.em,
          border(1.px, "solid", S.color.fg.subtle),
          borderRadius := S.borderRadius._3,
          backgroundColor := S.color.bg.layerTwo,
          color := S.color.fg.default,
        ),
      ),
      div(height := S.spacing._3),
      Button("Submit").content(onClick := PageMessages.add(PageMessage.warning("Client-side validation mock"))),
    )
}
