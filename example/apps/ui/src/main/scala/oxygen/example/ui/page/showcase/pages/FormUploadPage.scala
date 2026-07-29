package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.*
import zio.*

object FormUploadPage extends RoutablePage.NoParams[Any] {
  final case class PageState(drop: FileDropZone.State = FileDropZone.State())

  override val path: Seq[String] = Seq("showcase", "forms", "upload")
  override def title(s: PageState): String = "File upload"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(FormUploadPage, "File upload")(
        ShowcaseLayout.todoBackend("Upload endpoint / storage"),
        ShowcaseLayout.note("Drag & drop or click the zone to open the system file picker (names only, mock)."),
        FileDropZone("Drop files here or click to browse").zoomOut[PageState](_.drop),
      )

}

/**
  * One scrollable form with every built-in field type — visual/API smoke for form controls.
  */
