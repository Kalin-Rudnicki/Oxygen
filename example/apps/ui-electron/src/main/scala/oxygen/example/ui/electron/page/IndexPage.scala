package oxygen.example.ui.electron.page

import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object IndexPage extends RoutablePage.NoParams[Any] {

  final case class PageState(
  )

  override def initialLoad(params: PageParams): ZIO[Scope, UIError, PageState] =
    ZIO.succeed { PageState() }

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Index"

  override val path: Seq[String] = Seq()

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[Any, PageState] =
    fragment(
      PageMessagesBottomCorner.default,
      h1("Oxygen Electron Example"),
      br,
      div(
        Button()(
          "Other Page (updated)",
          onClick.push(OtherPage),
        ),
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
