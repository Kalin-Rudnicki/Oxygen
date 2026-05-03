package oxygen.example.ui.electron.page

import oxygen.node.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object OtherPage extends RoutablePage.NoParams[Any] {

  final case class PageState(
      cwd1: String,
      cwd2: String,
      home: String,
  )

  override def initialLoad(params: PageParams): ZIO[Scope, UIError, PageState] =
    for {
      cwd1 <- ChildProcess.spawn("pwd").orDie
      cwd2 <- OS.cwd.orDie
      home <- OS.home.orDie
    } yield PageState(
      cwd1.trim,
      cwd2.trim,
      home.trim,
    )

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Other"

  override val path: Seq[String] = Seq("other")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[Any, PageState] =
    fragment(
      PageMessagesBottomCorner.default,
      h1("Oxygen Electron Example - Other"),
      p(renderState.cwd1),
      p(renderState.cwd2),
      p(renderState.home),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
