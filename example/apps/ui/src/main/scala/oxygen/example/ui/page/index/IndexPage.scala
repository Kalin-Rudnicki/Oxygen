package oxygen.example.ui.page.index

import oxygen.core.syntax.number.*
import oxygen.example.api.UserApi
import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object IndexPage extends RoutablePage.NoParams[UserApi & LocalService] {

  final case class PageState(
      userToken: Option[UserToken],
  ) {

    def user: Option[User] = userToken.map(_.user)

  }

  override def initialLoad(params: PageParams): ZIO[LocalService & Scope, UIError, PageState] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.getOption)
    } yield PageState(userToken)

  private def runTest(size: Int): URIO[UserApi, Unit] =
    ZIO.serviceWithZIO[UserApi](_.makeResponse(size)).timed.flatMap { (dur, response) =>
      if (response.length == size) PageMessages.add(PageMessage.positive(s"Success! (${size.toStringCommas}) in ${dur.render}"))
      else PageMessages.add(PageMessage.negative(s"Error! (exp=${response.length.toStringCommas} != exp=${size.toStringCommas})"))
    }

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[UserApi & Scope, UIError, Unit] =
    ZIO.foreachDiscard(Chunk(100, 1_000, 10_000, 100_000, 1_000_000, 2_500_000, 10_000_000, 25_000_000))(runTest)

  override def title(state: PageState): String = "Index"

  override val path: Seq[String] = Seq()

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(optionalSignedInNavBar(renderState.user))(
      PageMessagesBottomCorner.default,
      h1("Oxygen Example"),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
