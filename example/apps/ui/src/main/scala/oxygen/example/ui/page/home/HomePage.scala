package oxygen.example.ui.page.home

import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object HomePage extends RoutablePage.NoParams[LocalService] {

  final case class PageState(
      userToken: UserToken,
  ) {

    def user: User = userToken.user

  }

  override def initialLoad(params: PageParams): ZIO[LocalService & Scope, UIError, PageState] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.get)
    } yield PageState(userToken)

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Home"

  override val path: Seq[String] = Seq("home")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(signedInNavBar(renderState.user))(
      PageMessagesBottomCorner.attached,
      h1("Home"),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
