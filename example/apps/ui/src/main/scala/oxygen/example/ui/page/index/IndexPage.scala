package oxygen.example.ui.page.index

import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object IndexPage extends RoutablePage.NoParams[LocalService] {

  final case class State(
      userToken: Option[UserToken],
  ) {

    def user: Option[User] = userToken.map(_.user)

  }

  override def initialLoad(params: Params): ZIO[LocalService & Scope, UIError, State] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.getOption)
    } yield State(userToken)

  override def postLoad(state: WidgetState[State]): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: State): String = "Index"

  override val path: Seq[String] = Seq("page")

  override protected def component(state: State): WidgetES[LocalService, State] =
    PageLayout.layout(optionalSignedInNavBar(state.user))(
      PageMessagesBottomCorner.attached,
      h1("Oxygen Example"),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
