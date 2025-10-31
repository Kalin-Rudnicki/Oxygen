package oxygen.example.ui.page.profile

import oxygen.example.api.model.user.{User, UserToken}
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.predef.core.*
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object ProfilePage extends RoutablePage.NoParams[LocalService] {

  final case class State(
      userToken: UserToken,
  ) {

    def user: User = userToken.user

  }

  override def initialLoad(params: Params): ZIO[LocalService & Scope, UIError, State] =
    for {
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.get)
    } yield State(userToken)

  override def postLoad(state: WidgetState[State]): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: State): String = "Profile"

  override val path: Seq[String] = Seq("profile")

  override protected def component(state: State): WidgetES[LocalService, State] =
    PageLayout.layout(signedInNavBar(state.user))(
      PageMessagesBottomCorner.attached,
      h1("Profile"),
      profileInfo,
      profileActions,
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val profileInfo: WidgetS[State] =
    Widget.state[State].get { state =>
      Section.section1("Profile Info")(
        p(s"Name : ${state.user.fullName}"),
        p(s"Email : ${state.user.email}"),
      )
    }

  private lazy val profileActions: WidgetES[LocalService, State] =
    Widget.state[State].get { state =>
      Section.section1("Profile Actions")(
        Button(
          "Sign Out",
          _.apply(style = Button.Style.DestructiveMinimal),
        )(
          onClick := { ZIO.serviceWithZIO[LocalService](_.userToken.clear) *> P.login.LoginPage.navigate.push(P.login.LoginPage.Params(state.user.email.email.some)) },
        ),
      )
    }

}
