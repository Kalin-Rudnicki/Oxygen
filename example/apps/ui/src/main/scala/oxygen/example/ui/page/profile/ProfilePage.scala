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

  override def title(state: PageState): String = "Profile"

  override val path: Seq[String] = Seq("profile")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(signedInNavBar(renderState.user))(
      PageMessagesBottomCorner.default,
      h1("Profile"),
      profileInfo,
      profileActions,
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val profileInfo: WidgetS[PageState] =
    Widget.state[PageState].get { state =>
      SectionWithHeader.section1("Profile Info")(
        p(s"Name : ${state.user.fullName}"),
        p(s"Email : ${state.user.email}"),
      )
    }

  private lazy val profileActions: WidgetES[LocalService, PageState] =
    Widget.state[PageState].get { state =>
      SectionWithHeader.section1("Profile Actions")(
        Button(_.destructive.minimal)(
          "Sign Out",
          onClick := { ZIO.serviceWithZIO[LocalService](_.userToken.clear) *> P.login.LoginPage.navigate.push(P.login.LoginPage.PageParams(state.user.email.email.some)) },
        ),
      )
    }

}
