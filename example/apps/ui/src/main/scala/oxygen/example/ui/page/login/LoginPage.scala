package oxygen.example.ui.page.login

import oxygen.crypto.model.Password
import oxygen.example.api.*
import oxygen.example.api.model.user.*
import oxygen.example.conversion.apiToUI.*
import oxygen.example.core.model.user.Email
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.predef.core.*
import oxygen.ui.web.{*, given}
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

// TODO (KR) : support redirect query param, useful if user attempts to navigate to a page, but is not logged in
//           : switching back and forth between login/register should maintain redirect url
object LoginPage extends RoutablePage[UserApi & LocalService] {

  final case class PageParams(
      email: Option[String],
  )

  final case class PageState(
      email: TextField.State,
      password: TextField.State,
  ) {

    def optEmail: Option[String] =
      email.text.trim.someWhen(_.nonEmpty)

  }

  override lazy val paramCodec: PageCodec[PageParams] =
    ("login" / PageCodec.query.plain.optional[String]("email")).autoTransform

  override def paramsFromState(state: PageState): PageParams =
    PageParams(state.optEmail)

  override def initialLoad(params: PageParams): ZIO[Scope, UIError, PageState] =
    ZIO.succeed(PageState(TextField.State.initial(params.email.getOrElse("")), TextField.State.empty))

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Login"

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[UserApi & LocalService, PageState] =
    PageLayout.layout(signedOutNavBar(renderState.optEmail))(
      PageMessagesBottomCorner.default,
      PageBodies.centeredCard(
        boxShadow := "0 4px 32px #01810120",
        h1(
          "Login",
          padding(0.px, S.spacing._8, S.spacing._1),
          marginTop := 0.px,
          color := S.color.primary,
          borderBottom := css(S.borderWidth._2, "solid", S.color.primary),
        ),
        (
          TextField
            .form[Email]("Email", _.email.width(300.px))
            .required
            .zoomOut[PageState](_.email) <*>
            TextField
              .form[String]("Password", _.password.width(300.px))
              .required
              .zoomOut[PageState](_.password) <*>
            Button.form("Login", _.button(_.medium))
        ).handleActionStateful { case (_, (email, password)) =>
          for {
            _ <- ZIO.logInfo("submitting form...")
            req = LoginRequest(email, Password.PlainText.wrap(password))
            res <- UserApi.login(req).toUILogged(_.toUI)
            _ <- ZIO.serviceWithZIO[LocalService](_.userToken.set(res.authorization))
            _ <- P.home.HomePage.navigate.push(())
          } yield ()
        },
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
