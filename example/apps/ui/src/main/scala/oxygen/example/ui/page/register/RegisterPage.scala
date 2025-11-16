package oxygen.example.ui.page.register

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
object RegisterPage extends RoutablePage[UserApi & LocalService] {

  final case class PageParams(
      email: Option[String],
  )

  final case class PageState(
      firstName: TextField.State,
      lastName: TextField.State,
      email: TextField.State,
      password: TextField.State,
  ) {

    def optEmail: Option[String] =
      email.text.trim.someWhen(_.nonEmpty)

  }

  override lazy val paramCodec: PageCodec[PageParams] =
    ("register" / PageCodec.query.plain.optional[String]("email")).autoTransform

  override def paramsFromState(state: PageState): PageParams =
    PageParams(state.optEmail)

  override def initialLoad(params: PageParams): ZIO[Scope, UIError, PageState] =
    ZIO.succeed(PageState(TextField.State.empty, TextField.State.empty, TextField.State.initial(params.email.getOrElse("")), TextField.State.empty))

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  override def title(state: PageState): String = "Sign Up"

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[UserApi & LocalService, PageState] =
    PageLayout.layout(signedOutNavBar(renderState.optEmail))(
      PageMessagesBottomCorner.default,
      PageBodies.centeredCard(
        boxShadow := "0 4px 32px #01810120",
        h1(
          "Sign Up",
          padding(0.px, S.spacing._8, S.spacing._2),
          marginTop := 0.px,
          color := S.color.primary,
          borderBottom := css(S.borderWidth._2, "solid", S.color.primary),
        ),
        registerForm.onSubmit { (_, req) =>
          for {
            _ <- ZIO.logInfo("submitting form...")
            res <- UserApi.register(req).toUILogged(_.toUI)
            _ <- ZIO.serviceWithZIO[LocalService](_.userToken.set(res.authorization))
            _ <- P.home.HomePage.navigate.push(())
          } yield ()
        },
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private lazy val registerForm: SubmitFormS[PageState, RegisterRequest] =
    for {
      (firstNameWidget, firstNameValue) <-
        TextField
          .form[String]("First Name", _.width(300.px))
          .required
          .zoomOut[PageState](_.firstName)
      (lastNameWidget, lastNameValue) <-
        TextField
          .form[String]("Last Name", _.width(300.px))
          .required
          .zoomOut[PageState](_.lastName)
      (emailWidget, emailValue) <-
        TextField
          .form[Email]("Email", _.email.width(300.px))
          .required
          .zoomOut[PageState](_.email)
      (passwordWidget, passwordValue) <-
        TextField
          .form[String]("Password", _.password.width(300.px))
          .required
          .mapValue(Password.PlainText.wrap)
          .zoomOut[PageState](_.password)
      (submitWidget, _) <- Button.form("Sign Up", _.button(_.medium))
    } yield (
      fragment(firstNameWidget, lastNameWidget, emailWidget, passwordWidget, submitWidget),
      (emailValue <*> firstNameValue <*> lastNameValue <*> passwordValue).mapValue(RegisterRequest.apply),
    )

}
