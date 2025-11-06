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
      firstName: String,
      lastName: String,
      email: String,
      password: String,
  ) {

    def optEmail: Option[String] = {
      val trimmed = email.trim
      trimmed.someWhen(_.nonEmpty)
    }

  }

  override lazy val paramCodec: PageCodec[PageParams] =
    ("register" / PageCodec.query.plain.optional[String]("email")).autoTransform

  override def paramsFromState(state: PageState): PageParams =
    PageParams(state.optEmail)

  override def initialLoad(params: PageParams): ZIO[Scope, UIError, PageState] =
    ZIO.succeed(PageState("", "", params.email.getOrElse(""), ""))

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  override def title(state: PageState): String = "Sign Up"

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[UserApi & LocalService, PageState] =
    PageLayout.layout(signedOutNavBar(renderState.optEmail))(
      PageMessagesBottomCorner.attached,
      PageBodies.centeredCard(
        boxShadow := "0 4px 32px #01810120",
        h1(
          "Sign Up",
          padding(0.px, S.spacing._8, S.spacing._2),
          marginTop := 0.px,
          color := S.color.primary,
          borderBottom := css(S.borderWidth._2, "solid", S.color.primary),
        ),
        (
          Form
            .textField[String](
              "First Name",
              formProps = Form.textField.Props(width = 300.px),
              inputProps = TextField.Props(width = 100.pct),
            )
            .required
            .zoomOut[PageState](_.firstName) <*>
            Form
              .textField[String](
                "Last Name",
                formProps = Form.textField.Props(width = 300.px),
                inputProps = TextField.Props(width = 100.pct),
              )
              .required
              .zoomOut[PageState](_.lastName) <*>
            Form
              .textField[Email](
                "Email",
                formProps = Form.textField.Props(width = 300.px),
                inputProps = TextField.Props(inputType = "email", width = 100.pct),
              )
              .required
              .zoomOut[PageState](_.email) <*>
            Form
              .textField[String](
                "Password",
                formProps = Form.textField.Props(width = 300.px),
                inputProps = TextField.Props(inputType = "password", width = 100.pct),
              )
              .required
              .zoomOut[PageState](_.password) <*>
            Form.submitButton("Sign Up", _.medium)
        ).onSubmit { case (_, (firstName, lastName, email, password)) =>
          for {
            _ <- ZIO.logInfo("submitting form...")
            req = RegisterRequest(
              email = email,
              firstName = firstName,
              lastName = lastName,
              password = Password.PlainText.wrap(password),
            )
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

}
