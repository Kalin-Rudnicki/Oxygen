package oxygen.example.ui.page.login

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

  final case class Params(
      email: Option[String],
  )

  final case class State(
      email: String,
      password: String,
  ) {

    def optEmail: Option[String] = {
      val trimmed = email.trim
      trimmed.someWhen(_.nonEmpty)
    }

  }

  override lazy val paramCodec: PageCodec[Params] =
    ("page" / "login" / PageCodec.query.plain.optional[String]("email")).autoTransform

  override def paramsFromState(state: State): Params =
    Params(state.optEmail)

  override def initialLoad(params: Params): ZIO[Scope, UIError, State] =
    ZIO.succeed(State(params.email.getOrElse(""), ""))

  override def postLoad(state: WidgetState[State]): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: State): String = "Login"

  override protected def component(state: State): WidgetES[UserApi & LocalService, State] =
    PageLayout.layout(signedOutNavBar(state.optEmail))(
      PageMessagesBottomCorner.attached,
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
          Form
            .textInput[Email](
              "Email",
              formProps = Form.textInput.Props(width = 300.px),
              inputProps = TextInput.Props(inputType = "email", width = 100.pct),
            )
            .required
            .zoomOut[State](_.email) &&
            Form
              .textInput[String](
                "Password",
                formProps = Form.textInput.Props(width = 300.px),
                inputProps = TextInput.Props(inputType = "password", width = 100.pct),
              )
              .required
              .zoomOut[State](_.password) &&
            Form.submitButton("Login", _(size = Button.Size.Large))
        ).onSubmit.sv { case (_, (email, password)) =>
          for {
            _ <- ZIO.logInfo(s"email = $email, replace-email = ${email.referenceEmail}, password = $password")
            res <- UserApi.login(LoginRequest(email, password)).toUILogged(_.toUI)
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
