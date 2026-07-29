package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object SignInPage extends RoutablePage.NoParams[Any] {
  final case class PageState(remember: Boolean = false)

  override val path: Seq[String] = Seq("showcase", "auth", "sign-in")
  override def title(s: PageState): String = "Sign-in"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(SignInPage, "Sign-in")(
        ShowcaseLayout.todoBackend("Authenticate against real user API"),
        div(
          margin := css(S.spacing._2, S.spacing._14),
          padding := css(S.spacing._5, S.spacing._10),
          borderRadius := S.borderRadius._8,
          backgroundColor := S.color.bg.layerOne,
          div(
            Label("Email"),
            div(height := S.spacing._1),
            input(
              `type`.text,
              padding := S.spacing._2,
              width := 28.ch,
              border(1.px, "solid", S.color.fg.subtle),
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
            ),
          ),
          div(height := S.spacing._3),
          div(
            Label("Password"),
            div(height := S.spacing._1),
            input(
              `type`.password,
              padding := S.spacing._2,
              width := 28.ch,
              border(1.px, "solid", S.color.fg.subtle),
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
            ),
          ),
          div(height := S.spacing._3),
          Checkbox.boolean("Remember me").zoomOut[PageState](_.remember),
          div(height := S.spacing._4),
          Button("Sign in").leading(Icon.login).content(
            onClick := PageMessages.add(
              PageMessage.info(if renderState.remember then "Mock sign-in (remember on)" else "Mock sign-in — no backend"),
            ),
          ),
        ),
      )
}
