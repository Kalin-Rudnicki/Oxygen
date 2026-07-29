package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object RegisterPage extends RoutablePage.NoParams[Any] {
  final case class PageState(agree: Boolean = false)

  override val path: Seq[String] = Seq("showcase", "auth", "register")
  override def title(s: PageState): String = "Register (centered card)"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit
  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(RegisterPage, "Register (centered card)")(
        ShowcaseLayout.todoBackend("Register user API"),
        div(
          maxWidth := 28.rem,
          margin := css(0.px, "auto"),
          padding := S.spacing._6,
          borderRadius := S.borderRadius._5,
          backgroundColor := S.color.bg.layerOne,
          border(1.px, "solid", S.color.bg.layerThree),
          h2("Create account"),
          div(height := S.spacing._3),
          div(
            Label("Email"),
            div(height := S.spacing._1),
            input(
              `type`.text,
              padding := S.spacing._2,
              width := 100.pct,
              border(1.px, "solid", S.color.fg.subtle),
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
            ),
          ),
          div(height := S.spacing._2),
          div(
            Label("Password"),
            div(height := S.spacing._1),
            input(
              `type`.password,
              padding := S.spacing._2,
              width := 100.pct,
              border(1.px, "solid", S.color.fg.subtle),
              borderRadius := S.borderRadius._3,
              backgroundColor := S.color.bg.layerTwo,
              color := S.color.fg.default,
            ),
          ),
          div(height := S.spacing._2),
          Checkbox.boolean("I agree to terms").zoomOut[PageState](_.agree),
          div(height := S.spacing._4),
          Button("Register").primary.disabled(!renderState.agree).content(
            onClick := PageMessages.add(PageMessage.positive("Mock register — no backend")),
          ),
        ),
      )
}
