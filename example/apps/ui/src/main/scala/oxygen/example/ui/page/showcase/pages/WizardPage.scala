package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object WizardPage extends RoutablePage.NoParams[Any] {
  final case class PageState(stepA: Int = 0, stepB: Int = 0)

  override val path: Seq[String] = Seq("showcase", "chrome", "wizard")
  override def title(s: PageState): String = "Wizard steps"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  private def stepBody(step: Int): (String, String) =
    step match {
      case 0 => ("Step 1 of 3", "Welcome — account basics")
      case 1 => ("Step 2 of 3", "Preferences")
      case _ => ("Step 3 of 3", "Review & finish")
    }

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] = {
    val (titleA, bodyA) = stepBody(renderState.stepA)
    val (titleB, bodyB) = stepBody(renderState.stepB)
    val pctA = ((renderState.stepA + 1) * 100) / 3
    val pctB = ((renderState.stepB + 1) * 100) / 3
    // second wizard: primary while in progress, positive when finished
    val fillB =
      if renderState.stepB >= 2 then S.color.status.positive.standard
      else S.color.primary.standard
    ShowcaseLayout
      .page(WizardPage, "Wizard steps")(
        ShowcaseLayout.todoBackend("Persist onboarding steps"),
        ShowcaseLayout.note("Step state machine (Wizard form combinator is separate — this is UI steps). Progress.fill are token-colored."),
        h3("Default (primary bar)", marginBottom := S.spacing._3),
        Progress.percent(pctA),
        div(height := S.spacing._3),
        Section.level2(h3(titleA), p(bodyA)),
        div(height := S.spacing._3),
        Button("Back").small.subtle.disabled(renderState.stepA == 0).content(onClick := state.update(s => s.copy(stepA = (s.stepA - 1).max(0)))),
        span(display.inlineBlock, width := S.spacing._2),
        Button(if renderState.stepA >= 2 then "Finish" else "Next")
          .small
          .primary
          .content(
            onClick := (
              if renderState.stepA >= 2 then PageMessages.add(PageMessage.positive("Onboarding complete (mock)"))
              else state.update(s => s.copy(stepA = (s.stepA + 1).min(2)))
            ),
          ),
        div(height := S.spacing._8),
        h3("Complete color (primary → positive on finish)", marginBottom := S.spacing._3),
        Progress.percent(pctB, fillColor = fillB),
        div(height := S.spacing._3),
        Section.level2(h3(titleB), p(bodyB)),
        div(height := S.spacing._3),
        Button("Back").small.subtle.disabled(renderState.stepB == 0).content(onClick := state.update(s => s.copy(stepB = (s.stepB - 1).max(0)))),
        span(display.inlineBlock, width := S.spacing._2),
        Button(if renderState.stepB >= 2 then "Finish" else "Next")
          .small
          .primary
          .content(
            onClick := (
              if renderState.stepB >= 2 then PageMessages.add(PageMessage.positive("Setup complete — bar is green"))
              else state.update(s => s.copy(stepB = (s.stepB + 1).min(2)))
            ),
          ),
      )
  }
}
