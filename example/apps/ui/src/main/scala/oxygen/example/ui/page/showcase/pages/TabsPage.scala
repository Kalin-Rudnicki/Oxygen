package oxygen.example.ui.page.showcase.pages

import oxygen.example.ui.page.showcase.ShowcaseLayout
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object TabsPage extends RoutablePage.NoParams[Any] {
  final case class PageState(
      tabs: Tabs.State = Tabs.State.initial("sec"),
      // Shared page state edited from tab panels (proves panels see full S)
      displayName: TextField.State = TextField.State.initial("Ada Lovelace"),
      mfaEnabled: Boolean = false,
      invoiceCount: Int = 0,
  )

  override val path: Seq[String] = Seq("showcase", "chrome", "tabs")
  override def title(s: PageState): String = "Tabs settings"
  override def initialLoad(params: Unit): ZIO[Scope, UIError, PageState] = ZIO.succeed(PageState())
  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] = ZIO.unit

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetS[PageState] =
    ShowcaseLayout
      .page(TabsPage, "Tabs settings")(
        h3(marginBottom := S.spacing._3, "Tabs"),
        p(
          color := S.color.fg.moderate,
          marginBottom := S.spacing._3,
          "Selection via Lens[PageState, Tabs.State] (string id); each panel is a normal widget over PageState. " +
            "Edit fields in one tab, switch away, and come back — state persists. Summary below stays in sync.",
        ),
        // Live summary outside the tab strip (same page state as panels)
        summaryBar(renderState),
        div(height := S.spacing._4),
        Tabs.empty[PageState](_.tabs)(
          Tabs.Tab("gen", "General")(
            generalPanel,
          ),
          Tabs.Tab("sec", "Security")(
            securityPanel,
          ),
          Tabs.Tab("bill", "Billing")(
            billingPanel(state),
          ),
        ),
      )

  private def summaryBar(s: PageState): WidgetS[PageState] =
    div(
      padding := S.spacing._3,
      borderRadius := S.borderRadius._3,
      backgroundColor := S.color.bg.layerTwo,
      border(1.px, "solid", S.color.bg.layerThree),
      display.flex,
      flexDirection.column,
      gap := S.spacing._1,
      span(fontWeight := "600", "Live page state (outside Tabs)"),
      span(color := S.color.fg.moderate, s"Selected tab: ${s.tabs.selected}"),
      span(color := S.color.fg.moderate, s"Display name: ${s.displayName.text}"),
      span(color := S.color.fg.moderate, s"MFA: ${if s.mfaEnabled then "on" else "off"}"),
      span(color := S.color.fg.moderate, s"Invoices: ${s.invoiceCount}"),
    )

  private def generalPanel: WidgetS[PageState] =
    div(
      padding := S.spacing._3,
      display.flex,
      flexDirection.column,
      gap := S.spacing._3,
      p(color := S.color.fg.moderate, "General — TextField bound to PageState.displayName"),
      TextField
        .form[String]("Display name")
        .describe("Edited here; visible in the summary and other tabs")
        .width(28.ch)
        .widget
        .discardAction
        .zoomOut[PageState](_.displayName),
    )

  private def securityPanel: WidgetS[PageState] =
    div(
      padding := S.spacing._3,
      display.flex,
      flexDirection.column,
      gap := S.spacing._3,
      p(color := S.color.fg.moderate, "Security — Checkbox + ToggleThumb on PageState.mfaEnabled"),
      Checkbox.boolean("Require multi-factor authentication").zoomOut[PageState](_.mfaEnabled),
      div(
        display.flex,
        alignItems.center,
        gap := S.spacing._3,
        ToggleThumb.boolean(_.positive).zoomOut[PageState](_.mfaEnabled),
        span("MFA (same field as checkbox above)"),
      ),
      Widget.state[PageState].get { s =>
        p(
          color := (if s.mfaEnabled then S.color.status.positive.standard else S.color.fg.moderate),
          if s.mfaEnabled then "MFA is enabled for this account."
          else "MFA is currently off.",
        )
      },
    )

  private def billingPanel(page: WidgetState[PageState]): WidgetS[PageState] =
    div(
      padding := S.spacing._3,
      display.flex,
      flexDirection.column,
      gap := S.spacing._3,
      p(color := S.color.fg.moderate, "Billing — counter on PageState.invoiceCount via WidgetState.update"),
      Widget.state[PageState].fixGet { (ws, s) =>
        fragment(
          p(fontWeight := "600", s"Invoices on file: ${s.invoiceCount}"),
          div(
            display.flex,
            gap := S.spacing._2,
            flexWrap.wrap,
            Button("+1 invoice").small.content(onClick := ws.update(ps => ps.copy(invoiceCount = ps.invoiceCount + 1))),
            Button("−1").small.subtle.content(
              onClick := ws.update(ps => ps.copy(invoiceCount = (ps.invoiceCount - 1).max(0))),
            ),
            Button("Reset").small.subtle.content(onClick := page.update(_.copy(invoiceCount = 0))),
          ),
          p(
            color := S.color.fg.subtle,
            s"(Also known as ${s.displayName.text}'s billing — name from General tab)",
          ),
        )
      },
    )

}
