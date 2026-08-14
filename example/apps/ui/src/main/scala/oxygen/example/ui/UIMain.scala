package oxygen.example.ui

import oxygen.example.api.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.http.client.*
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.ui.web.*
import oxygen.ui.web.apispec.ApiSpecPage
import oxygen.ui.web.create.*
import oxygen.ui.web.defaults.*
import oxygen.ui.web.service.{ColorTheme, LocalStorage}
import scala.collection.immutable.ArraySeq
import zio.*

object UIMain extends PageApp[UIMain.Env] {

  type Env =
    UserApi & ConnectionApi & StreamApi & PaymentApi & LocalService & RawClient & StripeService

  // override val logLevel: LogLevel = LogLevel.Debug

  /*
  override val jobs: Seq[GlobalJob[Env]] =
    Seq(
      GlobalJob.simplePoll("say-hi", 15.seconds)(5.seconds) {
        PageMessages.schedule(PageMessage.info("Hi there!"), 2.seconds)
      },
    )
   */

  override val styleSheets: ArraySeq[StyleSheet] =
    coreOxygenStyleSheets

  override protected def prePageLoad: RIO[Env & Scope, Unit] =
    ColorTheme.install

  override val pages: ArraySeq[RoutablePage[Env]] = ArraySeq(
    P.index.IndexPage,
    P.login.LoginPage,
    P.register.RegisterPage,
    P.home.HomePage,
    P.profile.ProfilePage,
    P.payment.AddPaymentMethodPage,
    ApiSpecPage,
    StylesPage,
    ComponentsPage,
    // EX-T03 showcase demos (Any env; mock data) — pages package split
    P.showcase.pages.ShowcaseHubPage,
    P.showcase.pages.ShellPage,
    P.showcase.pages.SignInPage,
    P.showcase.pages.RegisterPage,
    P.showcase.pages.DashboardPage,
    P.showcase.pages.ThemePage,
    P.showcase.pages.IconsPage,
    P.showcase.pages.FormValidationPage,
    P.showcase.pages.FormLockPage,
    P.showcase.pages.FormChoicesPage,
    P.showcase.pages.FormDateTimePage,
    P.showcase.pages.FormColorPage,
    P.showcase.pages.FormUploadPage,
    P.showcase.pages.FormAllPage,
    P.showcase.pages.ModalPage,
    P.showcase.pages.DrawerPage,
    P.showcase.pages.TooltipPage,
    P.showcase.pages.TablePage,
    P.showcase.pages.FeedPage,
    P.showcase.pages.SortablePage,
    P.showcase.pages.TabsPage,
    P.showcase.pages.WizardPage,
    P.showcase.pages.BusyPage,
    P.showcase.pages.MessagesPage,
    P.showcase.pages.AnchorsPage,
    P.showcase.pages.GridPage,
    P.showcase.pages.KitchenSinkPage,
  )

  override def layer: TaskLayer[Env] =
    ZLayer.make[Env](
      // clients
      ZLayer.succeed { Client.Config.relativeUrl },
      Client.layer.default,
      RawClient.default, // used directly by the ApiSpecPage to GET /oxygen/api-spec
      DeriveClient.clientLayer[UserApi],
      DeriveClient.clientLayer[ConnectionApi],
      DeriveClient.clientLayer[StreamApi],
      DeriveClient.clientLayer[PaymentApi],
      // other
      LocalStorage.live,
      LocalService.layer,
      StripeService.live,
    )

}
