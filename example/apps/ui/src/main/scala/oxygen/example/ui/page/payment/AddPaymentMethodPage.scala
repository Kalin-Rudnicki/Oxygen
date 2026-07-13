package oxygen.example.ui.page.payment

import oxygen.core.model.currency.CurrencyCode
import oxygen.example.api.PaymentApi
import oxygen.example.api.model.payment.*
import oxygen.example.api.model.user.*
import oxygen.example.conversion.apiToUI.*
import oxygen.example.core.model.*
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.payments.stripe.ui.component.StripeComponent
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import scala.scalajs.js
import zio.*

object AddPaymentMethodPage extends RoutablePage.NoParams[LocalService & PaymentApi & StripeService] {

  final case class PageState(
      userToken: UserToken,
      stripeComponent: StripeComponent,
      initId: InitPaymentMethodId,
  ) {

    def user: User = userToken.user

  }

  override def initialLoad(params: PageParams): ZIO[LocalService & PaymentApi & StripeService & Scope, UIError, PageState] =
    for {
      paymentsApi <- ZIO.service[PaymentApi]
      userToken <- ZIO.serviceWithZIO[LocalService](_.userToken.get)
      payInit <- paymentsApi.initPaymentMethod(userToken).toUILogged(_.toUI)
      stripeComponent <- StripeComponent.create(
        publishableKey = payInit.publishableKey,
        clientSecret = payInit.clientSecret,
        currency = CurrencyCode.USD,
        appearance = new F.StripeAppearance {
          // TODO (KR) :
        },
        // Card + Apple/Google Pay only; no Link / “save for later with Link”
        elementOptions = new F.StripeElementOptions {
          paymentMethodOrder = js.Array("card")
          wallets = F.StripeWalletsOptions.cardWalletsOnly
        },
      )
    } yield PageState(
      userToken = userToken,
      stripeComponent = stripeComponent,
      initId = payInit.id,
    )

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Add Payment Method"

  override val path: Seq[String] = Seq("payment-method", "add")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService & PaymentApi, PageState] =
    PageLayout.layout(signedInNavBar(renderState.user))(
      PageMessagesBottomCorner.default,
      h1("Add Payment Method"),
      div(width := 400.px, marginRight := 40.px)(
        StripeComponent.widget.zoomOut[PageState](_.stripeComponent),
      ),
      Spacing.vertical._6,
      div(
        Button(_.primary.extraLarge)(
          "ADD",
          onClick.s[PageState].handle { widgetState =>
            for {
              paymentsApi <- ZIO.service[PaymentApi]
              current <- widgetState.currentValue
              _ <- current.stripeComponent.submit // TODO (KR) : actually verify? I think the `commit` already asserts that its a success, though
              _ <- paymentsApi.completePaymentMethod(CompletePaymentMethodRequest(current.initId), current.userToken).toUILogged(_.toUI)
              _ <- P.home.HomePage.navigate.push(())
            } yield ()
          },
        ),
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
