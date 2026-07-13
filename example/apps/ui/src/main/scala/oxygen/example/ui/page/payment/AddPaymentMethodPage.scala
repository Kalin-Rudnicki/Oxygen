package oxygen.example.ui.page.payment

import oxygen.core.model.currency.CurrencyCode
import oxygen.example.api.PaymentApi
import oxygen.example.api.model.user.*
import oxygen.example.conversion.apiToUI.*
import oxygen.example.ui.common.*
import oxygen.example.ui.page as P
import oxygen.example.ui.service.LocalService
import oxygen.payments.stripe.ui.component.StripeComponent
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.ui.web.*
import oxygen.ui.web.component.*
import oxygen.ui.web.create.{*, given}
import zio.*

object AddPaymentMethodPage extends RoutablePage.NoParams[LocalService & PaymentApi & StripeService] {

  final case class PageState(
      userToken: UserToken,
      stripeComponent: StripeComponent,
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
        elementOptions = new F.StripeElementOptions {
          // TODO (KR) :
        },
      )
    } yield PageState(
      userToken = userToken,
      stripeComponent = stripeComponent,
    )

  override def postLoad(state: WidgetState[PageState], initialState: PageState): ZIO[Scope, UIError, Unit] =
    ZIO.unit

  override def title(state: PageState): String = "Add Payment Method"

  override val path: Seq[String] = Seq("payment-method", "add")

  override protected def component(state: WidgetState[PageState], renderState: PageState): WidgetES[LocalService, PageState] =
    PageLayout.layout(signedInNavBar(renderState.user))(
      PageMessagesBottomCorner.default,
      h1("Add Payment Method"),
      div(width := 50.pct)(
        StripeComponent.widget.zoomOut[PageState](_.stripeComponent),
      ),
      Spacing.vertical._6,
      div(
        Button(_.primary.extraLarge)(
          "ADD",
          onClick.s[PageState].handle { widgetState =>
            for {
              current <- widgetState.currentValue
              res <- current.stripeComponent.submit
              _ <- ZIO.logInfo(s"Res : $res")
              _ <- ZIO.dieMessage("todo")
            } yield ???
          },
        ),
      ),
    )

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Components
  //////////////////////////////////////////////////////////////////////////////////////////////////////

}
