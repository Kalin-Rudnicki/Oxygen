package oxygen.payments.stripe.ui.component

import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.component.StripeForeignElement
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.stripe.model.*
import oxygen.ui.web.{PWidget, UIError}
import oxygen.ui.web.create.*
import zio.*

final class StripeComponent private (
    private val element: StripeForeignElement,
) {

  def submit: IO[UIError, Any] =
    ??? // FIX-PRE-MERGE (KR) :

}
object StripeComponent {

  lazy val widget: WidgetS[StripeComponent] =
    Widget.state[StripeComponent].get { stripeState =>
      PWidget.ForeignHost.empty("div", "stripe-host", stripeState.element)
    }

  /** Meant to be called on page init */
  def create(
      publishableKey: StripePublishableKey,
      clientSecret: StripeSetupIntentClientSecret,
      currency: CurrencyCode,
      appearance: F.StripeAppearance,
      elementOptions: F.StripeElementOptions,
  ): ZIO[StripeService & Scope, UIError, StripeComponent] =
    for {
      stripe <- ZIO.service[StripeService]
      element <- stripe.create(
        publishableKey = publishableKey,
        clientSecret = clientSecret,
        currency = currency,
        appearance = appearance,
        elementOptions = elementOptions,
      )
    } yield new StripeComponent(element)

}
