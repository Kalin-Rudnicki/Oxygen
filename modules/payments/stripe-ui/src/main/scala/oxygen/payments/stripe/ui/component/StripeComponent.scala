package oxygen.payments.stripe.ui.component

import org.scalajs.dom.window
import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.stripe.model.*
import oxygen.ui.web.{PWidget, UIError}
import oxygen.ui.web.create.*
import scala.scalajs.js
import zio.*

final class StripeComponent private (
    private val element: StripeForeignElement,
) {

  /**
    * Full save-card flow: validate Payment Element, then `confirmSetup`.
    * Expects an in-page completion (`redirect: "if_required"`). Fails if Stripe
    * returns an error, no SetupIntent, or a non-succeeded status.
    */
  def submit: IO[UIError, SetupConfirmResult] =
    validateSubmit *> confirmSetup

  /////// Helpers ///////////////////////////////////////////////////////////////

  /** Validate the Payment Element (and collect wallet data if needed). */
  private def validateSubmit: IO[UIError, Unit] =
    StripeUtil
      .convertPromise(
        element.elements.submit(),
        _.error,
      )
      .unit

  private def confirmSetup: IO[UIError, SetupConfirmResult] = {
    val options =
      F.ConfirmSetupOptions.withElements(
        stripeElements = element.elements,
        secret = element.clientSecret.unwrap,
        returnUrl = window.location.href,
        redirectMode = "if_required",
      )

    StripeUtil
      .convertPromiseGet(
        element.stripe.confirmSetup(options),
        _.error,
        _.setupIntent,
        missing = "Payment method setup did not complete in-page. This method may require a redirect, which is not supported yet.",
      )
      .flatMap(decodeSetupIntent)
  }

  private def decodeSetupIntent(si: F.JsSetupIntent): IO[UIError, SetupConfirmResult] = {
    val status = si.status
    val paymentMethodId = paymentMethodIdFrom(si.payment_method)

    status match {
      case "succeeded" =>
        ZIO.succeed(
          SetupConfirmResult(
            setupIntentId = StripeSetupIntentId.wrap(si.id),
            status = status,
            paymentMethodId = paymentMethodId,
          ),
        )
      case other =>
        val message: String = s"Payment method setup did not succeed (status=$other).\nRedirect or additional steps may be required."
        ZIO.fail(UIError.userError(message))
    }
  }

  private def paymentMethodIdFrom(value: js.UndefOr[String | js.Object]): Option[StripePaymentMethodId] =
    StripeUtil.undefToOption(value).flatMap { v =>
      v.asInstanceOf[Matchable] match {
        case s: String if s.nonEmpty => Some(StripePaymentMethodId.wrap(s))
        case _                       => None
      }
    }

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
