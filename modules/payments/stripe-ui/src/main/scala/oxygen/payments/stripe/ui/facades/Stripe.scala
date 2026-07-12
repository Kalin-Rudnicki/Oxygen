package oxygen.payments.stripe.ui.facades

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobalScope

/**
  * Stripe.js instance (`const stripe = Stripe(pk)`).
  *
  * Load the script from Stripe's CDN before use:
  *   https://js.stripe.com/v3/
  */
@js.native
trait Stripe extends js.Object {

  def elements(): StripeElements = js.native
  def elements(options: StripeElementsOptions): StripeElements = js.native

  def confirmSetup(options: ConfirmSetupOptions): js.Promise[SetupIntentResult] = js.native

  def confirmPayment(options: ConfirmPaymentOptions): js.Promise[PaymentIntentResult] = js.native

}

@js.native
@JSGlobalScope
object StripeGlobal extends js.Object {

  /** `window.Stripe(publishableKey)` — requires Stripe.js to be loaded. */
  def Stripe(publishableKey: String): Stripe = js.native

  def Stripe(publishableKey: String, options: StripeConstructorOptions): Stripe = js.native

}

trait StripeConstructorOptions extends js.Object {
  var apiVersion: js.UndefOr[String] = js.undefined
  var locale: js.UndefOr[String] = js.undefined
}
