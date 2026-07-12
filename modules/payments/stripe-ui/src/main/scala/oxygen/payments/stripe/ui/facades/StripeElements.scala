package oxygen.payments.stripe.ui.facades

import scala.scalajs.js

/**
  * Elements group — create/mount Payment Element, validate via [[submit]].
  *
  * Typical setup flow:
  *   stripe.elements({ mode: "setup", currency: "usd" })
  *   elements.create("payment").mount("#payment-element")
  *   elements.submit() then stripe.confirmSetup({ elements, clientSecret, ... })
  */
@js.native
trait StripeElements extends js.Object {

  def create(`type`: String): StripeElement = js.native
  def create(`type`: String, options: StripeElementOptions): StripeElement = js.native

  def getElement(`type`: String): StripeElement | Null = js.native

  def submit(): js.Promise[SubmitResult] = js.native

  def update(options: StripeElementsOptions): Unit = js.native

  def fetchUpdates(): js.Promise[js.Object] = js.native

}

/** Options for `stripe.elements(...)`. */
trait StripeElementsOptions extends js.Object {

  /** `"payment"` | `"setup"` | `"subscription"` — deferred intent mode. */
  var mode: js.UndefOr[String] = js.undefined

  /** Required with `mode` (e.g. `"usd"`). */
  var currency: js.UndefOr[String] = js.undefined

  /** When creating Elements bound to an existing Intent. */
  var clientSecret: js.UndefOr[String] = js.undefined

  var appearance: js.UndefOr[js.Object] = js.undefined

  var loader: js.UndefOr[String] = js.undefined

  /** e.g. `"manual"` when creating PM before Intent confirm. */
  var paymentMethodCreation: js.UndefOr[String] = js.undefined

}

object StripeElementsOptions {

  def setup(currencyCode: String): StripeElementsOptions =
    new StripeElementsOptions {
      mode = "setup"
      currency = currencyCode
    }

  def payment(currencyCode: String): StripeElementsOptions =
    new StripeElementsOptions {
      mode = "payment"
      currency = currencyCode
    }

  def fromClientSecret(secret: String): StripeElementsOptions =
    new StripeElementsOptions {
      clientSecret = secret
    }

}
