package oxygen.payments.stripe.ui.facades

import scala.scalajs.js

/** Options for `stripe.confirmSetup(...)`. */
trait ConfirmSetupOptions extends js.Object {
  var elements: js.UndefOr[StripeElements] = js.undefined
  var clientSecret: js.UndefOr[String] = js.undefined
  var confirmParams: js.UndefOr[ConfirmParams] = js.undefined

  /** `"always"` | `"if_required"` — prefer `"if_required"` for cards to avoid full redirect. */
  var redirect: js.UndefOr[String] = js.undefined
}

object ConfirmSetupOptions {

  def withElements(
      stripeElements: StripeElements,
      secret: String,
      returnUrl: String,
      redirectMode: String = "if_required",
  ): ConfirmSetupOptions =
    new ConfirmSetupOptions {
      elements = stripeElements
      clientSecret = secret
      redirect = redirectMode
      confirmParams = ConfirmParams.returnUrl(returnUrl)
    }

}

/** Options for `stripe.confirmPayment(...)` (e.g. SCA recovery). */
trait ConfirmPaymentOptions extends js.Object {
  var elements: js.UndefOr[StripeElements] = js.undefined
  var clientSecret: js.UndefOr[String] = js.undefined
  var confirmParams: js.UndefOr[ConfirmParams] = js.undefined
  var redirect: js.UndefOr[String] = js.undefined
}

object ConfirmPaymentOptions {

  def withClientSecret(
      secret: String,
      returnUrl: String,
      redirectMode: String = "if_required",
  ): ConfirmPaymentOptions =
    new ConfirmPaymentOptions {
      clientSecret = secret
      redirect = redirectMode
      confirmParams = ConfirmParams.returnUrl(returnUrl)
    }

  def withElements(
      stripeElements: StripeElements,
      secret: String,
      returnUrl: String,
      redirectMode: String = "if_required",
  ): ConfirmPaymentOptions =
    new ConfirmPaymentOptions {
      elements = stripeElements
      clientSecret = secret
      redirect = redirectMode
      confirmParams = ConfirmParams.returnUrl(returnUrl)
    }

}

trait ConfirmParams extends js.Object {
  var return_url: js.UndefOr[String] = js.undefined
}

object ConfirmParams {

  def returnUrl(url: String): ConfirmParams =
    new ConfirmParams {
      return_url = url
    }

}
