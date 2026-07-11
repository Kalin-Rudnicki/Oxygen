package oxygen.payments.stripe.ui.facades

import scala.scalajs.js

/** Result of `elements.submit()`. */
@js.native
trait SubmitResult extends js.Object {
  val error: js.UndefOr[StripeJsError] = js.native
}

/** Result of `stripe.confirmSetup(...)`. */
@js.native
trait SetupIntentResult extends js.Object {
  val error: js.UndefOr[StripeJsError] = js.native
  val setupIntent: js.UndefOr[JsSetupIntent] = js.native
}

/** Result of `stripe.confirmPayment(...)`. */
@js.native
trait PaymentIntentResult extends js.Object {
  val error: js.UndefOr[StripeJsError] = js.native
  val paymentIntent: js.UndefOr[JsPaymentIntent] = js.native
}

/**
  * Stripe.js error object (not the same as JVM `StripeError`).
  * Named [[StripeJsError]] to avoid clashing with service-side errors.
  */
@js.native
trait StripeJsError extends js.Object {
  val `type`: String = js.native
  val message: js.UndefOr[String] = js.native
  val code: js.UndefOr[String] = js.native
  val decline_code: js.UndefOr[String] = js.native
  val param: js.UndefOr[String] = js.native
}

@js.native
trait JsSetupIntent extends js.Object {
  val id: String = js.native
  val status: String = js.native
  val client_secret: js.UndefOr[String] = js.native

  /** Payment method id string, or expanded object depending on API. */
  val payment_method: js.UndefOr[String | js.Object] = js.native
}

@js.native
trait JsPaymentIntent extends js.Object {
  val id: String = js.native
  val status: String = js.native
  val client_secret: js.UndefOr[String] = js.native
  val payment_method: js.UndefOr[String | js.Object] = js.native
}
