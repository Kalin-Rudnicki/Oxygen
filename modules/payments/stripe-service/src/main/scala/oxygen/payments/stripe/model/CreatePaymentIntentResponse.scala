package oxygen.payments.stripe.model

import oxygen.stripe.model.*

/**
  * Outcome of create+confirm (off-session) PaymentIntent.
  *
  * Transport/SDK failures stay on the error channel (`StripeError`).
  * These variants cover a successfully returned PaymentIntent object
  * whose status still needs product-level handling.
  */
sealed trait CreatePaymentIntentResponse {
  val paymentIntentId: StripePaymentIntentId
}
object CreatePaymentIntentResponse {

  /** Charge completed. Prefer webhooks as fulfillment source of truth. */
  final case class Succeeded(
      paymentIntentId: StripePaymentIntentId,
  ) extends CreatePaymentIntentResponse

  /** Async settlement in progress. Wait for webhook. */
  final case class Processing(
      paymentIntentId: StripePaymentIntentId,
  ) extends CreatePaymentIntentResponse

  /**
    * Customer must complete on-session authentication (e.g. 3DS).
    * FE: Stripe.js `confirmPayment` with `clientSecret`.
    */
  final case class RequiresAction(
      paymentIntentId: StripePaymentIntentId,
      clientSecret: StripePaymentIntentClientSecret,
  ) extends CreatePaymentIntentResponse

  /**
    * Charge did not complete; typically a decline / invalid PM.
    * Status is usually `requires_payment_method`.
    */
  final case class RequiresPaymentMethod(
      paymentIntentId: StripePaymentIntentId,
      failureCode: Option[String],
      failureMessage: Option[String],
  ) extends CreatePaymentIntentResponse

  final case class Canceled(
      paymentIntentId: StripePaymentIntentId,
  ) extends CreatePaymentIntentResponse

  /**
    * Statuses we do not expect for the off-session confirm flow
    * (e.g. `requires_confirmation`, `requires_capture`) but may still see.
    */
  final case class UnexpectedStatus(
      paymentIntentId: StripePaymentIntentId,
      status: String,
  ) extends CreatePaymentIntentResponse

}
