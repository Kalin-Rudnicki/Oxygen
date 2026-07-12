package oxygen.payments.stripe.model

import oxygen.stripe.model.*

/**
  * After FE `confirmSetup`, retrieve the SetupIntent to learn which `pm_…` was attached.
  * Prefer this (or webhooks) over listing all payment methods when you know the SetupIntent id.
  */
final case class RetrieveSetupIntentResponse(
    id: StripeSetupIntentId,
    status: String,
    paymentMethodId: Option[StripePaymentMethodId],
)
