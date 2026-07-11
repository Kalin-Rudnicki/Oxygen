package oxygen.payments.stripe.model

import oxygen.stripe.model.*

/** Lightweight SetupIntent view; prefer [[oxygen.payments.stripe.service.StripeService.getPaymentMethodFromSetupIntent]] when you need the full PM. */
final case class RetrieveSetupIntentResponse(
    id: StripeSetupIntentId,
    status: String,
    paymentMethodId: Option[StripePaymentMethodId],
)
