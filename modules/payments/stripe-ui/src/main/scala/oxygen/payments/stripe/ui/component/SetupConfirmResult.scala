package oxygen.payments.stripe.ui.component

import oxygen.stripe.model.*

/** Successful in-page `confirmSetup` result (no full redirect). */
final case class SetupConfirmResult(
    setupIntentId: StripeSetupIntentId,
    status: String,
    paymentMethodId: Option[StripePaymentMethodId],
)
