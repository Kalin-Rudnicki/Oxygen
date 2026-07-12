package oxygen.payments.stripe.model

import oxygen.core.model.Email
import oxygen.core.model.currency.*
import oxygen.stripe.model.*

final case class CreatePaymentIntentRequest(
    customerId: StripeCustomerId,
    paymentMethodId: StripePaymentMethodId,
    amount: PreciseMoney,
    description: String,
    email: Option[Email],
)
