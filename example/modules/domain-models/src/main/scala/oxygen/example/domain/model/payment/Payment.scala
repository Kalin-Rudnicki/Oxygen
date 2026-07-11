package oxygen.example.domain.model.payment

import java.time.Instant
import oxygen.core.model.currency.PreciseMoney
import oxygen.example.core.model.*
import oxygen.stripe.model.StripePaymentIntentId

final case class Payment(
    id: PaymentId,
    userId: UserId,
    paymentMethodId: PaymentMethodId,
    stripeId: StripePaymentIntentId,
    amount: PreciseMoney,
    description: String,
    status: String,
    createdAt: Instant,
)
