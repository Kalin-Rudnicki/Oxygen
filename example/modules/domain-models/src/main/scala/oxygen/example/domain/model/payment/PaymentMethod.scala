package oxygen.example.domain.model.payment

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.payments.model.PaymentMethodType
import oxygen.stripe.model.*

final case class PaymentMethod(
    id: PaymentMethodId,
    userId: UserId,
    stripeId: StripePaymentMethodId,
    name: Option[String],
    repr: PaymentMethodType,
    ord: Int,
    createdAt: Instant,
)
