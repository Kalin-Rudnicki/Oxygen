package oxygen.example.domain.model.payment

import java.time.Instant
import oxygen.core.model.currency.PreciseMoney
import oxygen.example.core.model.*

final case class Payment(
    id: PaymentId,
    userId: UserId,
    paymentMethodId: PaymentMethodId,
    amount: PreciseMoney,
    description: String,
    createdAt: Instant,
)
