package oxygen.example.domain.model.payment

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.stripe.model.*

final case class InitPayment(
    id: InitPaymentMethodId,
    userId: UserId,
    clientSecret: StripeSetupIntentClientSecret,
    // FIX-PRE-MERGE (KR) : needs some sort of status
    createdAt: Instant,
)
