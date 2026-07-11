package oxygen.example.domain.model.payment

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.stripe.model.*

final case class InitPaymentMethod(
    id: InitPaymentMethodId,
    userId: UserId,
    stripeId: StripeSetupIntentId,
    clientSecret: StripeSetupIntentClientSecret,
    createdAt: Instant,
    completedAt: Option[Instant],
)
