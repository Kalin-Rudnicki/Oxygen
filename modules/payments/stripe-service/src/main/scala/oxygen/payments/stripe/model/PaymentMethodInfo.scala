package oxygen.payments.stripe.model

import java.time.YearMonth
import oxygen.stripe.model.*

final case class PaymentMethodInfo(
    id: StripePaymentMethodId,
    card: Option[CardDisplay],
)

final case class CardDisplay(
    brand: String,
    last4: String,
    expiry: YearMonth,
)
