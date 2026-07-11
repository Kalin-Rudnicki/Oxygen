package oxygen.example.db.model

import oxygen.core.model.currency.CurrencyCode
import oxygen.sql.schema.RowRepr
import oxygen.stripe.model.*

given RowRepr[StripeCustomerId] = RowRepr.string.transform(StripeCustomerId.wrap, _.unwrap)
given RowRepr[StripeSetupIntentId] = RowRepr.string.transform(StripeSetupIntentId.wrap, _.unwrap)
given RowRepr[StripePaymentMethodId] = RowRepr.string.transform(StripePaymentMethodId.wrap, _.unwrap)
given RowRepr[StripeSetupIntentClientSecret] = RowRepr.string.transform(StripeSetupIntentClientSecret.wrap, _.unwrap)

given RowRepr[CurrencyCode] = RowRepr.fromPlainTextSchema
