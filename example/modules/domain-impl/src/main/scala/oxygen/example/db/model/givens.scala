package oxygen.example.db.model

import oxygen.sql.schema.RowRepr
import oxygen.stripe.model.StripeCustomerId

given RowRepr[StripeCustomerId] = RowRepr.string.transform(StripeCustomerId.wrap, _.unwrap)
