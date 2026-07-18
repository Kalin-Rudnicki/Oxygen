package oxygen.example.db.model

import oxygen.core.model.currency.CurrencyCode
import oxygen.sql.schema.RowRepr

final case class PreciseMoneyColumn(
    amount: BigDecimal,
    currency: CurrencyCode,
) derives RowRepr.ProductRepr
