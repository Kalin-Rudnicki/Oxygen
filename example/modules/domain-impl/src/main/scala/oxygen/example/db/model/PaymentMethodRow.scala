package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.payments.model.PaymentMethodType
import oxygen.schema.instances.given
import oxygen.sql.model.TypedJsonb
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*
import oxygen.stripe.model.*

@tableName("payment_method")
final case class PaymentMethodRow(
    @primaryKey id: PaymentMethodId,
    userId: UserId,
    stripeId: StripePaymentMethodId,
    name: Option[String],
    repr: TypedJsonb[PaymentMethodType],
    ord: Int,
    createdAt: Instant,
)
object PaymentMethodRow extends TableCompanion[PaymentMethodRow, PaymentMethodId](TableRepr.derived[PaymentMethodRow]) {

  @compile
  val byUserId: QueryIO[UserId, PaymentMethodRow] =
    for {
      userId <- input[UserId]
      pm <- select[PaymentMethodRow]
      _ <- where if pm.userId == userId
    } yield pm

}
