package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.schema.instances.given
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*
import oxygen.stripe.model.*

@tableName("payment")
final case class PaymentRow(
    @primaryKey id: PaymentId,
    userId: UserId,
    paymentMethodId: PaymentMethodId,
    stripeId: StripePaymentIntentId,
    @inlineColumnNames amount: PreciseMoneyColumn,
    description: String,
    status: String,
    createdAt: Instant,
)
object PaymentRow extends TableCompanion[PaymentRow, PaymentId](TableRepr.derived[PaymentRow]) {

  @compile
  val byUserId: QueryIO[UserId, PaymentRow] =
    for {
      userId <- input[UserId]
      pm <- select[PaymentRow]
      _ <- where if pm.userId == userId
    } yield pm

}
