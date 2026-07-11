package oxygen.example.db.model

import java.time.Instant
import oxygen.example.core.model.*
import oxygen.sql.query.*
import oxygen.sql.query.dsl.Q.*
import oxygen.sql.query.dsl.compile
import oxygen.sql.schema.*
import oxygen.stripe.model.*

@tableName("init_payment_method")
final case class InitPaymentMethodRow(
    @primaryKey id: InitPaymentMethodId,
    userId: UserId,
    stripeId: StripeSetupIntentId,
    clientSecret: StripeSetupIntentClientSecret,
    createdAt: Instant,
    completedAt: Option[Instant],
)
object InitPaymentMethodRow extends TableCompanion[InitPaymentMethodRow, InitPaymentMethodId](TableRepr.derived[InitPaymentMethodRow]) {

  @compile
  val byUserId: QueryIO[UserId, InitPaymentMethodRow] =
    for {
      userId <- input[UserId]
      ipm <- select[InitPaymentMethodRow]
      _ <- where if ipm.userId == userId
    } yield ipm

}
