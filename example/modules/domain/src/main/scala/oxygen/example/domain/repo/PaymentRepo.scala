package oxygen.example.domain.repo

import oxygen.example.core.model.*
import oxygen.example.domain.model.payment.*
import oxygen.storage.CRUDRepo
import zio.*

trait PaymentRepo extends CRUDRepo[PaymentId, Payment] {

  def paymentsForUser(userId: UserId): UIO[Seq[Payment]]

}
