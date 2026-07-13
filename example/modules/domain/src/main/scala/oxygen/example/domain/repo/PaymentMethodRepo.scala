package oxygen.example.domain.repo

import oxygen.example.core.model.*
import oxygen.example.domain.model.payment.*
import oxygen.storage.CRUDRepo
import zio.*

trait PaymentMethodRepo extends CRUDRepo[PaymentMethodId, PaymentMethod] {

  def paymentMethodsForUser(userId: UserId): UIO[Seq[PaymentMethod]]

}
