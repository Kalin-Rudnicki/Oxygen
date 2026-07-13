package oxygen.example.domain.repo

import oxygen.example.core.model.*
import oxygen.example.domain.model.payment.*
import oxygen.storage.CRUDRepo

trait InitPaymentMethodRepo extends CRUDRepo[InitPaymentMethodId, InitPaymentMethod]
