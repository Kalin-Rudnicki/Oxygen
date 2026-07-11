package oxygen.stripe.api

import oxygen.http.core.*
import zio.*

trait StripeApi {

  // FIX-PRE-MERGE (KR) :
  @route.get("/todo")
  def todo(): IO[String, String]

}
