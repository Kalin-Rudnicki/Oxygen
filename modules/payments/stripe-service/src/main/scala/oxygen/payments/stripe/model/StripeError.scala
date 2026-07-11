package oxygen.payments.stripe.model

import oxygen.predef.core.*

sealed trait StripeError extends Error
object StripeError {

  sealed trait SendError extends StripeError

  final case class SendApiError(endpoint: String, error: ApiError) extends SendError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class SendDefect(endpoint: String, error: Throwable) extends SendError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class BuildError(attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ApiError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ApiError

}
