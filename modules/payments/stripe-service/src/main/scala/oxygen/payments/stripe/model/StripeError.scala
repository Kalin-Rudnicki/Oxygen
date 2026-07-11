package oxygen.payments.stripe.model

import oxygen.predef.core.*

sealed trait StripeError extends Error
object StripeError {

  sealed trait SendError extends StripeError
  object SendError {

    def apply(objectType: String, action: String, error: Throwable): SendError =
      ApiError.from(error) match
        case Some(apiError) => SendApiError(objectType, action, apiError)
        case None           => SendDefect(objectType, action, error)

  }

  final case class SendApiError(objectType: String, action: String, error: ApiError) extends SendError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class SendDefect(objectType: String, action: String, error: Throwable) extends SendError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class BuildError(attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ApiError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  // FIX-PRE-MERGE (KR) :
  sealed trait ApiError
  object ApiError {

    def from(error: Throwable): Option[ApiError] =
      None // FIX-PRE-MERGE (KR) :

  }

}
