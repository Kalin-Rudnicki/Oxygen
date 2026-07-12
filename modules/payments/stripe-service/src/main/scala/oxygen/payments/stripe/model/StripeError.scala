package oxygen.payments.stripe.model

import oxygen.core.model.currency.PreciseMoney
import oxygen.predef.core.*

sealed trait StripeError extends Error {
  val target: Option[StripeError.Target]
  def withTarget(target: StripeError.Target): StripeError
}
object StripeError {

  final case class Target(objectType: String, action: String)

  sealed trait SendError extends StripeError {
    override def withTarget(target: StripeError.Target): SendError
  }
  object SendError {

    def apply(error: Throwable): SendError =
      ApiError.from(error) match
        case Some(apiError) => apiError
        case None           => SendDefect(None, error)

  }

  final case class SendDefect(target: Option[Target], error: Throwable) extends SendError {
    override def withTarget(target: Target): SendDefect = copy(target = target.some)
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class BuildError(target: Option[Target], attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def withTarget(target: Target): BuildError = copy(target = target.some)
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  final case class DecodeError(target: Option[Target], attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def withTarget(target: Target): DecodeError = copy(target = target.some)
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) :
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ApiError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ApiError extends SendError {
    override def withTarget(target: StripeError.Target): ApiError
  }
  object ApiError {

    def from(error: Throwable): Option[ApiError] =
      None // FIX-PRE-MERGE (KR) :

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ValidationError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ValidationError extends StripeError {
    override def withTarget(target: StripeError.Target): ValidationError
  }
  
  final case class ChargeNegativeAmount(target: Option[Target], amount: PreciseMoney) extends ValidationError {
    override def withTarget(target: Target): ChargeNegativeAmount = copy(target = target.some)
    override def errorMessage: Text = ??? // FIX-PRE-MERGE (KR) : 
  }

}
