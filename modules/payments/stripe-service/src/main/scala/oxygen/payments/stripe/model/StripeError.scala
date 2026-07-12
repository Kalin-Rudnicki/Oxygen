package oxygen.payments.stripe.model

import com.stripe.exception as SE
import oxygen.core.model.currency.PreciseMoney
import oxygen.predef.core.*

sealed trait StripeError extends Error {
  val target: Option[StripeError.Target]
  def withTarget(target: StripeError.Target): StripeError

  protected def targetlessErrorMessage: Text

  override final def errorMessage: Text = target match
    case Some(t) => str"""[${t.objectType}/${t.action}] $targetlessErrorMessage"""
    case None    => targetlessErrorMessage

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
    override def errorMessage: Text =
      str"Stripe send defect${targetSuffix(target)}: ${throwableMessage(error)}"
    override def causes: ArraySeq[Error] = ArraySeq(Error.fromThrowable(error))
  }

  final case class BuildError(target: Option[Target], attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def withTarget(target: Target): BuildError = copy(target = target.some)
    override def errorMessage: Text =
      str"Stripe build error${targetSuffix(target)} while building ${attemptType.prefixObject}: ${throwableMessage(error)}"
    override def causes: ArraySeq[Error] = ArraySeq(Error.fromThrowable(error))
  }

  final case class DecodeError(target: Option[Target], attemptType: TypeTag[?], error: Throwable) extends StripeError {
    override def withTarget(target: Target): DecodeError = copy(target = target.some)
    override def errorMessage: Text =
      str"Stripe decode error${targetSuffix(target)} while decoding ${attemptType.prefixObject}: ${throwableMessage(error)}"
    override def causes: ArraySeq[Error] = ArraySeq(Error.fromThrowable(error))
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ApiError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Typed failures from stripe-java (`com.stripe.exception.*`).
    * Populated via [[ApiError.from]] / [[SendError.apply]].
    */
  sealed trait ApiError extends SendError {
    val message: String
    val code: Option[String]
    val requestId: Option[String]
    val statusCode: Option[Int]
    override def withTarget(target: StripeError.Target): ApiError
  }
  object ApiError {

    final case class Card(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
        declineCode: Option[String],
        param: Option[String],
        charge: Option[String],
    ) extends ApiError {
      override def withTarget(target: Target): Card = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe card error${targetSuffix(target)}${detailSuffix(this)}${optField("decline_code", declineCode)}"
    }

    final case class InvalidRequest(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
        param: Option[String],
    ) extends ApiError {
      override def withTarget(target: Target): InvalidRequest = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe invalid request${targetSuffix(target)}${detailSuffix(this)}${optField("param", param)}"
    }

    final case class Authentication(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
    ) extends ApiError {
      override def withTarget(target: Target): Authentication = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe authentication error${targetSuffix(target)}${detailSuffix(this)}"
    }

    final case class Permission(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
    ) extends ApiError {
      override def withTarget(target: Target): Permission = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe permission error${targetSuffix(target)}${detailSuffix(this)}"
    }

    final case class RateLimit(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
        param: Option[String],
    ) extends ApiError {
      override def withTarget(target: Target): RateLimit = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe rate limit${targetSuffix(target)}${detailSuffix(this)}${optField("param", param)}"
    }

    final case class Idempotency(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
    ) extends ApiError {
      override def withTarget(target: Target): Idempotency = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe idempotency error${targetSuffix(target)}${detailSuffix(this)}"
    }

    final case class ApiConnection(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
    ) extends ApiError {
      override def withTarget(target: Target): ApiConnection = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe connection error${targetSuffix(target)}${detailSuffix(this)}"
    }

    final case class SignatureVerification(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
        sigHeader: Option[String],
    ) extends ApiError {
      override def withTarget(target: Target): SignatureVerification = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe signature verification error${targetSuffix(target)}${detailSuffix(this)}"
    }

    /** [[SE.ApiException]] and any other [[SE.StripeException]] subclass not mapped above. */
    final case class Generic(
        target: Option[Target],
        message: String,
        code: Option[String],
        requestId: Option[String],
        statusCode: Option[Int],
        exceptionType: String,
    ) extends ApiError {
      override def withTarget(target: Target): Generic = copy(target = target.some)
      override def errorMessage: Text =
        str"Stripe API error ($exceptionType)${targetSuffix(target)}${detailSuffix(this)}"
    }

    /**
      * Map stripe-java exceptions into [[ApiError]].
      * Non-Stripe throwables return [[None]] (become [[SendDefect]]).
      *
      * Match order matters: e.g. [[SE.PermissionException]] <: [[SE.AuthenticationException]].
      */
    def from(error: Throwable): Option[ApiError] =
      error match {
        case e: SE.CardException =>
          Card(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
            declineCode = optStr(e.getDeclineCode),
            param = optStr(e.getParam),
            charge = optStr(e.getCharge),
          ).some

        case e: SE.RateLimitException =>
          RateLimit(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
            param = optStr(e.getParam),
          ).some

        case e: SE.IdempotencyException =>
          Idempotency(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
          ).some

        case e: SE.InvalidRequestException =>
          InvalidRequest(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
            param = optStr(e.getParam),
          ).some

        case e: SE.PermissionException =>
          Permission(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
          ).some

        case e: SE.AuthenticationException =>
          Authentication(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
          ).some

        case e: SE.SignatureVerificationException =>
          SignatureVerification(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
            sigHeader = optStr(e.getSigHeader),
          ).some

        case e: SE.ApiConnectionException =>
          ApiConnection(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
          ).some

        case e: SE.StripeException =>
          Generic(
            target = None,
            message = stripeMessage(e),
            code = optStr(e.getCode),
            requestId = optStr(e.getRequestId),
            statusCode = optInt(e.getStatusCode),
            exceptionType = e.getClass.getSimpleName,
          ).some

        case _ =>
          None
      }

    private def stripeMessage(e: SE.StripeException): String =
      Option(e.getUserMessage)
        .orElse(Option(e.getMessage))
        .filter(_.nonEmpty)
        .getOrElse(e.getClass.getSimpleName)

    private def detailSuffix(e: ApiError): String = {
      val parts =
        List(
          s": ${e.message}",
          e.code.fold("")(c => s"; code=$c"),
          e.requestId.fold("")(id => s"; request-id=$id"),
          e.statusCode.fold("")(s => s"; status=$s"),
        )
      parts.mkString
    }

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      ValidationError
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait ValidationError extends StripeError {
    override def withTarget(target: StripeError.Target): ValidationError
  }

  final case class ChargeNegativeAmount(target: Option[Target], amount: PreciseMoney) extends ValidationError {
    override def withTarget(target: Target): ChargeNegativeAmount = copy(target = target.some)
    override def errorMessage: Text =
      str"Refusing to create PaymentIntent with non-positive amount${targetSuffix(target)}: $amount"
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Helpers
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def targetSuffix(target: Option[Target]): String =
    target.fold("")(t => s" [${t.objectType}/${t.action}]")

  private def optField(name: String, value: Option[String]): String =
    value.fold("")(v => s"; $name=$v")

  private def optStr(value: String): Option[String] =
    Option(value)

  private def optInt(value: Integer): Option[Int] =
    Option(value).map(_.intValue)

  private def throwableMessage(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)

}
