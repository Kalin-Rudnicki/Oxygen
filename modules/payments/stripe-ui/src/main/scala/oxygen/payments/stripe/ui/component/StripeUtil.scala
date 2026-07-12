package oxygen.payments.stripe.ui.component

import oxygen.payments.stripe.ui.facades as F
import oxygen.predef.core.*
import oxygen.ui.web.UIError
import scala.scalajs.js
import zio.*

object StripeUtil {

  def convertStripeError(stripeError: F.StripeJsError): UIO[UIError] =
    ZIO.logError(s"Stripe Error: $stripeError").as {
      (undefToOption(stripeError.param), undefToOption(stripeError.message)) match
        case (Some(param), Some(message)) => UIError.userError(s"$param : $message")
        case (None, Some(message))        => UIError.userError(message)
        case (_, None)                    => UIError.ClientSide.InternalDefect.somethingWentWrong(s"Stripe error : $stripeError")
    }

  def convertPromise[A](
      promise: => js.Promise[A],
      getError: A => js.UndefOr[F.StripeJsError],
  ): IO[UIError, A] =
    ZIO
      .fromPromiseJS { promise }
      .mapError { e => UIError.ClientSide.InternalDefect.somethingWentWrong(e) }
      .tap { res => ZIO.foreachDiscard(undefToOption(getError(res))) { convertStripeError(_).flatMap(ZIO.fail(_)) } }

  def convertPromiseGet[A, B](
      promise: => js.Promise[A],
      getError: A => js.UndefOr[F.StripeJsError],
      getSuccess: A => js.UndefOr[B],
  ): IO[UIError, B] =
    convertPromiseGet(promise, getError, getSuccess, missing = None)

  def convertPromiseGet[A, B](
      promise: => js.Promise[A],
      getError: A => js.UndefOr[F.StripeJsError],
      getSuccess: A => js.UndefOr[B],
      missing: String,
  ): IO[UIError, B] =
    convertPromiseGet(promise, getError, getSuccess, missing = missing.some)

  def convertPromiseGet[A, B](
      promise: => js.Promise[A],
      getError: A => js.UndefOr[F.StripeJsError],
      getSuccess: A => js.UndefOr[B],
      missing: Option[String],
  ): IO[UIError, B] =
    convertPromise[A](promise, getError).flatMap { res =>
      undefToOption(getSuccess(res)) match {
        case Some(value) => ZIO.succeed(value)
        case None        =>
          missing match {
            case Some(missing) => ZIO.fail(UIError.ClientSide.InternalDefect(missing, res.toString.some))
            case None          => ZIO.fail(UIError.ClientSide.InternalDefect.somethingWentWrong(res.toString))
          }
      }
    }

  private[component] def undefToOption[A](value: js.UndefOr[A]): Option[A] =
    if js.isUndefined(value) then None
    else Some(value.asInstanceOf[A])

}
