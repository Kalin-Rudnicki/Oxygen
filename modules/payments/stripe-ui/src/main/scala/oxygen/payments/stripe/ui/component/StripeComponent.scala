package oxygen.payments.stripe.ui.component

import org.scalajs.dom.window
import oxygen.core.model.currency.CurrencyCode
import oxygen.payments.stripe.ui.facades as F
import oxygen.payments.stripe.ui.service.StripeService
import oxygen.stripe.model.*
import oxygen.ui.web.{PWidget, UIError}
import oxygen.ui.web.create.*
import scala.scalajs.js
import zio.*

final class StripeComponent private (
    private val element: StripeForeignElement,
) {

  /**
    * Full save-card flow: validate Payment Element, then `confirmSetup`.
    * Expects an in-page completion (`redirect: "if_required"`). Fails if Stripe
    * returns an error, no SetupIntent, or a non-succeeded status.
    */
  def submit: IO[UIError, SetupConfirmResult] =
    validateSubmit *> confirmSetup

  /////// Helpers ///////////////////////////////////////////////////////////////

  /**
    * Validate the Payment Element (and collect wallet data if needed).
    * Does not confirm the SetupIntent.
    */
  private def validateSubmit: IO[UIError, Unit] =
    ZIO
      .fromPromiseJS { element.elements.submit() }
      .flatMapError { t =>
        val message: String = Option(t.getMessage).filter(_.nonEmpty).getOrElse("Failed to submit payment details")
        val error: UIError = UIError.ClientSide.UserActionable(message)
        ZIO.logError(s"Stripe Error:\n$t").as(error)
      }
      .flatMap { result =>
        undefToOption(result.error) match {
          case None =>
            ZIO.unit
          case Some(err) =>
            val message: String = undefToOption(err.message).filter(_.nonEmpty).getOrElse("Invalid payment details")
            ZIO.fail(UIError.ClientSide.UserActionable(message))
        }
      }

  private def confirmSetup: IO[UIError, SetupConfirmResult] = {
    val returnUrl = window.location.href
    val options =
      F.ConfirmSetupOptions.withElements(
        stripeElements = element.elements,
        secret = element.clientSecret.unwrap,
        returnUrl = returnUrl,
        redirectMode = "if_required",
      )

    ZIO
      .fromPromiseJS { element.stripe.confirmSetup(options) }
      .flatMapError { t =>
        val message: String = Option(t.getMessage).filter(_.nonEmpty).getOrElse("Failed to confirm payment method setup")
        val error: UIError = UIError.ClientSide.UserActionable(message)
        ZIO.logError(s"Stripe Error:\n$t").as(error)
      }
      .flatMap { result =>
        undefToOption(result.error) match {
          case Some(err) =>
            val message: String = undefToOption(err.message).filter(_.nonEmpty).getOrElse("Unable to save payment method")
            ZIO.fail(UIError.ClientSide.UserActionable(message))
          case None =>
            undefToOption(result.setupIntent) match {
              case None =>
                ZIO.fail(
                  UIError.ClientSide.UserActionable(
                    "Payment method setup did not complete in-page. This method may require a redirect, which is not supported yet.",
                  ),
                )
              case Some(si) =>
                decodeSetupIntent(si)
            }
        }
      }
  }

  private def decodeSetupIntent(si: F.JsSetupIntent): IO[UIError, SetupConfirmResult] = {
    val status = si.status
    val paymentMethodId = paymentMethodIdFrom(si.payment_method)

    status match {
      case "succeeded" =>
        ZIO.succeed(
          SetupConfirmResult(
            setupIntentId = StripeSetupIntentId.wrap(si.id),
            status = status,
            paymentMethodId = paymentMethodId,
          ),
        )
      case other =>
        ZIO.fail(
          UIError.ClientSide.UserActionable(
            s"Payment method setup did not succeed (status=$other). Redirect or additional steps may be required.",
          ),
        )
    }
  }

  private def paymentMethodIdFrom(value: js.UndefOr[String | js.Object]): Option[StripePaymentMethodId] =
    undefToOption(value).flatMap { v =>
      v.asInstanceOf[Matchable] match {
        case s: String if s.nonEmpty => Some(StripePaymentMethodId.wrap(s))
        case _                       => None
      }
    }

  private def undefToOption[A](value: js.UndefOr[A]): Option[A] =
    if js.isUndefined(value) then None
    else Some(value.asInstanceOf[A])

}
object StripeComponent {

  lazy val widget: WidgetS[StripeComponent] =
    Widget.state[StripeComponent].get { stripeState =>
      PWidget.ForeignHost.empty("div", "stripe-host", stripeState.element)
    }

  /** Meant to be called on page init */
  def create(
      publishableKey: StripePublishableKey,
      clientSecret: StripeSetupIntentClientSecret,
      currency: CurrencyCode,
      appearance: F.StripeAppearance,
      elementOptions: F.StripeElementOptions,
  ): ZIO[StripeService & Scope, UIError, StripeComponent] =
    for {
      stripe <- ZIO.service[StripeService]
      element <- stripe.create(
        publishableKey = publishableKey,
        clientSecret = clientSecret,
        currency = currency,
        appearance = appearance,
        elementOptions = elementOptions,
      )
    } yield new StripeComponent(element)

}
