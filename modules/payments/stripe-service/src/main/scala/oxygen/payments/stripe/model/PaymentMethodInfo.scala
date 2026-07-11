package oxygen.payments.stripe.model

import oxygen.payments.model.*
import oxygen.stripe.model.*

final case class PaymentMethodInfo(
    id: StripePaymentMethodId,
    methodType: PaymentMethodType,
)
object PaymentMethodInfo {

  /** Stripe `card.funding` → core [[PaymentMethodType.CardFunding]]. */
  def parseCardFunding(value: String): PaymentMethodType.CardFunding =
    Option(value).map(_.toLowerCase) match {
      case Some("credit")  => PaymentMethodType.CardFunding.Credit
      case Some("debit")   => PaymentMethodType.CardFunding.Debit
      case Some("prepaid") => PaymentMethodType.CardFunding.Prepaid
      case Some("unknown") => PaymentMethodType.CardFunding.Unknown
      case Some(otherType) => PaymentMethodType.CardFunding.Other(otherType)
      case None            => PaymentMethodType.CardFunding.Unknown
    }

  /** Stripe `card.wallet.type` → core [[PaymentMethodType.CardWallet]]. */
  def parseCardWallet(typeName: String): PaymentMethodType.CardWallet =
    typeName match {
      case "google_pay"  => PaymentMethodType.CardWallet.GooglePay
      case "apple_pay"   => PaymentMethodType.CardWallet.ApplePay
      case "link"        => PaymentMethodType.CardWallet.Link
      case "samsung_pay" => PaymentMethodType.CardWallet.SamsungPay
      case otherType     => PaymentMethodType.CardWallet.Other(otherType)
    }

}
