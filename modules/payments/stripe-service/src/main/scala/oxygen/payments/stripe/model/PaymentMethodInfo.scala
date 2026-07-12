package oxygen.payments.stripe.model

import java.time.YearMonth
import oxygen.stripe.model.*

/**
  * How this payment method presents to the user / what Stripe `type` it is.
  *
  * Apple Pay / Google Pay are not separate PM types — they are card PMs with a wallet.
  * PayPal is `type = "paypal"`.
  */
sealed trait PaymentMethodInfo {
  val id: StripePaymentMethodId
}
object PaymentMethodInfo {

  /**
    * Stripe `type = "card"`.
    * @param wallet set when the card was collected via a digital wallet (Apple Pay, Google Pay, …)
    */
  final case class Card(
      brand: String,
      last4: String,
      expiry: YearMonth,
      wallet: Option[CardWallet],
  ) extends PaymentMethodInfo

  /** Stripe `type = "paypal"`. */
  final case class PayPal(
      payerEmail: Option[String],
      payerId: Option[String],
      country: Option[String],
  ) extends PaymentMethodInfo

  /** Any other Stripe payment method type we don't model yet. */
  final case class Other(typeName: String) extends PaymentMethodInfo

}

/** Wallet used when collecting a card (Stripe `card.wallet.type`). */
sealed trait CardWallet
object CardWallet {

  case object GooglePay extends CardWallet
  case object ApplePay extends CardWallet
  case object Link extends CardWallet
  case object SamsungPay extends CardWallet

  final case class Other(typeName: String) extends CardWallet

  def fromStripeType(typeName: String): CardWallet =
    typeName match {
      case "google_pay"  => GooglePay
      case "apple_pay"   => ApplePay
      case "link"        => Link
      case "samsung_pay" => SamsungPay
      case other         => Other(other)
    }

}
