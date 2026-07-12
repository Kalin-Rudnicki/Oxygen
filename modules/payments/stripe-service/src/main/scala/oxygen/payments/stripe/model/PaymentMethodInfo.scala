package oxygen.payments.stripe.model

import java.time.YearMonth
import oxygen.stripe.model.*

/**
  * Vaulted payment method from Stripe.
  *
  * Apple Pay / Google Pay are not separate PM types — they are `type=card` with a wallet.
  * PayPal is `type=paypal`.
  */
sealed trait PaymentMethodInfo {
  val id: StripePaymentMethodId
}
object PaymentMethodInfo {

  /** Shared display fields for card-backed methods (plain card or wallet). */
  sealed trait CardLike extends PaymentMethodInfo {
    val brand: String
    val last4: String
    val expiry: YearMonth
    val funding: CardFunding
  }

  /** Stripe `type=card` without a digital wallet. */
  final case class Card(
      id: StripePaymentMethodId,
      brand: String,
      last4: String,
      expiry: YearMonth,
      funding: CardFunding,
  ) extends CardLike

  /** Stripe `type=card` collected via a digital wallet (`card.wallet`). */
  final case class WalletCard(
      id: StripePaymentMethodId,
      brand: String,
      last4: String,
      expiry: YearMonth,
      funding: CardFunding,
      wallet: CardWallet,
  ) extends CardLike

  /** Stripe `card.funding`: credit / debit / prepaid / unknown. */
  enum CardFunding {
    case Credit, Debit, Prepaid, Unknown
  }
  object CardFunding {

    def fromStripe(value: String): CardFunding =
      Option(value).map(_.toLowerCase) match {
        case Some("credit")  => Credit
        case Some("debit")   => Debit
        case Some("prepaid") => Prepaid
        case _               => Unknown
      }

  }

  /** Stripe `type=paypal`. */
  final case class PayPal(
      id: StripePaymentMethodId,
      payerEmail: Option[String],
      payerId: Option[String],
      country: Option[String],
  ) extends PaymentMethodInfo

  /** Any other Stripe payment method type not modeled yet. */
  final case class Other(
      id: StripePaymentMethodId,
      typeName: String,
  ) extends PaymentMethodInfo

  /** Wallet used when collecting a card (`card.wallet.type`). */
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

}
