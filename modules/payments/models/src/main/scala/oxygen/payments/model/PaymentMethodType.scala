package oxygen.payments.model

import java.time.YearMonth
import oxygen.json.jsonDiscriminator
import oxygen.predef.core.*
import oxygen.schema.JsonSchema

/**
  * Vaulted payment method.
  *
  * Apple Pay / Google Pay are not separate PM types — they are `type=card` with a wallet.
  * PayPal is `type=paypal`.
  */
@jsonDiscriminator("methodType")
sealed trait PaymentMethodType derives JsonSchema
object PaymentMethodType {

  /** Shared display fields for card-backed methods (plain card or wallet). */
  sealed trait CardLike extends PaymentMethodType {
    val brand: String
    val last4: String
    val expiry: YearMonth
    val funding: CardFunding
  }

  /** `type=card` without a digital wallet. */
  final case class Card(
      brand: String,
      last4: String,
      expiry: YearMonth,
      funding: CardFunding,
  ) extends CardLike

  /** `type=card` collected via a digital wallet (`card.wallet`). */
  final case class WalletCard(
      brand: String,
      last4: String,
      expiry: YearMonth,
      funding: CardFunding,
      wallet: CardWallet,
  ) extends CardLike

  /** `card.funding`: credit / debit / prepaid / unknown. */
  enum CardFunding derives EnumWithOther {
    case Credit
    case Debit
    case Prepaid
    case Unknown
    case Other(otherType: String)
  }

  /** `type=paypal`. */
  final case class PayPal(
      payerEmail: Option[String],
      payerId: Option[String],
      country: Option[String],
  ) extends PaymentMethodType

  /** Any other payment method type not modeled yet. */
  final case class Other(
      typeName: String,
  ) extends PaymentMethodType

  /** Wallet used when collecting a card (`card.wallet.type`). */
  enum CardWallet derives EnumWithOther {
    case GooglePay
    case ApplePay
    case Link
    case SamsungPay
    case Other(otherType: String)
  }

}
