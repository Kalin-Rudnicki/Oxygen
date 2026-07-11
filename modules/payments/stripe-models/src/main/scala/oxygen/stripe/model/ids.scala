package oxygen.stripe.model

import oxygen.schema.{JsonSchema, PlainTextSchema}

opaque type StripeCustomerId = String
object StripeCustomerId {

  def wrap(value: String): StripeCustomerId = value
  extension (self: StripeCustomerId) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripeCustomerId] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripeCustomerId] = JsonSchema.fromPlainText[StripeCustomerId].secret

}

opaque type StripeSetupIntentId = String
object StripeSetupIntentId {

  def wrap(value: String): StripeSetupIntentId = value
  extension (self: StripeSetupIntentId) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripeSetupIntentId] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripeSetupIntentId] = JsonSchema.fromPlainText[StripeSetupIntentId].secret

}

opaque type StripePaymentIntentId = String
object StripePaymentIntentId {

  def wrap(value: String): StripePaymentIntentId = value
  extension (self: StripePaymentIntentId) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripePaymentIntentId] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripePaymentIntentId] = JsonSchema.fromPlainText[StripePaymentIntentId].secret

}

opaque type StripePaymentMethodId = String
object StripePaymentMethodId {

  def wrap(value: String): StripePaymentMethodId = value
  extension (self: StripePaymentMethodId) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripePaymentMethodId] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripePaymentMethodId] = JsonSchema.fromPlainText[StripePaymentMethodId].secret

}
