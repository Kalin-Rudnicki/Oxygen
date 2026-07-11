package oxygen.stripe.model

import oxygen.schema.{JsonSchema, PlainTextSchema}

opaque type StripePublishableKey = String
object StripePublishableKey {

  def wrap(value: String): StripePublishableKey = value
  extension (self: StripePublishableKey) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripePublishableKey] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripePublishableKey] = JsonSchema.fromPlainText[StripePublishableKey].secret

}

opaque type StripeSecretKey = String
object StripeSecretKey {

  def wrap(value: String): StripeSecretKey = value
  extension (self: StripeSecretKey) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripeSecretKey] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripeSecretKey] = JsonSchema.fromPlainText[StripeSecretKey].secret

}

opaque type StripeWebhookSigningSecret = String
object StripeWebhookSigningSecret {

  def wrap(value: String): StripeWebhookSigningSecret = value
  extension (self: StripeWebhookSigningSecret) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripeWebhookSigningSecret] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripeWebhookSigningSecret] = JsonSchema.fromPlainText[StripeWebhookSigningSecret].secret

}

opaque type StripeSetupIntentClientSecret = String
object StripeSetupIntentClientSecret {

  def wrap(value: String): StripeSetupIntentClientSecret = value
  extension (self: StripeSetupIntentClientSecret) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripeSetupIntentClientSecret] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripeSetupIntentClientSecret] = JsonSchema.fromPlainText[StripeSetupIntentClientSecret].secret

}

opaque type StripePaymentIntentClientSecret = String
object StripePaymentIntentClientSecret {

  def wrap(value: String): StripePaymentIntentClientSecret = value
  extension (self: StripePaymentIntentClientSecret) def unwrap: String = self

  given plainTextSchema: PlainTextSchema[StripePaymentIntentClientSecret] = PlainTextSchema.string
  given jsonSchema: JsonSchema[StripePaymentIntentClientSecret] = JsonSchema.fromPlainText[StripePaymentIntentClientSecret].secret

}
