package oxygen.events.pulsar

final case class PulsarSource(
    persistent: Boolean,
    tenant: String,
    namespace: String,
    topic: String,
)
