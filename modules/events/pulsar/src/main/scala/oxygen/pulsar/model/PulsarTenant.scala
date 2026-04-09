package oxygen.pulsar.model

final case class PulsarTenant(
    tenant: String,
)
object PulsarTenant {

  given Ordering[PulsarTenant] =
    Ordering.by[PulsarTenant, String](_.tenant)

}
