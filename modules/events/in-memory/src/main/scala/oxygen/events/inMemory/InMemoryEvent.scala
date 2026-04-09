package oxygen.events.inMemory

import oxygen.events.*

final case class InMemoryEvent[+K, +V](
    underlying: Event[Unit, K, V],
) extends Event[String, K, V] {
  override val source: String = "in-memory"
  override val key: K = underlying.key
  override val value: V = underlying.value
  override def headers: Map[String, String] = underlying.headers
}
