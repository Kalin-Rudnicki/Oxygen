package oxygen.events

final case class Event[+S, +K, +V](
    source: S,
    key: K,
    value: V,
    headers: Map[String, String],
)
object Event {
  
  type Sourceless[+K, +V] = Event[Unit, K, V]
  
}
