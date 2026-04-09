package oxygen.events

final case class Event[+S, +K, +V](
    source: S,
    key: K,
    value: V,
    headers: Map[String, String],
)
