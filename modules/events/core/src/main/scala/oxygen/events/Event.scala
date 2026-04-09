package oxygen.events

trait Event[+S, +K, +V] {
  val source: S
  val key: K
  val value: V
  def headers: Map[String, String]
}
object Event {

  type Sourceless[+K, +V] = Event[Unit, K, V]
  type StringSource[+K, +V] = Event[String, K, V]

  final case class Basic[+S, +K, +V](
      source: S,
      key: K,
      value: V,
      headers: Map[String, String],
  ) extends Event[S, K, V]

  final case class StringKey[+K, +V](
      underlying: Event[?, K, V],
  ) extends Event[String, K, V] {
    override val source: String = underlying.source.toString
    override val key: K = underlying.key
    override val value: V = underlying.value
    override def headers: Map[String, String] = underlying.headers
  }

}
