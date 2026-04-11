package oxygen.events.pulsar

import oxygen.events.*
import zio.*

final case class PulsarEventProducer[K, V] private () extends EventProducer[Event[PulsarSource, K, V]] {

  override def produce(value: Event[PulsarSource, K, V]): UIO[Unit] =
    ??? // FIX-PRE-MERGE (KR) :

}
object PulsarEventProducer {

  // def layer[K: Tag, V: Tag]

}
