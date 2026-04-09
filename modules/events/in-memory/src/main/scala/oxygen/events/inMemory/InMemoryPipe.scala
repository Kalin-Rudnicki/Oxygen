package oxygen.events.inMemory

import oxygen.events.*
import zio.*
import zio.stream.*

object InMemoryPipe {

  def layer[K: Tag, V: Tag]: ULayer[EventProducer.Sourceless[K, V] & EventConsumer.StringSource[K, V]] =
    ZLayer.scopedEnvironment {
      for {
        hub <- Hub.unbounded[Event.Sourceless[K, V]]
        _ <- ZIO.addFinalizer(hub.shutdown)
        producer = InMemoryProducer(hub)
        consumer = InMemoryConsumer(hub)
      } yield ZEnvironment(producer, consumer)
    }

  private final case class InMemoryProducer[K, V](hub: Hub[Event.Sourceless[K, V]]) extends EventProducer[Event.Sourceless[K, V]] {
    override def produce(value: Event.Sourceless[K, V]): UIO[Unit] = hub.offer(value).unit
  }

  private final case class InMemoryConsumer[K, V](queue: Hub[Event.Sourceless[K, V]]) extends EventConsumer[Event.StringSource[K, V]] {

    override def stream: EventStream[Any, Throwable, Event.StringSource[K, V]] =
      EventStream(
        "in-memory",
        true,
        ZStream.scoped(queue.subscribe).flatMap(ZStream.fromQueue(_)).map { e => Committable(InMemoryEvent(e), ZIO.unit) },
      )

  }

}
