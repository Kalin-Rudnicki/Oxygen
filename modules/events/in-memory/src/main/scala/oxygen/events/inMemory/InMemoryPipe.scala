package oxygen.events.inMemory

import oxygen.events.*
import zio.*

object InMemoryPipe {

  def layer[K: Tag, V: Tag]: ULayer[EventProducer.Sourceless[K, V] & EventConsumer.Sourceless[K, V]] =
    ZLayer.fromZIOEnvironment {
      for {
        queue <- Queue.unbounded[Event.Sourceless[K, V]]
        producer = InMemoryProducer(queue)
        consumer = InMemoryConsumer(queue)
      } yield ZEnvironment(producer, consumer)
    }

  private final case class InMemoryProducer[K, V](queue: Enqueue[Event.Sourceless[K, V]]) extends EventProducer[Event.Sourceless[K, V]] {
    override def produce(value: Event.Sourceless[K, V]): UIO[Unit] = queue.offer(value).unit
  }

  private final case class InMemoryConsumer[K, V](queue: Dequeue[Event.Sourceless[K, V]]) extends EventConsumer[Event.Sourceless[K, V]] {
    override def consume[R](f: Event.Sourceless[K, V] => URIO[R, Unit]): URIO[R, Unit] = queue.take.flatMap(f).forever
  }

}
