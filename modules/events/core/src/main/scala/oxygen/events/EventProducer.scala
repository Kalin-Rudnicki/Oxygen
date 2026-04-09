package oxygen.events

import zio.*

trait EventProducer[-A] {

  def produce(value: A): UIO[Unit]

  final def contramap[B](f: B => A): EventProducer[B] = EventProducer.Contramapped(this, f)

}
object EventProducer {

  final case class Contramapped[A, B](a: EventProducer[A], transform: B => A) extends EventProducer[B] {
    override def produce(value: B): UIO[Unit] = a.produce(transform(value))
  }

}
