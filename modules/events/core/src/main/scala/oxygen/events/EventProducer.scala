package oxygen.events

import zio.*

trait EventProducer[-A] {

  def produce(value: A): UIO[Unit]

  final def contramap[B](f: B => A): EventProducer[B] = EventProducer.Contramapped(this, f)

}
object EventProducer {

  type Sourceless[-K, -V] = EventProducer[Event.Sourceless[K, V]]

  final case class Contramapped[A, B](a: EventProducer[A], transform: B => A) extends EventProducer[B] {
    override def produce(value: B): UIO[Unit] = a.produce(transform(value))
  }

  def contramapLayer[A: Tag, B: Tag](f: B => A): URLayer[EventProducer[A], EventProducer[B]] =
    ZLayer.service[EventProducer[A]].project(_.contramap(f))

}
