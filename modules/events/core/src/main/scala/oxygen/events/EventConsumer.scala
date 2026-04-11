package oxygen.events

import zio.*

trait EventConsumer[+A] {

  def consume[R](f: A => URIO[R, Unit]): URIO[R, Unit]

  // TODO (KR) :
  // def filter(f: A => Boolean): EventConsumer[A]

  final def map[B](f: A => B): EventConsumer[B] = EventConsumer.Mapped(this, f)

}
object EventConsumer {

  type Sourceless[+K, +V] = EventConsumer[Event.Sourceless[K, V]]

  final case class Mapped[A, B](a: EventConsumer[A], transform: A => B) extends EventConsumer[B] {
    override def consume[R](f: B => URIO[R, Unit]): URIO[R, Unit] = a.consume(v => f(transform(v)))
  }

  def mapLayer[A: Tag, B: Tag](f: A => B): URLayer[EventConsumer[A], EventConsumer[B]] =
    ZLayer.service[EventConsumer[A]].project(_.map(f))

}
