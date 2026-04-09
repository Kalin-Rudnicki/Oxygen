package oxygen.events

import zio.*

trait EventConsumer[+A] {

  def consume[R](f: A => URIO[R, Unit]): URIO[R, Unit]

  final def map[B](f: A => B): EventConsumer[B] = EventConsumer.Mapped(this, f)

}
object EventConsumer {

  final case class Mapped[A, B](a: EventConsumer[A], transform: A => B) extends EventConsumer[B] {
    override def consume[R](f: B => URIO[R, Unit]): URIO[R, Unit] = a.consume(v => f(transform(v)))
  }

}
