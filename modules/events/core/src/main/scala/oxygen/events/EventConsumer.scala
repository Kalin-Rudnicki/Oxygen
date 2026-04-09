package oxygen.events

import zio.*

trait EventConsumer[+A] {

  def stream: EventStream[Any, Throwable, A]

  final def map[B](f: A => B): EventConsumer[B] = EventConsumer.Mapped(this, f)

}
object EventConsumer {

  type StringSource[+K, +V] = EventConsumer[Event[String, K, V]]

  final case class Mapped[A, B](a: EventConsumer[A], transform: A => B) extends EventConsumer[B] {
    override def stream: EventStream[Any, Throwable, B] = a.stream.map(transform)
  }

  def mapLayer[A: Tag, B: Tag](f: A => B): URLayer[EventConsumer[A], EventConsumer[B]] =
    ZLayer.service[EventConsumer[A]].project(_.map(f))

}
