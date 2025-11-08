package oxygen.zio.syntax

import zio.*

object queue {

  extension [A](inQueue: Dequeue[A]) {

    def map[B](f: A => B): URIO[Scope, Dequeue[B]] =
      Queue.unbounded[B].tap { outQueue =>
        lazy val loop: UIO[Unit] =
          inQueue.take.flatMap(in => outQueue.offer(f(in))) *> loop

        loop.forkScoped
      }

    def mapOption[B](f: A => Option[B]): URIO[Scope, Dequeue[B]] =
      Queue.unbounded[B].tap { outQueue =>
        lazy val loop: UIO[Unit] =
          inQueue.take.flatMap(in => ZIO.foreachDiscard(f(in))(outQueue.offer)) *> loop

        loop.forkScoped
      }

    def mapIterable[B](f: A => Iterable[B]): URIO[Scope, Dequeue[B]] =
      Queue.unbounded[B].tap { outQueue =>
        lazy val loop: UIO[Unit] =
          inQueue.take.flatMap(in => outQueue.offerAll(f(in))) *> loop

        loop.forkScoped
      }

    // TODO (KR) : zio versions of this?

  }

}
