package oxygen.zio.syntax

import oxygen.core.typeclass.SeqOps
import zio.*

object seq {

  extension [F[_], A](self: F[A])(using seqOps: SeqOps[F]) {

    def mapZIO[R, E, B](f: A => ZIO[R, E, B]): ZIO[R, E, F[B]] =
      if seqOps.knownSize(self) == 0 then ZIO.succeed(seqOps.newBuilder[B].result())
      else
        ZIO.suspendSucceed {
          val iterator = seqOps.newIterator(self)
          val builder = seqOps.newBuilder[B]
          builder.sizeHint(seqOps.knownSize(self))

          ZIO.whileLoop(iterator.hasNext)(f(iterator.next()))(builder.addOne).as(builder.result())
        }

    def flatMapZIO[R, E, B](f: A => ZIO[R, E, F[B]]): ZIO[R, E, F[B]] =
      self.mapZIO(f).map(seqOps.flattenAttemptKnownSize)

    def foreachZIO[R, E](f: A => ZIO[R, E, Any]): ZIO[R, E, Unit] =
      ZIO.suspendSucceed {
        if seqOps.knownSize(self) == 0 then Exit.unit
        else {
          val iterator = seqOps.newIterator(self)
          ZIO.whileLoop(iterator.hasNext)(f(iterator.next()))(_ => ())
        }
      }

  }

}
