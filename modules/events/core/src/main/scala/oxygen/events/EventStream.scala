package oxygen.events

import zio.*
import zio.stream.*

final class EventStream[-R, +E, +A](sourceName: String, supportsParallel: Boolean, raw: ZStream[R, E, Committable[A]]) {

  /////// run ///////////////////////////////////////////////////////////////

  def foreach[R2 <: R, E2 >: E](f: A => ZIO[R2, E2, Unit]): ZIO[R2, E2, Unit] =
    raw.foreach { event =>
      f(event.value) *> event.commit
    }

  // TODO (KR) : support parallelization
  // def foreachPar[R2 <: R, E2 >: E, K](n: Int, k: A => K)(f: A => ZIO[R2, E2, Unit]): ZIO[R2, E2, Unit] =
  //   ZIO.dieMessage(s"Parallel stream not supported [source=$sourceName]").unlessDiscard(supportsParallel) *>
  //     KeyedBuffer.run(raw, k, n, f)

  /////// effectless ///////////////////////////////////////////////////////////////

  def map[B](f: A => B): EventStream[R, E, B] =
    EventStream(sourceName, supportsParallel, raw.map { a => a.as(f(a.value)) })

  def mapOption[B](f: A => Option[B]): EventStream[R, E, B] =
    mapZIOOption { a => ZIO.succeed(f(a)) }

  def collect[B](f: PartialFunction[A, B]): EventStream[R, E, B] = {
    val f2 = f.lift
    mapZIOOption { a => ZIO.succeed(f2(a)) }
  }

  def collectOption[B](f: PartialFunction[A, Option[B]]): EventStream[R, E, B] = {
    val f2 = f.lift
    mapZIOOption { a => ZIO.succeed(f2(a).flatten) }
  }

  def filter(f: A => Boolean): EventStream[R, E, A] =
    EventStream(
      sourceName,
      supportsParallel,
      raw.mapChunksZIO { as =>
        ZIO.foreach(as) { a =>
          if f(a.value) then ZIO.some(a)
          else a.commit.as(None)
        }.map(_.flatten)
      },
    )

  def withFilter(f: A => Boolean): EventStream[R, E, A] = filter(f)

  def filterNot(f: A => Boolean): EventStream[R, E, A] = filter(!f(_))

  /////// effectful ///////////////////////////////////////////////////////////////

  def mapZIO[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, B]): EventStream[R2, E2, B] =
    EventStream(sourceName, supportsParallel, raw.mapZIOChunked { a => f(a.value).map { b => a.as(b) } })

  def collectZIO[R2 <: R, E2 >: E, B](f: PartialFunction[A, ZIO[R2, E2, B]]): EventStream[R2, E2, B] = {
    val f2 = f.lift
    mapZIOOption { a =>
      f2(a) match {
        case Some(value) => value.asSome
        case None        => ZIO.none
      }
    }
  }

  def collectZIOOption[R2 <: R, E2 >: E, B](f: PartialFunction[A, ZIO[R2, E2, Option[B]]]): EventStream[R2, E2, B] = {
    val f2 = f.lift
    mapZIOOption { a =>
      f2(a) match {
        case Some(value) => value
        case None        => ZIO.none
      }
    }
  }

  def mapZIOOption[R2 <: R, E2 >: E, B](f: A => ZIO[R2, E2, Option[B]]): EventStream[R2, E2, B] =
    EventStream(
      sourceName,
      supportsParallel,
      raw.mapChunksZIO { as =>
        ZIO.foreach(as) { a =>
          f(a.value).flatMap {
            case Some(b) => ZIO.some(a.as(b))
            case None    => a.commit.as(None)
          }
        }.map(_.flatten)
      },
    )

  def mapZIOOptionError[R2 <: R, E2 >: E, B](f: A => ZIO[R2, Option[E2], B]): EventStream[R2, E2, B] =
    mapZIOOption(f(_).unsome)

}
