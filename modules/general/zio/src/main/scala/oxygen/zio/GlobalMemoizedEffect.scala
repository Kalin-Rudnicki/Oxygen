package oxygen.zio

import zio.*

abstract class GlobalMemoizedEffect[E, A] {

  protected val underlyingEffect: IO[E, A]

  private val cache: Ref.Synchronized[Option[Exit[E, A]]] =
    Unsafe.unsafely { Ref.Synchronized.unsafe.make(None) }

  private val getOrRun: IO[E, A] =
    cache.modifyZIO {
      case current @ Some(exit) => ZIO.succeed((exit, current))
      case None                 => underlyingEffect.exit.map { exit => (exit, Some(exit)) }
    }.flatten

  final val effect: IO[E, A] =
    getOrRun

}
object GlobalMemoizedEffect {

  def make[E, A](underlyingEffect: IO[E, A]): GlobalMemoizedEffect[E, A] = {
    val tmp = underlyingEffect
    new GlobalMemoizedEffect[E, A] { override protected val underlyingEffect: IO[E, A] = tmp }
  }

  /**
    * Note, this should only be called when assigning the result to a global value.
    * If not, the global cache ref will not be the same, and this does nothing.
    *
    * --- good ---
    * val res: UIO[Int] = GlobalMemoizedEffect.cacheVal { Random.nextInt }
    *
    * --- bad ---
    * def res: UIO[Int] = GlobalMemoizedEffect.cacheVal { Random.nextInt }
    *
    * --- bad ---
    * val res: UIO[Int] =
    *   for {
    *     _ <- ZIO.logInfo("logging")
    *     res <- GlobalMemoizedEffect.cacheVal { Random.nextInt }
    *   } yield res
    */
  def cacheVal[E, A](underlyingEffect: IO[E, A]): IO[E, A] = {
    val effect: GlobalMemoizedEffect[E, A] = GlobalMemoizedEffect.make(underlyingEffect)
    effect.effect
  }

}
