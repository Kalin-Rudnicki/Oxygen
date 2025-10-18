package oxygen.zio

import zio.*

abstract class GlobalLayer[E, A] {

  protected val underlyingLayer: Layer[E, A]

  private lazy val effect: IO[E, ZEnvironment[A]] =
    GlobalMemoizedEffect.cacheVal { underlyingLayer.build(Scope.global) }

  final val layer: Layer[E, A] =
    ZLayer.fromZIOEnvironment { effect }

}
object GlobalLayer {

  def make[E, A](underlyingLayer: Layer[E, A]): GlobalLayer[E, A] = {
    val tmp = underlyingLayer
    new GlobalLayer[E, A] { override protected val underlyingLayer: Layer[E, A] = tmp }
  }

  /**
    * Note, this should only be called when assigning the result to a global value.
    * If not, the global cache ref will not be the same, and this does nothing.
    *
    * --- good ---
    * val res: ULayer[Int] = GlobalLayer.cacheVal { ZLayer { Random.nextInt } }
    *
    * --- bad ---
    * def res: ULayer[Int] = GlobalLayer.cacheVal { ZLayer { Random.nextInt } }
    *
    * --- bad ---
    * val res: ULayer[Int] =
    *   for {
    *     _ <- ZLayer { ZIO.logInfo("logging") }
    *     res <- GlobalLayer.cacheVal { ZLayer { Random.nextInt } }
    *   } yield res
    */
  def cacheVal[E, A](underlyingLayer: Layer[E, A]): Layer[E, A] = {
    val layer: GlobalLayer[E, A] = GlobalLayer.make(underlyingLayer)
    layer.layer
  }

}
