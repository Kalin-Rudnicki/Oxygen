package oxygen.zio

import oxygen.predef.core.*
import zio.*

abstract class GlobalLayer[E, A] {

  private val cache: Ref.Synchronized[Option[ZEnvironment[A]]] =
    Unsafe.unsafely { Ref.Synchronized.unsafe.make(None) }

  protected val underlyingLayer: Layer[E, A]

  private lazy val makeEnvironment: IO[E, ZEnvironment[A]] =
    underlyingLayer.build(Scope.global)

  private lazy val getOrMakeEnvironment: IO[E, ZEnvironment[A]] =
    cache.modifyZIO {
      case current @ Some(env) => ZIO.succeed((env, current))
      case None                => makeEnvironment.map { env => (env, env.some) }
    }

  final val layer: Layer[E, A] =
    ZLayer.fromZIOEnvironment { getOrMakeEnvironment }

}
