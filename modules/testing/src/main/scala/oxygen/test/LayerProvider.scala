package oxygen.test

import zio.*
import zio.test.*
import oxygen.test.OxygenSpec.DefaultEnv


sealed trait LayerProvider[+R] {
  // TODO (KR) : def build[E] (?)
  private[test] def build(spec: Spec[R & DefaultEnv, Any]): Spec[DefaultEnv, Any]
}
object LayerProvider {

  inline def providePerTest[PerTestR: EnvironmentTag](inline layer: ZLayer[?, Any, ?]*): LayerProvider.PerTest[PerTestR] =
    new LayerProvider.PerTest[PerTestR](
      ZLayer.makeSome[DefaultEnv, PerTestR](layer*),
    )

  inline def provideShared[SharedR: EnvironmentTag](inline layer: ZLayer[?, Any, ?]*): LayerProvider.Shared[SharedR] =
    new LayerProvider.Shared[SharedR](
      ZLayer.makeSome[DefaultEnv, SharedR](layer*),
    )

  case object Empty extends LayerProvider[Any] {

    override private[test] def build(spec: Spec[DefaultEnv, Any]): Spec[DefaultEnv, Any] = spec

  }

  final class Shared[SharedR: EnvironmentTag](
      sharedLayer: ZLayer[DefaultEnv, Any, SharedR],
  ) extends LayerProvider[SharedR] {

    override private[test] def build(spec: Spec[SharedR & DefaultEnv, Any]): Spec[DefaultEnv, Any] =
      spec.provideSomeLayerShared(sharedLayer)

    inline def providePerTest[PerTestR: EnvironmentTag](inline layer: ZLayer[?, Any, ?]*): LayerProvider.SharedAndPerTest[SharedR, PerTestR] =
      new LayerProvider.SharedAndPerTest[SharedR, PerTestR](
        sharedLayer,
        ZLayer.makeSome[DefaultEnv, PerTestR](layer*),
      )

  }

  final class PerTest[PerTestR: EnvironmentTag](
      perTestLayer: ZLayer[DefaultEnv, Any, PerTestR],
  ) extends LayerProvider[PerTestR] {

    override private[test] def build(spec: Spec[PerTestR & DefaultEnv, Any]): Spec[DefaultEnv, Any] =
      spec.provideSomeLayer(perTestLayer)

  }

  final class SharedAndPerTest[SharedR: EnvironmentTag, PerTestR: EnvironmentTag](
      sharedLayer: ZLayer[DefaultEnv, Any, SharedR],
      perTestLayer: ZLayer[DefaultEnv & SharedR, Any, PerTestR],
  ) extends LayerProvider[SharedR & PerTestR] {

    override private[test] def build(spec: Spec[SharedR & PerTestR & DefaultEnv, Any]): Spec[DefaultEnv, Any] =
      spec.provideSomeLayer(perTestLayer).provideSomeLayerShared(sharedLayer)

  }

}
