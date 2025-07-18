package oxygen.test

import oxygen.zio.logging.{LogConfig, LogLevels}
import zio.*
import zio.test.*

abstract class OxygenSpec[_R] extends ZIOSpecDefault {

  final type DefaultEnv = OxygenSpec.DefaultEnv
  final type RootTestAspect = OxygenSpec.RootTestAspect

  final type R = _R
  final type Env = DefaultEnv & R

  final type TestSpecAspect = TestAspectAtLeastR[Env]
  final type TestSpec = Spec[Env, Any]

  // =====| Abstract |=====

  def testSpec: TestSpec

  def layerProvider: LayerProvider[R]

  // =====| Overridable |=====

  def defaultLogLevel: LogLevel = LogLevels.Important

  def withDefaultAspects: Boolean = true

  // NOTE : aspect priority: defaultTestAspects > OxygenSpec.defaultTestAspects > testAspects

  def testAspects: Chunk[TestSpecAspect] = Chunk.empty
  def rootTestAspects: Chunk[RootTestAspect] = Chunk.empty

  // TODO (KR) : allow aspects with `Chunk[TestAspectAtLeastR[DefaultEnv]]` that can be applied after layer application.

  // =====| Concrete |=====

  override final def aspects: Chunk[TestAspectAtLeastR[TestEnvironment]] = super.aspects

  override final def spec: Spec[TestEnvironment & Scope, Any] = {
    val testAspects0: Chunk[TestSpecAspect] =
      testAspects
    val rootTestAspects0: Chunk[RootTestAspect] =
      if (withDefaultAspects) OxygenSpec.defaultTestAspects(defaultLogLevel) ++ rootTestAspects
      else rootTestAspects

    val withTestAspectsApplied: TestSpec =
      testAspects0.foldLeft(testSpec) { _ @@ _ }
    val withLayersApplied: Spec[DefaultEnv, Any] =
      layerProvider.build(withTestAspectsApplied)
    val withRootAspectsApplied: Spec[DefaultEnv, Any] =
      rootTestAspects0.foldLeft(withLayersApplied) { _ @@ _ }

    withRootAspectsApplied
  }

}
object OxygenSpec {

  type DefaultEnv = TestEnvironment & Scope
  type RootTestAspect = TestAspectAtLeastR[DefaultEnv]

  private def defaultTestAspects(defaultLogLevel: LogLevel): Chunk[TestAspectAtLeastR[DefaultEnv]] =
    Chunk(
      TestAspect.samples(15),
      TestAspect.shrinks(0),
      OxygenAspects.usingConfig(LogConfig.oxygenDefault(defaultLogLevel)),
      OxygenAspects.withTestAsLogSpan,
    )

}
