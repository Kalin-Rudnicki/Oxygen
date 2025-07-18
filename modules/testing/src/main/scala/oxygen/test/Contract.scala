package oxygen.test

import zio.test.*

abstract class Contract[_R] {

  final type R = _R
  final type DefaultEnv = OxygenSpec.DefaultEnv
  final type Env = DefaultEnv & R

  final type TestSpec = Spec[Env, Any]

  // =====| Abstract |=====

  def testSpec: TestSpec

}
