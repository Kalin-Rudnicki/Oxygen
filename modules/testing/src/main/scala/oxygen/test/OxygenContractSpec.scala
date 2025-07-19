package oxygen.test

import oxygen.core.syntax.option.*
import zio.*
import zio.test.*

abstract class OxygenContractSpec[_R](implName: String, contract: Contract[_R]) extends OxygenSpec[_R] {

  private def modifyCase(spec: TestSpec): Option[TestSpec] =
    spec.caseValue match {
      case Spec.ExecCase(exec, spec)     => modifyCase(spec).map(s => Spec(Spec.ExecCase(exec, s)))
      case Spec.LabeledCase(label, spec) => Spec(Spec.LabeledCase(s"$label ($implName)", spec)).some
      case _                             => None
    }

  override final def testSpec: TestSpec = {
    val built = contract.testSpec
    modifyCase(built).getOrElse { suite(implName)(built) }
  }

}
