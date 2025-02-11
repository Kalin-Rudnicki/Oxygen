package oxygen.meta.example

// @oxygen.meta.annotation.showDerivation
final case class CaseClass1(
    @fieldName("my-int") int: Int,
    string: String,
    boolean: Boolean,
)

final case class CaseClass2(
)

final case class CaseClass3(
    int: Option[Int],
    booleans: Option[Seq[Boolean]],
)

final case class CaseClass4(
    value: Int,
    nested: Option[CaseClass4],
)

final case class CaseClass5(
    cc1: CaseClass1,
    cc2: CaseClass2,
)

case object CaseObject1

sealed trait SealedTrait1
object SealedTrait1 {
  final case class Case1(int: Int, string: String) extends SealedTrait1
  final case class Case2() extends SealedTrait1
  case object Case3 extends SealedTrait1
}

sealed abstract class SealedAbstractClass1
object SealedAbstractClass1 {
  final case class Case1(int: Int, string: String) extends SealedAbstractClass1
  final case class Case2() extends SealedAbstractClass1
  case object Case3 extends SealedAbstractClass1
}

// @oxygen.meta.annotation.showDerivation
enum Enum1 {
  case Case1(int: Int, string: String)
  case Case2()
  case Case3
}
