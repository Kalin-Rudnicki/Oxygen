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

final case class CaseClass6[A](
    field: A,
)

case object CaseObject1

sealed trait SealedTrait1
object SealedTrait1 {
  final case class Case1(int: Int, string: String) extends SealedTrait1
  final case class Case2() extends SealedTrait1
  case object Case3 extends SealedTrait1
}

sealed trait SealedTrait3[+A, +B] derives Show
object SealedTrait3 {
  final case class AB1[+B, +A](a: B, b: A) extends SealedTrait3[B, A]
  final case class AB2[+C, +D](a: C, b: D) extends SealedTrait3[D, C]
  final case class A[+A](a: A) extends SealedTrait3[A, Nothing]
  final case class B[+B](b: B) extends SealedTrait3[Nothing, B]
  case object Neither extends SealedTrait3[Nothing, Nothing]
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

// @oxygen.meta.annotation.showDerivation
enum Enum2[+A, +B] derives Show {
  case AB1(a: A, b: B)
  case AB2(a: B, b: A)
  case A(a: A)
  case B(b: B)
  case Neither
}
