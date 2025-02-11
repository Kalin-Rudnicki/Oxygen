package oxygen.meta.example

final class Class1

object Object1

sealed trait SealedTrait2
object SealedTrait2 {
  final case class Case1(int: Int, string: String) extends SealedTrait2
  final case class Case2() extends SealedTrait2
  object Case3 extends SealedTrait2
}
