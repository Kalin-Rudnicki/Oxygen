package oxygen.quoted

import oxygen.predef.test.*
import scala.reflect.ClassTag

object QuotedSpec extends OxygenSpecDefault {

  final case class Class1()
  final case class Class2(a: Int)
  final class Class3(val a: Int, val b: String, val c: Double)(val d: Boolean)

  private inline def primaryConstructorSpec[A](exp: String*)(using ct: ClassTag[A]): TestSpec =
    test(ct.runtimeClass.getSimpleName) {
      assertTrue(Macros.getConstructorParams[A] == exp.toSet)
    }

  override def testSpec: TestSpec =
    suite("QuotedSpec")(
      primaryConstructorSpec[Class1](),
      primaryConstructorSpec[Class2]("a"),
      primaryConstructorSpec[Class3]("a", "b", "c", "d"),
    )

}
