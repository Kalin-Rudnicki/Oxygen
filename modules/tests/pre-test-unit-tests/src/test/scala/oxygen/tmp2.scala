package oxygen

object tmp2 {

  sealed trait ParentImpl { _self: tmp.Parent =>
    def self: tmp.Parent & ParentImpl = _self
  }
  trait Case1Impl extends ParentImpl, tmp.Case1
  trait Case2Impl extends ParentImpl, tmp.Case2

  val c1a: Case1Impl = ???
  val c1b: tmp.Case1 = c1a

  val c2a: Case2Impl = ???
  val c2b: tmp.Case2 = c2a

  val pa: ParentImpl = ???
  val pb: tmp.Parent = pa.self
  // val pc: tmp.Parent = pa

}

import oxygen.meta.k0.*
import scala.quoted.*

trait MyMonoid[A] {
  def zero: A
  def join(a: A, b: A): A
}
object MyMonoid {

  given int: MyMonoid[Int] =
    new MyMonoid[Int] {
      override def zero: Int = 0
      override def join(a: Int, b: Int): Int = a + b
    }

  given string: MyMonoid[String] =
    new MyMonoid[String] {
      override def zero: String = ""
      override def join(a: String, b: String): String = a + b
    }

  private def zeroImpl[A: Type](gen: ProductGeneric[A], instances: Expressions[MyMonoid, A])(using Quotes): Expr[A] =
    gen.instantiate.id { [b] => (_, _) ?=> (field: gen.Field[b]) =>
      val fieldInstance: Expr[MyMonoid[b]] = field.getExpr(instances)
      '{ $fieldInstance.zero }
    }

  private def joinImpl[A: Type](gen: ProductGeneric[A], instances: Expressions[MyMonoid, A])(aExpr: Expr[A], bExpr: Expr[A])(using Quotes): Expr[A] =
    gen.instantiate.id { [b] => (_, _) ?=> (field: gen.Field[b]) =>
      val fieldInstance: Expr[MyMonoid[b]] = field.getExpr(instances)
      val fieldAExpr: Expr[b] = field.fromParent(aExpr)
      val fieldBExpr: Expr[b] = field.fromParent(bExpr)
      '{ $fieldInstance.join($fieldAExpr, $fieldBExpr) }
    }

  private def derivedImpl[A: Type](using Quotes): Expr[MyMonoid[A]] = {
    val gen: ProductGeneric[A] = ProductGeneric.of[A]
    gen.cacheVals.summonTypeClasses[MyMonoid]().defineAndUse { instances =>
      '{
        new MyMonoid[A] {
          override def zero: A = ${ zeroImpl[A](gen, instances) }
          override def join(a: A, b: A): A = ${ joinImpl[A](gen, instances)('a, 'b) }
        }
      }
    }
  }

  inline def derived[A]: MyMonoid[A] = ${ derivedImpl[A] }

}

final case class MyCaseClass(a: Int, b: String)
object MyCaseClass {

  given MyMonoid[MyCaseClass] = {
    lazy val instance_a: MyMonoid[Int] = MyMonoid.int
    lazy val instance_b: MyMonoid[String] = MyMonoid.string
    new MyMonoid[MyCaseClass] {
      override def zero: MyCaseClass =
        MyCaseClass(
          instance_a.zero,
          instance_b.zero,
        )
      override def join(a: MyCaseClass, b: MyCaseClass): MyCaseClass =
        MyCaseClass(
          instance_a.join(a.a, b.a),
          instance_b.join(a.b, b.b),
        )
    }
  }

}

final case class CC1(int: Int, string: String, boolean: Option[Boolean]) derives FromExprT

sealed trait myAnnot extends scala.annotation.Annotation derives FromExprT
final case class myAnnot1() extends myAnnot
final case class myAnnot2(a: String) extends myAnnot
final case class myAnnot3(b: Int) extends myAnnot
final case class myAnnot4(c: List[Boolean], cc1: CC1) extends myAnnot
