package oxygen.meta

// FIX-PRE-MERGE (KR) :
object Outer {

  opaque type InvisibleOpaqueType1 = Int
  opaque type InvisibleOpaqueType2 <: java.time.temporal.Temporal = java.time.LocalTime
  opaque type InvisibleOpaqueType3[A] = Option[A]

}

// FIX-PRE-MERGE (KR) : make an actual spec?
object ScalaTypeSpec {

  sealed trait SealedTrait1
  final case class CaseClass1() extends SealedTrait1
  case object CaseObject1 extends SealedTrait1

  sealed trait SealedTrait2[A]
  final case class CaseClass2[A]() extends SealedTrait2[A]
  final case class CaseClass3[A]() extends SealedTrait2[Option[A]]
  case object CaseObject2 extends SealedTrait2[Unit]

  final class SimpleClass
  final class WeirdClass1[A, B](val a: A)(val b: B)(c: A, d: B) {
    println((a, b, c, d))
  }

  type Alias1[A] = List
  type Alias2[A] = List[Option[A]]
  type Alias3[A] = [B] =>> Either[A, B]

  opaque type VisibleOpaqueType1 = Int
  opaque type VisibleOpaqueType2 <: java.time.temporal.Temporal = java.time.LocalTime
  opaque type VisibleOpaqueType3[A] = Option[A]

  //

  // Macros.doTheTypeStuff[Outer.InvisibleOpaqueType1]

}
