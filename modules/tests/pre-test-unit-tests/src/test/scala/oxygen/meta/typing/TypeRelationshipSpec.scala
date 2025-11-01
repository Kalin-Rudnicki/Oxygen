package oxygen.meta.typing

object TypeRelationshipSpec {

  sealed trait ABC
  sealed trait AB extends ABC
  final class A extends AB
  final class B extends AB
  final class C extends ABC

  /////// ABC ///////////////////////////////////////////////////////////////

  TypeRelationship.Equals[ABC, ABC]
  TypeRelationship.StrictSuperType[ABC, AB]
  TypeRelationship.StrictSuperType[ABC, A]
  TypeRelationship.StrictSuperType[ABC, B]
  TypeRelationship.StrictSuperType[ABC, C]

  /////// AB ///////////////////////////////////////////////////////////////

  TypeRelationship.StrictSubType[AB, ABC]
  TypeRelationship.Equals[AB, AB]
  TypeRelationship.StrictSuperType[AB, A]
  TypeRelationship.StrictSuperType[AB, B]
  TypeRelationship.Disjoint[AB, C]

  /////// A ///////////////////////////////////////////////////////////////

  TypeRelationship.StrictSubType[A, ABC]
  TypeRelationship.StrictSubType[A, AB]
  TypeRelationship.Equals[A, A]
  TypeRelationship.Disjoint[A, B]
  TypeRelationship.Disjoint[A, C]

  /////// B ///////////////////////////////////////////////////////////////

  TypeRelationship.StrictSubType[B, ABC]
  TypeRelationship.StrictSubType[B, AB]
  TypeRelationship.Disjoint[B, A]
  TypeRelationship.Equals[B, B]
  TypeRelationship.Disjoint[B, C]

  /////// C ///////////////////////////////////////////////////////////////

  TypeRelationship.StrictSubType[C, ABC]
  TypeRelationship.Disjoint[C, AB]
  TypeRelationship.Disjoint[C, A]
  TypeRelationship.Disjoint[C, B]
  TypeRelationship.Equals[C, C]

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Misc
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def fail[E](error: => E)(using @scala.annotation.unused ev: E =!= Nothing): E = error

  summon[A =!= AB]
  summon[A !<<:< A]
  summon[A !>>:> A]
  summon[A >!< A]

  fail(5)

}
