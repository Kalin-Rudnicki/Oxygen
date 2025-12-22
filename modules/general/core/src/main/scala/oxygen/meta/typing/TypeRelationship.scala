package oxygen.meta.typing

import oxygen.core.syntax.common.*
import oxygen.meta.typing.TypeRelationship.*
import oxygen.quoted.{SuperType as _, *}
import scala.annotation.publicInBinary
import scala.quoted.*

// TODO (KR) : have a way to represent that these 2 types are effectively the same:
//           : enum MyEnum { case A, B, C }
//           : MyEnum =~= MyEnum.A.type | MyEnum.B.type | MyEnum.C.type
sealed trait TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

  final def self: (SubType[A, B] | NotSubType[A, B]) & (SuperType[A, B] | NotSuperType[A, B]) = rawSelf
  final def subSelf: SubType[A, B] | NotSubType[A, B] = rawSelf
  final def superSelf: SuperType[A, B] | NotSuperType[A, B] = rawSelf

  def flip: TypeRelationship[B, A]

}
object TypeRelationship { // TODO (KR) : figure out the best way for these things to be able to prove each other without constantly re-deriving

  type Constraints[A, B] =
    (SubType[A, B] | NotSubType[A, B]) &
      (SuperType[A, B] | NotSuperType[A, B]) &
      (StrictSubType[A, B] | NotStrictSubType[A, B]) &
      (StrictSuperType[A, B] | NotStrictSuperType[A, B]) &
      (Equals[A, B] | NotEquals[A, B]) &
      (Disjoint[A, B] | NotDisjoint[A, B])

  def apply[A, B](using ev: TypeRelationship[A, B]): TypeRelationship[A, B] = ev

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      1
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  sealed trait SubTypeRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isSubType: Boolean

  }

  sealed trait SuperTypeRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isSuperType: Boolean

  }

  sealed trait StrictSubTypeRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isStrictSubType: Boolean

  }

  sealed trait StrictSuperTypeRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isStrictSuperType: Boolean

  }

  sealed trait EqualsRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isEquals: Boolean

  }

  sealed trait DisjointRelationship[A, B] extends TypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    val isDisjoint: Boolean

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      2
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  /////// SubType ///////////////////////////////////////////////////////////////

  sealed trait SubType[A, B] extends SubTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isSubType: Boolean = true

    override def flip: SuperType[B, A]

    val aToB: A => B

    final def unapply(a: A): Some[B] = Some(aToB(a))

  }
  object SubType {

    def apply[A, B](using ev: SubType[A, B]): SubType[A, B] = ev

    inline given derive: [A, B] => SubType[A, B] = TypeRelationship.deriveInternal[A, B, SubType[A, B]]

  }

  sealed trait NotSubType[A, B] extends SubTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isSubType: Boolean = false

    override def flip: NotSuperType[B, A]

  }
  object NotSubType {

    def apply[A, B](using ev: NotSubType[A, B]): NotSubType[A, B] = ev

    inline given derive: [A, B] => NotSubType[A, B] = TypeRelationship.deriveInternal[A, B, NotSubType[A, B]]

  }

  /////// SuperType ///////////////////////////////////////////////////////////////

  sealed trait SuperType[A, B] extends SuperTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isSuperType: Boolean = true

    val bToA: B => A

    override def flip: SubType[B, A]

  }
  object SuperType {

    def apply[A, B](using ev: SuperType[A, B]): SuperType[A, B] = ev

    inline given derive: [A, B] => SuperType[A, B] = TypeRelationship.deriveInternal[A, B, SuperType[A, B]]

  }

  sealed trait NotSuperType[A, B] extends SuperTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isSuperType: Boolean = false

    override def flip: NotSubType[B, A]

  }
  object NotSuperType {

    def apply[A, B](using ev: NotSuperType[A, B]): NotSuperType[A, B] = ev

    inline given derive: [A, B] => NotSuperType[A, B] = TypeRelationship.deriveInternal[A, B, NotSuperType[A, B]]

  }

  /////// StrictSubType ///////////////////////////////////////////////////////////////

  sealed trait NotStrictSubType[A, B] extends StrictSubTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isStrictSubType: Boolean = false

    override def flip: NotStrictSuperType[B, A]

  }
  object NotStrictSubType {

    def apply[A, B](using ev: NotStrictSubType[A, B]): NotStrictSubType[A, B] = ev

    inline given derive: [A, B] => NotStrictSubType[A, B] = TypeRelationship.deriveInternal[A, B, NotStrictSubType[A, B]]

  }

  /////// StrictSuperType ///////////////////////////////////////////////////////////////

  sealed trait NotStrictSuperType[A, B] extends StrictSuperTypeRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isStrictSuperType: Boolean = false

    override def flip: NotStrictSubType[B, A]

  }
  object NotStrictSuperType {

    def apply[A, B](using ev: NotStrictSuperType[A, B]): NotStrictSuperType[A, B] = ev

    inline given derive: [A, B] => NotStrictSuperType[A, B] = TypeRelationship.deriveInternal[A, B, NotStrictSuperType[A, B]]

  }

  /////// Equals ///////////////////////////////////////////////////////////////

  sealed trait NotEquals[A, B] extends EqualsRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isEquals: Boolean = false

    override def flip: NotEquals[B, A]

  }
  object NotEquals {

    def apply[A, B](using ev: NotEquals[A, B]): NotEquals[A, B] = ev

    inline given derive: [A, B] => NotEquals[A, B] = TypeRelationship.deriveInternal[A, B, NotEquals[A, B]]

  }

  /////// Disjoint ///////////////////////////////////////////////////////////////

  sealed trait NotDisjoint[A, B] extends DisjointRelationship[A, B] { rawSelf: Constraints[A, B] =>

    override final val isDisjoint: Boolean = false

    override def flip: NotDisjoint[B, A]

  }
  object NotDisjoint {

    def apply[A, B](using ev: NotDisjoint[A, B]): NotDisjoint[A, B] = ev

    inline given derive: [A, B] => NotDisjoint[A, B] = TypeRelationship.deriveInternal[A, B, NotDisjoint[A, B]]

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      3
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final class StrictSubType[A, B] @publicInBinary private[TypeRelationship] (
      val aToB: A => B,
  ) extends SubType[A, B],
        NotSuperType[A, B],
        StrictSubTypeRelationship[A, B],
        NotStrictSuperType[A, B],
        NotEquals[A, B],
        NotDisjoint[A, B] {

    override val isStrictSubType: Boolean = true

    override def flip: StrictSuperType[B, A] = new StrictSuperType[B, A](aToB)

  }
  object StrictSubType {

    def apply[A, B](using ev: StrictSubType[A, B]): StrictSubType[A, B] = ev

    inline given derive: [A, B] => StrictSubType[A, B] = TypeRelationship.deriveInternal[A, B, StrictSubType[A, B]]

  }

  final class StrictSuperType[A, B] @publicInBinary private[TypeRelationship] (
      val bToA: B => A,
  ) extends NotSubType[A, B],
        SuperType[A, B],
        NotStrictSubType[A, B],
        StrictSuperTypeRelationship[A, B],
        NotEquals[A, B],
        NotDisjoint[A, B] {

    override val isStrictSuperType: Boolean = true

    override def flip: StrictSubType[B, A] = new StrictSubType[B, A](bToA)

  }
  object StrictSuperType {

    def apply[A, B](using ev: StrictSuperType[A, B]): StrictSuperType[A, B] = ev

    inline given derive: [A, B] => StrictSuperType[A, B] = TypeRelationship.deriveInternal[A, B, StrictSuperType[A, B]]

  }

  final class Equals[A, B] @publicInBinary private[TypeRelationship] (
      val aToB: A => B,
      val bToA: B => A,
  ) extends SubType[A, B],
        SuperType[A, B],
        NotStrictSubType[A, B],
        NotStrictSuperType[A, B],
        EqualsRelationship[A, B],
        NotDisjoint[A, B] {

    override val isEquals: Boolean = true

    override def flip: Equals[B, A] = new Equals[B, A](bToA, aToB)

  }
  object Equals {

    def apply[A, B](using ev: Equals[A, B]): Equals[A, B] = ev

    inline given derive: [A, B] => Equals[A, B] = TypeRelationship.deriveInternal[A, B, Equals[A, B]]

  }

  final class Disjoint[A, B] @publicInBinary private[TypeRelationship] (
  ) extends NotSubType[A, B],
        NotSuperType[A, B],
        NotStrictSubType[A, B],
        NotStrictSuperType[A, B],
        NotEquals[A, B],
        DisjointRelationship[A, B] {

    override val isDisjoint: Boolean = true

    override def flip: Disjoint[B, A] = new Disjoint[B, A]

  }
  object Disjoint {

    def apply[A, B](using ev: Disjoint[A, B]): Disjoint[A, B] = ev

    inline given derive: [A, B] => Disjoint[A, B] = TypeRelationship.deriveInternal[A, B, Disjoint[A, B]]

  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      4
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  private def deriveInternalImpl[A: scala.quoted.Type, B: scala.quoted.Type, T <: TypeRelationship[A, B]: scala.quoted.Type](using Quotes): Expr[T] = {

    def castFunction[C: scala.quoted.Type, D: scala.quoted.Type]: Expr[C => D] =
      '{ (c: C) => ${ ('c).asExprOf[D] } }

    def subTypeEv[C: scala.quoted.Type, D: scala.quoted.Type]: Option[Expr[C => D]] =
      if TypeRepr.of[C] <:< TypeRepr.of[D] then castFunction[C, D].some
      else None

    val raw: Expr[TypeRelationship[A, B]] =
      (subTypeEv[A, B], subTypeEv[B, A]) match {
        case (Some(ab), Some(ba)) => '{ new Equals[A, B]($ab, $ba) }
        case (Some(ab), None)     => '{ new StrictSubType[A, B]($ab) }
        case (None, Some(ba))     => '{ new StrictSuperType[A, B]($ba) }
        case (None, None)         => '{ new Disjoint[A, B]() }
      }

    val typed: Expr[T] =
      if raw.isExprOf[T] then raw.asExprOf[T]
      else
        report.errorAndAbort(
          s"""Attempted to assert: ${TypeRepr.of[T].showAnsiCode}
             |          But found: ${raw.toTerm.tpe.showAnsiCode}""".stripMargin,
        )

    /*
    report.info(
      s"""${TypeRepr.of[T].showAnsiCode}
         |${tpe.showAnsiCode}
         |${typed.showAnsiCode}""".stripMargin,
    )
     */

    typed
  }

  private[typing] inline def deriveInternal[A, B, T <: TypeRelationship[A, B]] = ${ deriveInternalImpl[A, B, T] }

}
