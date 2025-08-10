package oxygen.core

sealed trait InfiniteSet[A] extends Product {

  val explicit: Set[A]

  final def invert: InfiniteSet[A] = this match
    case InfiniteSet.Inclusive(explicit) => InfiniteSet.Exclusive(explicit)
    case InfiniteSet.Exclusive(explicit) => InfiniteSet.Inclusive(explicit)

  /**
    * Everything that is in `either` this `or` that
    */
  final def union(that: InfiniteSet[A]): InfiniteSet[A] = (this, that) match
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Inclusive(explicit1 | explicit2)
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Exclusive(explicit2 &~ explicit1)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Exclusive(explicit1 &~ explicit2)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Exclusive(explicit1 & explicit2)

  /**
    * Everything that is in `both` this `and` that
    */
  final def intersection(that: InfiniteSet[A]): InfiniteSet[A] = (this, that) match
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Inclusive(explicit1 & explicit2)
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Inclusive(explicit1 &~ explicit2)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Inclusive(explicit2 &~ explicit1)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Exclusive(explicit1 | explicit2)

  /**
    * Everything that is in `this`, `but not` in that
    */
  final def disjunction(that: InfiniteSet[A]): InfiniteSet[A] = (this, that) match
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Inclusive(explicit1 &~ explicit2)
    case (InfiniteSet.Inclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Inclusive(explicit1 & explicit2)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Inclusive(explicit2)) => InfiniteSet.Exclusive(explicit1 | explicit2)
    case (InfiniteSet.Exclusive(explicit1), InfiniteSet.Exclusive(explicit2)) => InfiniteSet.Inclusive(explicit2 &~ explicit1)

  final def contains(value: A): Boolean = this match
    case InfiniteSet.Inclusive(explicit) => explicit.contains(value)
    case InfiniteSet.Exclusive(explicit) => !explicit.contains(value)

  // aliases
  inline final def ~ : InfiniteSet[A] = invert
  inline final def |(that: InfiniteSet[A]): InfiniteSet[A] = union(that)
  inline final def &(that: InfiniteSet[A]): InfiniteSet[A] = intersection(that)
  inline final def &~(that: InfiniteSet[A]): InfiniteSet[A] = disjunction(that)

  override final def toString: String =
    explicit.mkString(s"$productPrefix(", ",", ")")

  final def toStringOrdered(using Ordering[A]): String =
    explicit.toSeq.sorted.mkString(s"$productPrefix(", ",", ")")

}
object InfiniteSet {

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Builders
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  def empty[T]: InfiniteSet[T] = InfiniteSet.Inclusive()
  def full[T]: InfiniteSet[T] = InfiniteSet.Exclusive()

  inline def nothing[T]: InfiniteSet[T] = empty[T]
  inline def everything[T]: InfiniteSet[T] = full[T]

  def unionAll[A](sets: InfiniteSet[A]*): InfiniteSet[A] =
    sets.foldLeft(InfiniteSet.empty[A])(_ | _)

  def intersectAll[A](sets: InfiniteSet[A]*): InfiniteSet[A] =
    sets.foldLeft(InfiniteSet.full[A])(_ & _)

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Inclusive
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Inclusive[A](explicit: Set[A]) extends InfiniteSet[A]
  object Inclusive {
    def apply[A](explicit: A*): InfiniteSet.Inclusive[A] = InfiniteSet.Inclusive(explicit.toSet)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////
  //      Exclusive
  //////////////////////////////////////////////////////////////////////////////////////////////////////

  final case class Exclusive[A](explicit: Set[A]) extends InfiniteSet[A]
  object Exclusive {
    def apply[A](explicit: A*): InfiniteSet.Exclusive[A] = InfiniteSet.Exclusive(explicit.toSet)
  }

}
