package oxygen.core

import scala.reflect.TypeTest

/**
  * Docs from [[Ordering]]:
  * def compare(x: T, y: T): Int
  * The result sign has the following meaning:
  *
  *  - negative if x < y
  *  - positive if x > y
  *  - zero otherwise (if x == y)
  */
opaque type ComparisonResult <: Int = Int
object ComparisonResult {

  inline def apply(raw: Int): ComparisonResult = raw

  val LessThan: ComparisonResult = -1
  val GreaterThan: ComparisonResult = 1
  val EqualTo: ComparisonResult = 0

  extension (self: ComparisonResult)
    def ||(that: => ComparisonResult): ComparisonResult =
      if self == EqualTo then that
      else self

  def makeOrdering[A](f: (A, A) => ComparisonResult): Ordering[A] =
    new Ordering[A] {
      override def compare(x: A, y: A): ComparisonResult = f(x, y)
    }

  def refinedOrElseBy[A, B <: A, C](by: A => C)(using tt: TypeTest[A, B], bOrd: Ordering[B], cOrd: Ordering[C]): Ordering[A] = {
    val rawOrdering: Ordering[A] =
      Ordering.by(by)
    makeOrdering[A] {
      case (x: B, y: B) => bOrd.compare(x, y)
      case (x, y)       => rawOrdering.compare(x, y)
    }
  }

}
