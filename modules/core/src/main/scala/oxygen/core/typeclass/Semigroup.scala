package oxygen.core.typeclass

import oxygen.core.NonEmptyList
import scala.collection.IterableOps

trait Semigroup[A] {

  def combine(x: A, y: A): A

}
object Semigroup extends SemigroupLowPriority.LowPriority1 {

  inline def apply[A](implicit ev: Semigroup[A]): ev.type = ev

  implicit def nel[A]: Semigroup[NonEmptyList[A]] = _ ++ _

  implicit def numeric[A](implicit numeric: Numeric[A]): Semigroup[A] = numeric.plus(_, _)

}

object SemigroupLowPriority {

  trait LowPriority1 {

    implicit def fromIterableOps[F[_A] <: IterableOps[_A, F, F[_A]], A]: Semigroup[F[A]] = _ ++ _

  }

  Option
  
}
