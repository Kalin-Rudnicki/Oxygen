package oxygen.core.typeclass

import oxygen.core.collection.NonEmptyList
import scala.collection.IterableOps

trait Semigroup[A] {

  def combine(x: A, y: A): A

}
object Semigroup extends SemigroupLowPriority.LowPriority1 {

  inline def apply[A](using ev: Semigroup[A]): ev.type = ev

  given nel: [A] => Semigroup[NonEmptyList[A]] = _ ++ _

  given numeric: [A] => (numeric: Numeric[A]) => Semigroup[A] = numeric.plus(_, _)

}

object SemigroupLowPriority {

  trait LowPriority1 {

    given fromIterableOps: [F[_A] <: IterableOps[_A, F, F[_A]], A] => Semigroup[F[A]] = _ ++ _

  }

}
