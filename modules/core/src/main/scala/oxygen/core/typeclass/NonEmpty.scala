package oxygen.core.typeclass

import oxygen.core.NonEmptyList
import oxygen.core.syntax.option.*

trait NonEmpty[F[_]] {

  type G[_]

  def nonEmpty[A](self: F[A]): Option[G[A]]

  final def unsafeNonEmpty[A](self: F[A]): G[A] = nonEmpty(self).getOrElse(throw NonEmpty.WasNotNonEmpty(self))

}
object NonEmpty {

  type Aux[_F[_], _G[_]] = NonEmpty[_F] { type G[A] = _G[A] }

  final case class WasNotNonEmpty(seq: Any) extends Throwable {
    override def getMessage: String = s"Seq was not non-empty: $seq"
  }

  implicit val nel: NonEmpty.Aux[List, NonEmptyList] =
    new NonEmpty[List] {

      override type G[A] = NonEmptyList[A]

      override def nonEmpty[A](self: List[A]): Option[NonEmptyList[A]] = self match
        case head :: tail => NonEmptyList(head, tail).some
        case Nil          => None

    }

}
