package oxygen.core.typeclass

import oxygen.core.collection.NonEmptyList
import oxygen.core.syntax.option.*

trait Pure[F[_]] {

  def pure[A](self: A): F[A]

}
object Pure extends PureLowPriority.LowPriority1 {

  inline def apply[F[_]](implicit ev: Pure[F]): ev.type = ev

  implicit val option: Pure[Option] =
    new Pure[Option] {
      override def pure[A](self: A): Option[A] = self.some
    }

  implicit val list: Pure[List] =
    new Pure[List] {
      override def pure[A](self: A): List[A] = self :: Nil
    }

  implicit val nonEmptyList: Pure[NonEmptyList] =
    new Pure[NonEmptyList] {
      override def pure[A](self: A): NonEmptyList[A] = NonEmptyList(self, Nil)
    }

  implicit val seq: Pure[Seq] =
    new Pure[Seq] {
      override def pure[A](self: A): Seq[A] = Seq(self)
    }

}

object PureLowPriority {

  trait LowPriority1 {

    given fromApplicative: [F[_]] => (applicative: Applicative[F]) => Pure[F] = applicative

  }

}
