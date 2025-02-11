package oxygen.cli

import oxygen.predef.core.*

object Defaultable {

  sealed trait Optional[+V] {

    final def map[V2](f: V => V2): Optional[V2] = this match
      case Defaultable.Some(value) => Defaultable.Some(f(value))
      case Defaultable.Auto        => Defaultable.Auto
      case Defaultable.None        => Defaultable.None

    // TODO (KR) : remove?
    final def toOption: Option[V] = this match
      case Some(value) => scala.Some(value)
      case Auto        => scala.None
      case None        => scala.None

  }
  sealed trait NonOptional[+V] extends Optional[V]

  case object Auto extends NonOptional[Nothing]
  final case class Some[+V](value: V) extends NonOptional[V]
  case object None extends Optional[Nothing]

}

given [A] => Conversion[A, Defaultable.NonOptional[A]] = Defaultable.Some(_)
given [A] => Conversion[Option[A], Defaultable.Optional[A]] = {
  case Some(value) => Defaultable.Some(value)
  case None        => Defaultable.None
}
