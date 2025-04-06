package oxygen.schema

import oxygen.predef.core.*

sealed trait Specified[+A] {

  final def toOption: Option[A] = this match
    case Specified.WasSpecified(value) => value.some
    case Specified.WasNotSpecified     => None

  override final def toString: String = this match
    case Specified.WasSpecified(value) => value.toString
    case Specified.WasNotSpecified     => "<<unspecified>>"

}
object Specified {

  final case class WasSpecified[+A](value: A) extends Specified[A]
  case object WasNotSpecified extends Specified[Nothing]

  def fromOption[A](value: Option[A]): Specified[A] = value match
    case Some(value) => Specified.WasSpecified(value)
    case None        => Specified.WasNotSpecified

}

given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)
