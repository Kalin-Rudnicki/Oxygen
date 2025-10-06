package oxygen.core

sealed trait Specified[+A] {

  final def toOption: Option[A] = this match
    case Specified.WasSpecified(value) => Some(value)
    case Specified.WasNotSpecified     => None

  final def isSpecified: Boolean = this match
    case Specified.WasSpecified(_) => true
    case Specified.WasNotSpecified => false

  final def getOrElse[A2 >: A](default: => A2): A2 = this match
    case Specified.WasSpecified(value) => value
    case Specified.WasNotSpecified     => default

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

  given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)
  given aToBSpecified: [A, B] => (conv: Conversion[A, B]) => Conversion[A, Specified.WasSpecified[B]] = a => Specified.WasSpecified(conv(a))

}
