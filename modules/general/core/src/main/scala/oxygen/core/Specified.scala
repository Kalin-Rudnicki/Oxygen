package oxygen.core

/**
  * Representation of an Option with a specialized purpose of whether a value is specified or not.
  *
  * This has 2 main benefits:
  * 1) You can define a function like `def doStuff(a: Specified[Int] = Specified.WasNotSpecified): Unit = ???`
  *    and then call it like: `doStuff(5)` because of the given Conversion
  * 2) You have the ability to represent both `Specified[Int]` and `Specified[Option[Int]]`.
  *    This is very useful when it comes to things like partial updates.
  *    With only the ability to represent `Option[Int]`, you are often left stuck with being able to update to `Some(_)`, and None means `no update`.
  * 3) You can tell the difference between:
  *    {}                // Specified.WasNotSpecified
  *    { "field": null } // Specified.WasSpecified(None)
  *    { "field": 5 }    // Specified.WasSpecified(Some(5))
  */
sealed trait Specified[+A] {

  final def toOption: Option[A] = this match
    case Specified.WasSpecified(value) => Some(value)
    case Specified.WasNotSpecified     => None

  final def isSpecified: Boolean = this match
    case Specified.WasSpecified(_) => true
    case Specified.WasNotSpecified => false
  final def isNotSpecified: Boolean =
    !this.isSpecified

  final def getOrElse[A2 >: A](default: => A2): A2 = this match
    case Specified.WasSpecified(value) => value
    case Specified.WasNotSpecified     => default
  final def orElse[A2 >: A](alternative: => Specified[A2]): Specified[A2] = this match
    case specified @ Specified.WasSpecified(_) => specified
    case Specified.WasNotSpecified             => alternative

  final def map[B](f: A => B): Specified[B] = this match
    case Specified.WasSpecified(value) => Specified.WasSpecified(f(value))
    case Specified.WasNotSpecified     => Specified.WasNotSpecified
  final def flatMap[B](f: A => Specified[B]): Specified[B] = this match
    case Specified.WasSpecified(value) => f(value)
    case Specified.WasNotSpecified     => Specified.WasNotSpecified
  final def flatMapOption[B](f: A => Option[B]): Specified[B] =
    this.flatMap(v => Specified.fromOption(f(v)))

  final def fold[B](notSpec: => B)(spec: A => B): B = this match
    case Specified.WasSpecified(value) => spec(value)
    case Specified.WasNotSpecified     => notSpec

  final def filter(f: A => Boolean): Specified[A] = this match
    case specified @ Specified.WasSpecified(value) if f(value) => specified
    case _                                                     => Specified.WasNotSpecified
  final def filterNot(f: A => Boolean): Specified[A] =
    this.filter(!f(_))

  override final def toString: String = this match
    case Specified.WasSpecified(value) => value.toString
    case Specified.WasNotSpecified     => "<<unspecified>>"

}
object Specified {

  final case class WasSpecified[+A](value: A) extends Specified[A]
  case object WasNotSpecified extends Specified[Nothing]

  def apply[A](value: A): Specified[A] =
    if value == null then Specified.WasNotSpecified
    else Specified.WasSpecified(value)

  def when[A](cond: Boolean)(value: => A): Specified[A] = if cond then Specified.WasSpecified(value) else Specified.WasNotSpecified

  def fromOption[A](value: Option[A]): Specified[A] = value match
    case Some(value) => Specified.WasSpecified(value)
    case None        => Specified.WasNotSpecified

  given idToSpecified: [A] => Conversion[A, Specified.WasSpecified[A]] = Specified.WasSpecified(_)
  given aToBSpecified: [A, B] => (conv: Conversion[A, B]) => Conversion[A, Specified.WasSpecified[B]] = a => Specified.WasSpecified(conv(a))

}

def unspecified: Specified.WasNotSpecified.type = Specified.WasNotSpecified
def ___ : Specified.WasNotSpecified.type = Specified.WasNotSpecified
