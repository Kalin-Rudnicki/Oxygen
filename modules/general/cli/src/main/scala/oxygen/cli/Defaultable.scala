package oxygen.cli

import oxygen.predef.core.*

sealed trait Defaultable[+A]
object Defaultable {

  type Opt[+A] = Defaultable[Option[A]]

  extension [A](self: Defaultable[A])
    def toOption: Option[A] = self match
      case Explicit(value) => value.some
      case Default         => None

  extension [A](self: Defaultable.Opt[A])
    def toFlatOption: Option[A] = self match
      case Explicit(value) => value
      case Default         => None

  final case class Explicit[+A](value: A) extends Defaultable[A]
  case object Default extends Defaultable[Nothing]

}
