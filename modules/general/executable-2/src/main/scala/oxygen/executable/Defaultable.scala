package oxygen.executable

sealed trait Defaultable[+A]
object Defaultable {

  type Opt[+A] = Defaultable[Option[A]]

  final case class Explicit[+A](value: A) extends Defaultable[A]
  case object Default extends Defaultable[Nothing]

}
