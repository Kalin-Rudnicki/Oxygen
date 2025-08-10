package oxygen.core

trait Unapply[A, B] {
  def unapply(a: A): Option[B]
}
object Unapply {
  def apply[A, B](f: PartialFunction[A, Option[B]]): Unapply[A, B] = f.lift(_).flatten
  def fromPartialFunction[A, B](pf: PartialFunction[A, B]): Unapply[A, B] = a => pf.lift(a)
}
