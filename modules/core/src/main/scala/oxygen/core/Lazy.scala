package oxygen.core

final class Lazy[A] private (_value: => A) {
  lazy val value: A = _value
  override def toString: String = value.toString
}
object Lazy {

  def apply[A](value: => A): Lazy[A] = new Lazy[A](value)

}
