package oxygen.core.typeclass

import oxygen.core.collection.NonEmptyList

trait EnumEncoding[A] {
  def encodeMany(value: A): NonEmptyList[String]
}
object EnumEncoding {

  private final case class Many[A](f: A => NonEmptyList[String]) extends EnumEncoding[A] {
    override def encodeMany(value: A): NonEmptyList[String] = f(value)
  }

  private final case class Single[A](f: A => String) extends EnumEncoding[A] {
    override def encodeMany(value: A): NonEmptyList[String] = NonEmptyList.one(f(value))
  }

  private final class ToString[A] extends EnumEncoding[A] {
    override def encodeMany(value: A): NonEmptyList[String] = NonEmptyList.one(value.toString)
  }

  def many[A](f: A => NonEmptyList[String]): EnumEncoding[A] = EnumEncoding.Many(f)
  def single[A](f: A => String): EnumEncoding[A] = EnumEncoding.Single(f)
  def fromToString[A]: EnumEncoding[A] = new EnumEncoding.ToString[A]

  given default: [A] => EnumEncoding[A] = EnumEncoding.fromToString[A]

}
