package oxygen.core

opaque type Nullable[+A] <: Option[A] = Option[A]
object Nullable {

  def wrap[A](value: Option[A]): Nullable[A] = value
  def some[A](value: A): Nullable[A] = Some(value)
  def none: Nullable[Nothing] = None

  extension [A](self: Nullable[A]) def unwrap: Option[A] = self

  given convertOption: [A] => Conversion[Option[A], Nullable[A]] = ConversionUtils.id
  given convertSome: [A] => Conversion[A, Nullable[A]] = Nullable.some(_)

}
